BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.2.0', 3, 2, 0))
  AS "subquery"("string", "major", "minor", "revision");

-- TODO: preliminary script

ALTER TABLE "member" ADD COLUMN "disable_notifications" BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE "member" ADD COLUMN "notification_counter" INT4 NOT NULL DEFAULT 1;
ALTER TABLE "member" ADD COLUMN "notification_sample_size" INT4 NOT NULL DEFAULT 3;
ALTER TABLE "member" ADD COLUMN "notification_dow" INT4 CHECK ("notification_dow" BETWEEN 0 AND 6);
ALTER TABLE "member" ADD COLUMN "notification_hour" INT4 CHECK ("notification_hour" BETWEEN 0 AND 23);

UPDATE "member" SET "disable_notifications" = TRUE WHERE "notify_level" = 'none';

DROP VIEW "selected_event_seen_by_member";
DROP VIEW "event_seen_by_member";
ALTER TABLE "member" DROP COLUMN "notify_level";
DROP TYPE "notify_level";
 
CREATE TABLE "subscription" (
        PRIMARY KEY ("member_id", "unit_id"),
        "member_id"             INT4            REFERENCES "member" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "unit_id"               INT4            REFERENCES "unit" ("id") ON DELETE CASCADE ON UPDATE CASCADE );
CREATE INDEX "subscription_unit_id_idx" ON "subscription" ("unit_id");

COMMENT ON TABLE "subscription" IS 'An entry in this table denotes that the member wishes to receive notifications regardless of his/her privileges in the given unit';

CREATE TABLE "ignored_area" (
        PRIMARY KEY ("member_id", "area_id"),
        "member_id"             INT4            REFERENCES "unit" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "area_id"               INT4            REFERENCES "area" ("id") ON DELETE CASCADE ON UPDATE CASCADE );
CREATE INDEX "ignored_area_area_id_idx" ON "ignored_area" ("area_id");

COMMENT ON TABLE "ignored_area" IS 'An entry in this table denotes that the member does not wish to receive notifications for the given subject area unless he/she declared interested in a particular issue';

CREATE TABLE "initiative_notification_sent" (
        PRIMARY KEY ("member_id", "initiative_id"),
        "member_id"             INT4            REFERENCES "member" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "initiative_id"         INT4            REFERENCES "initiative" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "last_draft_id"         INT8            NOT NULL,
        "last_suggestion_id"    INT8 );
CREATE INDEX "initiative_notification_sent_initiative_idx" ON "initiative_notification_sent" ("initiative_id");

CREATE TABLE "newsletter" (
        "id"                    SERIAL4         PRIMARY KEY,
        "published"             TIMESTAMPTZ     NOT NULL,
        "unit_id"               INT4            REFERENCES "unit" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "include_all_members"   BOOLEAN         NOT NULL,
        "sent"                  TIMESTAMPTZ,
        "subject"               TEXT            NOT NULL,
        "content"               TEXT            NOT NULL );
CREATE INDEX "newsletter_unit_id_idx" ON "newsletter" ("unit_id", "published");
CREATE INDEX "newsletter_all_units_published_idx" ON "newsletter" ("published") WHERE "unit_id" ISNULL;
CREATE INDEX "newsletter_published_idx" ON "newsletter" ("published");

CREATE VIEW "event_for_notification" AS
  SELECT
    "member"."id" AS "recipient_id",
    "event".*
  FROM "member" CROSS JOIN "event"
  JOIN "issue" ON "issue"."id" = "event"."issue_id"
  JOIN "area" ON "area"."id" = "issue"."area_id"
  LEFT JOIN "privilege" ON
    "privilege"."member_id" = "member"."id" AND
    "privilege"."unit_id" = "area"."unit_id" AND
    "privilege"."voting_right" = TRUE
  LEFT JOIN "subscription" ON
    "subscription"."member_id" = "member"."id" AND
    "subscription"."unit_id" = "area"."unit_id"
  LEFT JOIN "ignored_area" ON
    "ignored_area"."member_id" = "member"."id" AND
    "ignored_area"."area_id" = "issue"."area_id"
  LEFT JOIN "interest" ON
    "interest"."member_id" = "member"."id" AND
    "interest"."issue_id" = "event"."issue_id"
  LEFT JOIN "supporter" ON
    "supporter"."member_id" = "member"."id" AND
    "supporter"."initiative_id" = "event"."initiative_id"
  WHERE ("privilege"."member_id" NOTNULL OR "subscription"."member_id" NOTNULL)
  AND ("ignored_area"."member_id" ISNULL OR "interest"."member_id" NOTNULL)
  AND (
    "event"."event" = 'issue_state_changed'::"event_type" OR
    ( "event"."event" = 'initiative_revoked'::"event_type" AND
      "supporter"."member_id" NOTNULL ) );

CREATE VIEW "updated_initiative" AS
  SELECT
    "supporter"."member_id" AS "recipient_id",
    FALSE AS "featured",
    "supporter"."initiative_id"
  FROM "supporter"
  JOIN "initiative" ON "supporter"."initiative_id" = "initiative"."id"
  JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
  LEFT JOIN "initiative_notification_sent" AS "sent" ON
    "sent"."member_id" = "supporter"."member_id" AND
    "sent"."initiative_id" = "supporter"."initiative_id"
  LEFT JOIN "ignored_initiative" ON
    "ignored_initiative"."member_id" = "supporter"."member_id" AND
    "ignored_initiative"."initiative_id" = "supporter"."initiative_id"
  WHERE "issue"."state" IN ('admission', 'discussion')
  AND "ignored_initiative"."member_id" ISNULL
  AND (
    EXISTS (
      SELECT NULL FROM "draft"
      LEFT JOIN "ignored_member" ON
        "ignored_member"."member_id" = "supporter"."member_id" AND
        "ignored_member"."other_member_id" = "draft"."author_id"
      WHERE "draft"."initiative_id" = "supporter"."initiative_id"
      AND "draft"."id" > "supporter"."draft_id"
      AND "ignored_member"."member_id" ISNULL
    ) OR EXISTS (
      SELECT NULL FROM "suggestion"
      LEFT JOIN "opinion" ON
        "opinion"."member_id" = "supporter"."member_id" AND
        "opinion"."suggestion_id" = "suggestion"."id"
      LEFT JOIN "ignored_member" ON
        "ignored_member"."member_id" = "supporter"."member_id" AND
        "ignored_member"."other_member_id" = "suggestion"."author_id"
      WHERE "suggestion"."initiative_id" = "supporter"."initiative_id"
      AND "opinion"."member_id" ISNULL
      AND COALESCE("suggestion"."id" > "sent"."last_suggestion_id", TRUE)
      AND "ignored_member"."member_id" ISNULL
    )
  );

CREATE FUNCTION "featured_initiative"
  ( "recipient_id_p" "member"."id"%TYPE,
    "area_id_p"      "area"."id"%TYPE )
  RETURNS SETOF "initiative"."id"%TYPE
  LANGUAGE 'plpgsql' STABLE AS $$
    DECLARE
      "counter_v"         "member"."notification_counter"%TYPE;
      "sample_size_v"     "member"."notification_sample_size"%TYPE;
      "initiative_id_ary" INT4[];  --"initiative"."id"%TYPE[]
      "match_v"           BOOLEAN;
      "member_id_v"       "member"."id"%TYPE;
      "seed_v"            TEXT;
      "initiative_id_v"   "initiative"."id"%TYPE;
    BEGIN
      SELECT "notification_counter", "notification_sample_size"
        INTO "counter_v", "sample_size_v"
        FROM "member" WHERE "id" = "recipient_id_p";
      "initiative_id_ary" := '{}';
      LOOP
        "match_v" := FALSE;
        FOR "member_id_v", "seed_v" IN
          SELECT * FROM (
            SELECT DISTINCT
              "supporter"."member_id",
              md5(
                "recipient_id_p" || '-' ||
                "counter_v"      || '-' ||
                "area_id_p"      || '-' ||
                "supporter"."member_id"
              ) AS "seed"
            FROM "supporter"
            JOIN "initiative" ON "initiative"."id" = "supporter"."initiative_id"
            JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
            WHERE "supporter"."member_id" != "recipient_id_p"
            AND "issue"."area_id" = "area_id_p"
            AND "issue"."state" IN ('admission', 'discussion', 'verification')
          ) AS "subquery"
          ORDER BY "seed"
        LOOP
          SELECT "initiative"."id" INTO "initiative_id_v"
            FROM "initiative"
            JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
            JOIN "area" ON "area"."id" = "issue"."area_id"
            JOIN "supporter" ON "supporter"."initiative_id" = "initiative"."id"
            LEFT JOIN "supporter" AS "self_support" ON
              "self_support"."initiative_id" = "initiative"."id" AND
              "self_support"."member_id" = "recipient_id_p"
            LEFT JOIN "privilege" ON
              "privilege"."member_id" = "recipient_id_p" AND
              "privilege"."unit_id" = "area"."unit_id" AND
              "privilege"."voting_right" = TRUE
            LEFT JOIN "subscription" ON
              "subscription"."member_id" = "recipient_id_p" AND
              "subscription"."unit_id" = "area"."unit_id"
            LEFT JOIN "ignored_initiative" ON
              "ignored_initiative"."member_id" = "recipient_id_p" AND
              "ignored_initiative"."initiative_id" = "initiative"."id"
            WHERE "supporter"."member_id" = "member_id_v"
            AND "issue"."area_id" = "area_id_p"
            AND "issue"."state" IN ('admission', 'discussion', 'verification')
            AND "self_support"."member_id" ISNULL
            AND NOT "initiative_id_ary" @> ARRAY["initiative"."id"]
            AND (
              "privilege"."member_id" NOTNULL OR
              "subscription"."member_id" NOTNULL )
            AND "ignored_initiative"."member_id" ISNULL
            AND NOT EXISTS (
              SELECT NULL FROM "draft"
              JOIN "ignored_member" ON
                "ignored_member"."member_id" = "recipient_id_p" AND
                "ignored_member"."other_member_id" = "draft"."author_id"
              WHERE "draft"."initiative_id" = "initiative"."id"
            )
            ORDER BY md5("seed_v" || '-' || "initiative"."id")
            LIMIT 1;
          IF FOUND THEN
            "match_v" := TRUE;
            RETURN NEXT "initiative_id_v";
            IF array_length("initiative_id_ary", 1) + 1 >= "sample_size_v" THEN
              RETURN;
            END IF;
            "initiative_id_ary" := "initiative_id_ary" || "initiative_id_v";
          END IF;
        END LOOP;
        EXIT WHEN NOT "match_v";
      END LOOP;
      RETURN;
    END;
  $$;

CREATE VIEW "updated_or_featured_initiative" AS
  SELECT
    "subquery".*,
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE "better_initiative"."issue_id" = "initiative"."issue_id"
      AND
        ( COALESCE("better_initiative"."harmonic_weight", -1),
          -"better_initiative"."id" ) >
        ( COALESCE("initiative"."harmonic_weight", -1),
          -"initiative"."id" )
    ) AS "leading"
  FROM (
    SELECT * FROM "updated_initiative"
    UNION ALL
    SELECT
      "member"."id" AS "recipient_id",
      TRUE AS "featured",
      "featured_initiative_id" AS "initiative_id"
    FROM "member" CROSS JOIN "area"
    CROSS JOIN LATERAL
      "featured_initiative"("member"."id", "area"."id") AS "featured_initiative_id"
    JOIN "initiative" ON "initiative"."id" = "featured_initiative_id"
  ) AS "subquery"
  JOIN "initiative" ON "initiative"."id" = "subquery"."initiative_id";

CREATE VIEW "leading_complement_initiative" AS
  SELECT * FROM (
    SELECT DISTINCT ON ("uf_initiative"."recipient_id", "initiative"."issue_id")
      "uf_initiative"."recipient_id",
      FALSE AS "featured",
      "uf_initiative"."initiative_id",
      TRUE AS "leading"
    FROM "updated_or_featured_initiative" AS "uf_initiative"
    JOIN "initiative" AS "uf_initiative_full" ON
      "uf_initiative_full"."id" = "uf_initiative"."initiative_id"
    JOIN "initiative" ON
      "initiative"."issue_id" = "uf_initiative_full"."issue_id"
    ORDER BY
      "uf_initiative"."recipient_id",
      "initiative"."issue_id",
      "initiative"."harmonic_weight" DESC,
      "initiative"."id"
  ) AS "subquery"
  WHERE NOT EXISTS (
    SELECT NULL FROM "updated_or_featured_initiative" AS "other"
    WHERE "other"."recipient_id" = "subquery"."recipient_id"
    AND "other"."initiative_id" = "subquery"."initiative_id"
  );

CREATE VIEW "unfiltered_initiative_for_notification" AS
  SELECT
    "subquery".*,
    "supporter"."member_id" NOTNULL AS "supported",
    CASE WHEN "supporter"."member_id" NOTNULL THEN
      EXISTS (
        SELECT NULL FROM "draft"
        WHERE "draft"."initiative_id" = "subquery"."initiative_id"
        AND "draft"."id" > "supporter"."draft_id"
      )
    ELSE
      EXISTS (
        SELECT NULL FROM "draft"
        WHERE "draft"."initiative_id" = "subquery"."initiative_id"
        AND COALESCE("draft"."id" > "sent"."last_draft_id", TRUE)
      )
    END AS "new_draft",
    CASE WHEN "supporter"."member_id" NOTNULL THEN
      ( SELECT count(1) FROM "suggestion"
        LEFT JOIN "opinion" ON
          "opinion"."member_id" = "supporter"."member_id" AND
          "opinion"."suggestion_id" = "suggestion"."id"
        WHERE "suggestion"."initiative_id" = "subquery"."initiative_id"
        AND "opinion"."member_id" ISNULL
        AND COALESCE("suggestion"."id" > "sent"."last_suggestion_id", TRUE)
      )
    ELSE
      ( SELECT count(1) FROM "suggestion"
        WHERE "suggestion"."initiative_id" = "subquery"."initiative_id"
        AND COALESCE("suggestion"."id" > "sent"."last_suggestion_id", TRUE)
      )
    END AS "new_suggestion_count"
  FROM (
    SELECT * FROM "updated_or_featured_initiative"
    UNION ALL
    SELECT * FROM "leading_complement_initiative"
  ) AS "subquery"
  LEFT JOIN "supporter" ON
    "supporter"."member_id" = "subquery"."recipient_id" AND
    "supporter"."initiative_id" = "subquery"."initiative_id"
  LEFT JOIN "initiative_notification_sent" AS "sent" ON
    "sent"."member_id" = "subquery"."recipient_id" AND
    "sent"."initiative_id" = "subquery"."initiative_id";

CREATE VIEW "initiative_for_notification" AS
  SELECT "unfiltered1".*
  FROM "unfiltered_initiative_for_notification" "unfiltered1"
  JOIN "initiative" AS "initiative1" ON
    "initiative1"."id" = "unfiltered1"."initiative_id"
  JOIN "issue" AS "issue1" ON "issue1"."id" = "initiative1"."issue_id"
  WHERE EXISTS (
    SELECT NULL
    FROM "unfiltered_initiative_for_notification" "unfiltered2"
    JOIN "initiative" AS "initiative2" ON
      "initiative2"."id" = "unfiltered2"."initiative_id"
    JOIN "issue" AS "issue2" ON "issue2"."id" = "initiative2"."issue_id"
    WHERE "unfiltered1"."recipient_id" = "unfiltered2"."recipient_id"
    AND "issue1"."area_id" = "issue2"."area_id"
    AND ("unfiltered2"."new_draft" OR "unfiltered2"."new_suggestion_count" > 0 )
  );

CREATE VIEW "newsletter_to_send" AS
  SELECT
    "member"."id" AS "recipient_id",
    "newsletter"."id" AS "newsletter_id"
  FROM "newsletter" CROSS JOIN "member"
  LEFT JOIN "privilege" ON
    "privilege"."member_id" = "member"."id" AND
    "privilege"."unit_id" = "newsletter"."unit_id" AND
    "privilege"."voting_right" = TRUE
  LEFT JOIN "subscription" ON
    "subscription"."member_id" = "member"."id" AND
    "subscription"."unit_id" = "newsletter"."unit_id"
  WHERE "newsletter"."published" <= now()
  AND "newsletter"."sent" ISNULL
  AND "member"."locked" = FALSE
  AND (
    "member"."disable_notifications" = FALSE OR
    "newsletter"."include_all_members" = TRUE )
  AND (
    "newsletter"."unit_id" ISNULL OR
    "privilege"."member_id" NOTNULL OR
    "subscription"."member_id" NOTNULL );

CREATE FUNCTION "get_initiatives_for_notification"
  ( "recipient_id_p" "member"."id"%TYPE )
  RETURNS SETOF "initiative_for_notification"
  LANGUAGE 'plpgsql' VOLATILE AS $$
    DECLARE
      "result_row"           "initiative_for_notification"%ROWTYPE;
      "last_draft_id_v"      "draft"."id"%TYPE;
      "last_suggestion_id_v" "suggestion"."id"%TYPE;
    BEGIN
      PERFORM "require_transaction_isolation"();
      PERFORM NULL FROM "member" WHERE "id" = "recipient_id_p" FOR UPDATE;
      FOR "result_row" IN
        SELECT * FROM "initiative_for_notification"
        WHERE "recipient_id" = "recipient_id_p"
      LOOP
        SELECT "id" INTO "last_draft_id_v" FROM "draft"
          WHERE "draft"."initiative_id" = "result_row"."initiative_id"
          ORDER BY "id" DESC LIMIT 1;
        SELECT "id" INTO "last_suggestion_id_v" FROM "suggestion"
          WHERE "suggestion"."initiative_id" = "result_row"."initiative_id"
          ORDER BY "id" DESC LIMIT 1;
        INSERT INTO "initiative_notification_sent"
          ("member_id", "initiative_id", "last_draft_id", "last_suggestion_id")
          VALUES (
            "recipient_id_p",
            "result_row"."initiative_id",
            "last_draft_id_v",
            "last_suggestion_id_v" )
          ON CONFLICT ("member_id", "initiative_id") DO UPDATE SET
            "last_draft_id" = CASE
              WHEN "initiative_notification_sent"."last_draft_id" > "last_draft_id_v"
              THEN "initiative_notification_sent"."last_draft_id"
              ELSE "last_draft_id_v"
            END,
            "last_suggestion_id" = CASE
              WHEN "initiative_notification_sent"."last_suggestion_id" > "last_suggestion_id_v"
              THEN "initiative_notification_sent"."last_suggestion_id"
              ELSE "last_suggestion_id_v"
            END;
        RETURN NEXT "result_row";
      END LOOP;
      DELETE FROM "initiative_notification_sent"
        USING "initiative", "issue"
        WHERE "initiative_notification_sent"."member_id" = "recipient_id_p"
        AND "initiative"."id" = "initiative_notification_sent"."initiative_id"
        AND "issue"."id" = "initiative"."issue_id"
        AND ( "issue"."closed" NOTNULL OR "issue"."fully_frozen" NOTNULL );
      UPDATE "member" SET "notification_counter" = "notification_counter" + 1
        WHERE "id" = "recipient_id_p";
      RETURN;
    END;
  $$;

COMMIT;
