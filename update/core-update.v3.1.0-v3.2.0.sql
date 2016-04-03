BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.2.0', 3, 2, 0))
  AS "subquery"("string", "major", "minor", "revision");

ALTER TABLE "member" ADD COLUMN "disable_notifications" BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE "member" ADD COLUMN "notification_counter" INT4 NOT NULL DEFAULT 1;
ALTER TABLE "member" ADD COLUMN "notification_sample_size" INT4 NOT NULL DEFAULT 3;
ALTER TABLE "member" ADD COLUMN "notification_dow" INT4 CHECK ("notification_dow" BETWEEN 0 AND 6);
ALTER TABLE "member" ADD COLUMN "notification_hour" INT4 CHECK ("notification_hour" BETWEEN 0 AND 23);

UPDATE "member" SET "disable_notifications" = TRUE WHERE "notify_level" = 'none';
 
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

CREATE VIEW "updated_initiative" AS
  SELECT
    "supporter"."member_id" AS "seen_by_member_id",
    TRUE AS "supported",
    EXISTS (
      SELECT NULL FROM "draft"
      WHERE "draft"."initiative_id" = "initiative"."id"
      AND "draft"."id" > "supporter"."draft_id"
    ) AS "new_draft",
    ( SELECT count(1) FROM "suggestion"
      LEFT JOIN "opinion" ON
        "opinion"."member_id" = "supporter"."member_id" AND
        "opinion"."suggestion_id" = "suggestion"."id"
      WHERE "suggestion"."initiative_id" = "initiative"."id"
      AND "opinion"."member_id" ISNULL
      AND COALESCE(
        "suggestion"."id" > "sent"."last_suggestion_id",
        TRUE
      )
    ) AS "new_suggestion_count",
    FALSE AS "featured",
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE
        "better_initiative"."issue_id" = "initiative"."issue_id"
      AND
        ( COALESCE("better_initiative"."harmonic_weight", -1),
          -"better_initiative"."id" ) >
        ( COALESCE("initiative"."harmonic_weight", -1),
          -"initiative"."id" )
    ) AS "leading",
    "initiative".*
  FROM "supporter" JOIN "initiative"
  ON "supporter"."initiative_id" = "initiative"."id"
  LEFT JOIN "initiative_notification_sent" AS "sent"
    ON "sent"."member_id" = "supporter"."member_id"
    AND "sent"."initiative_id" = "initiative"."id"
  JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
  WHERE "issue"."state" IN ('admission', 'discussion')
  AND (
    EXISTS (
      SELECT NULL FROM "draft"
      WHERE "draft"."initiative_id" = "initiative"."id"
      AND "draft"."id" > "supporter"."draft_id"
    ) OR EXISTS (
      SELECT NULL FROM "suggestion"
      LEFT JOIN "opinion" ON
        "opinion"."member_id" = "supporter"."member_id" AND
        "opinion"."suggestion_id" = "suggestion"."id"
      WHERE "suggestion"."initiative_id" = "initiative"."id"
      AND "opinion"."member_id" ISNULL
      AND COALESCE(
        "suggestion"."id" > "sent"."last_suggestion_id",
        TRUE
      )
    )
  );

CREATE FUNCTION "featured_initiative"
  ( "member_id_p" "member"."id"%TYPE,
    "area_id_p"   "area"."id"%TYPE )
  RETURNS SETOF "initiative"
  LANGUAGE 'plpgsql' STABLE AS $$
    DECLARE
      "member_row"        "member"%ROWTYPE;
      "member_id_v"       "member"."id"%TYPE;
      "seed_v"            TEXT;
      "result_row"        "initiative"%ROWTYPE;
      "match_v"           BOOLEAN;
      "initiative_id_ary" INT4[];  --"initiative"."id"%TYPE[]
    BEGIN
      SELECT INTO "member_row" * FROM "member" WHERE "id" = "member_id_p";
      "initiative_id_ary" := '{}';
      LOOP
        "match_v" := FALSE;
        FOR "member_id_v", "seed_v" IN
          SELECT * FROM (
            SELECT DISTINCT
              "supporter"."member_id",
              md5("member_id_p" || '-' || "member_row"."notification_counter" || '-' || "area_id_p" || '-' || "supporter"."member_id") AS "seed"
            FROM "supporter"
            JOIN "member" ON "member"."id" = "supporter"."member_id"
            JOIN "initiative" ON "initiative"."id" = "supporter"."initiative_id"
            JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
            WHERE "supporter"."member_id" != "member_id_p"
            AND "issue"."area_id" = "area_id_p"
            AND "issue"."state" IN ('admission', 'discussion', 'verification')
          ) AS "subquery"
          ORDER BY "seed"
        LOOP
          SELECT "initiative".* INTO "result_row"
            FROM "initiative"
            JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
            JOIN "supporter" ON "supporter"."initiative_id" = "initiative"."id"
            LEFT JOIN "supporter" AS "self_support" ON
              "self_support"."initiative_id" = "initiative"."id" AND
              "self_support"."member_id" = "member_id_p"
            WHERE "supporter"."member_id" = "member_id_v"
            AND "issue"."area_id" = "area_id_p"
            AND "issue"."state" IN ('admission', 'discussion', 'verification')
            AND "self_support"."member_id" ISNULL
            AND NOT "initiative_id_ary" @> ARRAY["initiative"."id"]
            ORDER BY md5("seed_v" || '-' || "initiative"."id")
            LIMIT 1;
          IF FOUND THEN
            "match_v" := TRUE;
            "initiative_id_ary" := "initiative_id_ary" || "result_row"."id";
            RETURN NEXT "result_row";
            IF array_length("initiative_id_ary", 1) >= "member_row"."notification_sample_size" THEN
              RETURN;
            END IF;
          END IF;
        END LOOP;
        EXIT WHEN NOT "match_v";
      END LOOP;
      RETURN;
    END;
  $$;

CREATE VIEW "updated_or_featured_initiative" AS
  SELECT * FROM "updated_initiative"
  UNION ALL
  SELECT
    "member"."id" AS "seen_by_member_id",
    FALSE AS "supported",
    EXISTS (
      SELECT NULL FROM "draft"
      WHERE "draft"."initiative_id" = "initiative"."id"
      AND COALESCE(
        "draft"."id" > "sent"."last_draft_id",
        TRUE
      )
    ) AS "new_draft",
    ( SELECT count(1) FROM "suggestion"
      WHERE "suggestion"."initiative_id" = "initiative"."id"
      AND COALESCE(
        "suggestion"."id" > "sent"."last_suggestion_id",
        TRUE
      )
    ) AS "new_suggestion_count",
    TRUE AS "featured",
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE
        "better_initiative"."issue_id" = "initiative"."issue_id"
      AND
        ( COALESCE("better_initiative"."harmonic_weight", -1),
          -"better_initiative"."id" ) >
        ( COALESCE("initiative"."harmonic_weight", -1),
          -"initiative"."id" )
    ) AS "leading",
    "initiative".*
  FROM "member" CROSS JOIN "area"
  CROSS JOIN LATERAL
    "featured_initiative"("member"."id", "area"."id") AS "initiative"
  LEFT JOIN "initiative_notification_sent" AS "sent"
    ON "sent"."member_id" = "member"."id"
    AND "sent"."initiative_id" = "initiative"."id";

CREATE VIEW "leading_complement_initiative" AS
  SELECT * FROM (
    SELECT DISTINCT ON ("seen_by_member_id", "initiative"."issue_id")
      "uf_initiative"."seen_by_member_id",
      "supporter"."member_id" NOTNULL AS "supported",
      CASE WHEN "supporter"."member_id" NOTNULL THEN FALSE ELSE
        EXISTS (
          SELECT NULL FROM "draft"
          WHERE "draft"."initiative_id" = "initiative"."id"
          AND COALESCE(
            "draft"."id" > "sent"."last_draft_id",
            TRUE
          )
        )
      END AS "new_draft",
      CASE WHEN "supporter"."member_id" NOTNULL THEN 0 ELSE
        ( SELECT count(1) FROM "suggestion"
          WHERE "suggestion"."initiative_id" = "initiative"."id"
          AND COALESCE(
            "suggestion"."id" > "sent"."last_suggestion_id",
            TRUE
          )
        )
      END AS "new_suggestion_count",
      FALSE AS "featured",
      TRUE AS "leading",
      "initiative".*
    FROM "updated_or_featured_initiative" AS "uf_initiative"
    JOIN "initiative" ON
      "uf_initiative"."issue_id" = "initiative"."issue_id"
    LEFT JOIN "supporter" ON
      "supporter"."member_id" = "uf_initiative"."seen_by_member_id" AND
      "supporter"."initiative_id" = "initiative"."id"
    LEFT JOIN "initiative_notification_sent" AS "sent"
      ON "sent"."member_id" = "uf_initiative"."seen_by_member_id"
      AND "sent"."initiative_id" = "initiative"."id"
    ORDER BY
      "seen_by_member_id",
      "initiative"."issue_id",
      "initiative"."harmonic_weight" DESC,
      "initiative"."id"
  ) AS "subquery"
  WHERE NOT EXISTS (
    SELECT NULL FROM "updated_or_featured_initiative" AS "other"
    WHERE "other"."seen_by_member_id" = "subquery"."seen_by_member_id"
    AND "other"."id" = "subquery"."id"
  );

CREATE VIEW "unfiltered_initiative_for_notification" AS
  SELECT * FROM "updated_or_featured_initiative"
  UNION ALL
  SELECT * FROM "leading_complement_initiative";

CREATE VIEW "initiative_for_notification" AS
  SELECT "initiative1".*
  FROM "unfiltered_initiative_for_notification" "initiative1"
  JOIN "issue" AS "issue1" ON "initiative1"."issue_id" = "issue1"."id"
  WHERE EXISTS (
    SELECT NULL
    FROM "unfiltered_initiative_for_notification" "initiative2"
    JOIN "issue" AS "issue2" ON "initiative2"."issue_id" = "issue2"."id"
    WHERE "initiative1"."seen_by_member_id" = "initiative2"."seen_by_member_id"
    AND "issue1"."area_id" = "issue2"."area_id"
    AND ( "initiative2"."new_draft" OR "initiative2"."new_suggestion_count" > 0 )
  );

CREATE FUNCTION "get_initiatives_for_notification"
  ( "member_id_p" "member"."id"%TYPE )
  RETURNS SETOF "initiative_for_notification"
  LANGUAGE 'plpgsql' VOLATILE AS $$
    DECLARE
      "result_row"           "initiative_for_notification"%ROWTYPE;
      "last_draft_id_v"      "draft"."id"%TYPE;
      "last_suggestion_id_v" "suggestion"."id"%TYPE;
    BEGIN
      PERFORM "require_transaction_isolation"();
      PERFORM NULL FROM "member" WHERE "id" = "member_id_p" FOR UPDATE;
      FOR "result_row" IN
        SELECT * FROM "initiative_for_notification"
        WHERE "seen_by_member_id" = "member_id_p"
      LOOP
        SELECT "id" INTO "last_draft_id_v" FROM "draft"
          WHERE "draft"."initiative_id" = "result_row"."id"
          ORDER BY "id" DESC LIMIT 1;
        SELECT "id" INTO "last_suggestion_id_v" FROM "suggestion"
          WHERE "suggestion"."initiative_id" = "result_row"."id"
          ORDER BY "id" DESC LIMIT 1;
        INSERT INTO "initiative_notification_sent"
          ("member_id", "initiative_id", "last_draft_id", "last_suggestion_id")
          VALUES (
            "member_id_p",
            "result_row"."id",
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
        WHERE "initiative_notification_sent"."member_id" = "member_id_p"
        AND "initiative"."id" = "initiative_notification_sent"."initiative_id"
        AND "issue"."id" = "initiative"."issue_id"
        AND ( "issue"."closed" NOTNULL OR "issue"."fully_frozen" NOTNULL );
      UPDATE "member" SET "notification_counter" = "notification_counter" + 1
        WHERE "id" = "member_id_p";
      RETURN;
    END;
  $$;

COMMIT;
