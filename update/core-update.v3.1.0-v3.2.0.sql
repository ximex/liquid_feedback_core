BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.2.0', 3, 2, 0))
  AS "subquery"("string", "major", "minor", "revision");

ALTER TABLE "member" ADD COLUMN "disable_notifications"    BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE "member" ADD COLUMN "notification_counter"     INT4    NOT NULL DEFAULT 1;
ALTER TABLE "member" ADD COLUMN "notification_sample_size" INT4    NOT NULL DEFAULT 3;
ALTER TABLE "member" ADD COLUMN "notification_dow"         INT4    CHECK ("notification_dow" BETWEEN 0 AND 6);
ALTER TABLE "member" ADD COLUMN "notification_hour"        INT4    CHECK ("notification_hour" BETWEEN 0 AND 23);
ALTER TABLE "member" ADD COLUMN "notification_sent"        TIMESTAMP;
ALTER TABLE "member" ADD
  CONSTRAINT "notification_dow_requires_notification_hour"
  CHECK ("notification_dow" ISNULL OR "notification_hour" NOTNULL);

UPDATE "member" SET "disable_notifications" = TRUE WHERE "notify_level" = 'none'::"notify_level";

DROP VIEW "selected_event_seen_by_member";
DROP VIEW "event_seen_by_member";

ALTER TABLE "member" DROP COLUMN "notify_level";

DROP TYPE "notify_level";

COMMENT ON COLUMN "member"."disable_notifications"    IS 'TRUE if member does not want to receive notifications';
COMMENT ON COLUMN "member"."notification_counter"     IS 'Sequential number of next scheduled notification message (used as a seed for pseudo-random initiative selection algorithm)';
COMMENT ON COLUMN "member"."notification_sample_size" IS 'Number of featured initiatives per issue in scheduled notification messages';
COMMENT ON COLUMN "member"."notification_dow"         IS 'Day of week for scheduled notifications (NULL to receive a daily digest)';
COMMENT ON COLUMN "member"."notification_hour"        IS 'Time of day when scheduled notifications are sent out';
COMMENT ON COLUMN "member"."notification_sent"        IS 'Timestamp of last scheduled notification mail that has been sent out';

ALTER TABLE "rendered_member_statement" ALTER COLUMN "member_id" SET DATA TYPE INT4;

DROP VIEW "expired_session";

ALTER TABLE "session" ALTER COLUMN "member_id" SET DATA TYPE INT4;

CREATE VIEW "expired_session" AS
  SELECT * FROM "session" WHERE now() > "expiry";
CREATE RULE "delete" AS ON DELETE TO "expired_session" DO INSTEAD
  DELETE FROM "session" WHERE "ident" = OLD."ident";

COMMENT ON VIEW "expired_session" IS 'View containing all expired sessions where DELETE is possible';
COMMENT ON RULE "delete" ON "expired_session" IS 'Rule allowing DELETE on rows in "expired_session" view, i.e. DELETE FROM "expired_session"';

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

ALTER TABLE "ignored_initiative" DROP CONSTRAINT "ignored_initiative_pkey";
DROP INDEX "ignored_initiative_member_id_idx";

ALTER TABLE "ignored_initiative" ADD PRIMARY KEY ("member_id", "initiative_id");
CREATE INDEX "ignored_initiative_initiative_id_idx" ON "ignored_initiative" ("initiative_id");

COMMENT ON TABLE "ignored_initiative" IS 'An entry in this table denotes that the member does not wish to receive notifications for the given initiative';

ALTER TABLE "notification_sent" RENAME TO "notification_event_sent";
ALTER INDEX "notification_sent_singleton_idx" RENAME TO "notification_event_sent_singleton_idx";

CREATE TABLE "notification_initiative_sent" (
        PRIMARY KEY ("member_id", "initiative_id"),
        "member_id"             INT4            REFERENCES "member" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "initiative_id"         INT4            REFERENCES "initiative" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "last_draft_id"         INT8            NOT NULL,
        "last_suggestion_id"    INT8 );
CREATE INDEX "notification_initiative_sent_initiative_idx" ON "notification_initiative_sent" ("initiative_id");

COMMENT ON TABLE "notification_initiative_sent" IS 'Information which initiatives have been promoted to a member in a scheduled notification mail';

COMMENT ON COLUMN "notification_initiative_sent"."last_draft_id"      IS 'Current (i.e. last) draft_id when initiative had been promoted';
COMMENT ON COLUMN "notification_initiative_sent"."last_suggestion_id" IS 'Current (i.e. last) draft_id when initiative had been promoted';

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

COMMENT ON TABLE "newsletter" IS 'Contains newsletters created by administrators to be sent out and for further reference';

COMMENT ON COLUMN "newsletter"."published"           IS 'Timestamp when the newsletter is to be sent out (and made available in the frontend)';
COMMENT ON COLUMN "newsletter"."unit_id"             IS 'If set, only members with voting right in the given unit are considered to be recipients';
COMMENT ON COLUMN "newsletter"."include_all_members" IS 'TRUE = include all members regardless of their ''disable_notifications'' setting';
COMMENT ON COLUMN "newsletter"."sent"                IS 'Timestamp when the newsletter has been mailed out';
COMMENT ON COLUMN "newsletter"."subject"             IS 'Subject line (e.g. to be used for the email)';
COMMENT ON COLUMN "newsletter"."content"             IS 'Plain text content of the newsletter';

CREATE OR REPLACE FUNCTION "issue_requires_first_initiative_trigger"()
  RETURNS TRIGGER
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      IF NOT EXISTS (
        SELECT NULL FROM "initiative" WHERE "issue_id" = NEW."id"
      ) THEN
        RAISE EXCEPTION 'Cannot create issue without an initial initiative.' USING
          ERRCODE = 'integrity_constraint_violation',
          HINT    = 'Create issue, initiative, and draft within the same transaction.';
      END IF;
      RETURN NULL;
    END;
  $$;

CREATE OR REPLACE FUNCTION "initiative_requires_first_draft_trigger"()
  RETURNS TRIGGER
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      IF NOT EXISTS (
        SELECT NULL FROM "draft" WHERE "initiative_id" = NEW."id"
      ) THEN
        RAISE EXCEPTION 'Cannot create initiative without an initial draft.' USING
          ERRCODE = 'integrity_constraint_violation',
          HINT    = 'Create issue, initiative and draft within the same transaction.';
      END IF;
      RETURN NULL;
    END;
  $$;

CREATE OR REPLACE FUNCTION "suggestion_requires_first_opinion_trigger"()
  RETURNS TRIGGER
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      IF NOT EXISTS (
        SELECT NULL FROM "opinion" WHERE "suggestion_id" = NEW."id"
      ) THEN
        RAISE EXCEPTION 'Cannot create a suggestion without an opinion.' USING
          ERRCODE = 'integrity_constraint_violation',
          HINT    = 'Create suggestion and opinion within the same transaction.';
      END IF;
      RETURN NULL;
    END;
  $$;

CREATE OR REPLACE FUNCTION "forbid_changes_on_closed_issue_trigger"()
  RETURNS TRIGGER
  LANGUAGE 'plpgsql' VOLATILE AS $$
    DECLARE
      "issue_id_v" "issue"."id"%TYPE;
      "issue_row"  "issue"%ROWTYPE;
    BEGIN
      IF EXISTS (
        SELECT NULL FROM "temporary_transaction_data"
        WHERE "txid" = txid_current()
        AND "key" = 'override_protection_triggers'
        AND "value" = TRUE::TEXT
      ) THEN
        RETURN NULL;
      END IF;
      IF TG_OP = 'DELETE' THEN
        "issue_id_v" := OLD."issue_id";
      ELSE
        "issue_id_v" := NEW."issue_id";
      END IF;
      SELECT INTO "issue_row" * FROM "issue"
        WHERE "id" = "issue_id_v" FOR SHARE;
      IF (
        "issue_row"."closed" NOTNULL OR (
          "issue_row"."state" = 'voting' AND
          "issue_row"."phase_finished" NOTNULL
        )
      ) THEN
        IF
          TG_RELID = 'direct_voter'::regclass AND
          TG_OP = 'UPDATE'
        THEN
          IF
            OLD."issue_id"  = NEW."issue_id"  AND
            OLD."member_id" = NEW."member_id" AND
            OLD."weight" = NEW."weight"
          THEN
            RETURN NULL;  -- allows changing of voter comment
          END IF;
        END IF;
        RAISE EXCEPTION 'Tried to modify data after voting has been closed.' USING
          ERRCODE = 'integrity_constraint_violation';
      END IF;
      RETURN NULL;
    END;
  $$;

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

COMMENT ON VIEW "event_for_notification" IS 'Entries of the "event" table which are of interest for a particular notification mail recipient';

COMMENT ON COLUMN "event_for_notification"."recipient_id" IS 'member_id of the recipient of a notification mail';

CREATE VIEW "updated_initiative" AS
  SELECT
    "supporter"."member_id" AS "recipient_id",
    FALSE AS "featured",
    "supporter"."initiative_id"
  FROM "supporter"
  JOIN "initiative" ON "supporter"."initiative_id" = "initiative"."id"
  JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
  LEFT JOIN "notification_initiative_sent" AS "sent" ON
    "sent"."member_id" = "supporter"."member_id" AND
    "sent"."initiative_id" = "supporter"."initiative_id"
  LEFT JOIN "ignored_initiative" ON
    "ignored_initiative"."member_id" = "supporter"."member_id" AND
    "ignored_initiative"."initiative_id" = "supporter"."initiative_id"
  WHERE "issue"."state" IN ('admission', 'discussion')
  AND "initiative"."revoked" ISNULL
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

COMMENT ON VIEW "updated_initiative" IS 'Helper view for view "updated_or_featured_initiative"';

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
            AND "initiative"."revoked" ISNULL
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

COMMENT ON FUNCTION "featured_initiative"
  ( "recipient_id_p" "member"."id"%TYPE,
    "area_id_p"      "area"."id"%TYPE )
  IS 'Helper function for view "updated_or_featured_initiative"';

CREATE VIEW "updated_or_featured_initiative" AS
  SELECT
    "subquery".*,
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE "better_initiative"."issue_id" = "initiative"."issue_id"
      AND
        ( COALESCE("better_initiative"."supporter_count", -1),
          -"better_initiative"."id" ) >
        ( COALESCE("initiative"."supporter_count", -1),
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

COMMENT ON VIEW "updated_or_featured_initiative" IS 'Initiatives to be included in a scheduled notification mail because (a) they have been updated or (b) they are featured';

COMMENT ON COLUMN "updated_or_featured_initiative"."recipient_id" IS '"id" of the member who receives the notification mail';
COMMENT ON COLUMN "updated_or_featured_initiative"."featured" IS 'TRUE if the initiative has been included because it was selected by the "featured_initiative" algorithm (see source of function "featured_initiative")';
COMMENT ON COLUMN "updated_or_featured_initiative"."initiative_id" IS '"id" of the initiative to be included in the notification mail';
COMMENT ON COLUMN "updated_or_featured_initiative"."leading" IS 'TRUE if the initiative has the highest "supporter_count" in the issue';

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
    WHERE "initiative"."revoked" ISNULL
    ORDER BY
      "uf_initiative"."recipient_id",
      "initiative"."issue_id",
      "initiative"."supporter_count" DESC,
      "initiative"."id"
  ) AS "subquery"
  WHERE NOT EXISTS (
    SELECT NULL FROM "updated_or_featured_initiative" AS "other"
    WHERE "other"."recipient_id" = "subquery"."recipient_id"
    AND "other"."initiative_id" = "subquery"."initiative_id"
  );

COMMENT ON VIEW "leading_complement_initiative" IS 'Helper view for view "unfiltered_initiative_for_notification" in order to always include the most supported initiative of an issue';
COMMENT ON COLUMN "leading_complement_initiative"."featured" IS 'Always FALSE in this view';
COMMENT ON COLUMN "leading_complement_initiative"."initiative_id" IS '"id" of the initiative to be included in the notification mail';
COMMENT ON COLUMN "leading_complement_initiative"."leading" IS 'Always TRUE in this view';

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
  LEFT JOIN "notification_initiative_sent" AS "sent" ON
    "sent"."member_id" = "subquery"."recipient_id" AND
    "sent"."initiative_id" = "subquery"."initiative_id";

COMMENT ON VIEW "unfiltered_initiative_for_notification" IS 'Helper view which simply combines the views "updated_or_featured_initiative" and "leading_complement_initiative" and adds columns "supported", "new_draft", and "new_suggestion_count';

COMMENT ON COLUMN "unfiltered_initiative_for_notification"."supported"            IS 'TRUE if initiative is supported by the recipient';
COMMENT ON COLUMN "unfiltered_initiative_for_notification"."new_draft"            IS 'TRUE if a new draft exists (using the "draft_id" column of the "supporter" table in case of "supported" initiatives and the "last_draft_id" column of the "notification_initiative_sent" table in all other cases)';
COMMENT ON COLUMN "unfiltered_initiative_for_notification"."new_suggestion_count" IS 'Number of new suggestions (using the "last_suggestion_id" column of the "notification_initiative_sent" table while ignoring suggestions with an "opinion")';

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

COMMENT ON VIEW "initiative_for_notification" IS 'Initiatives to be included in a scheduled notification mail';

COMMENT ON COLUMN "initiative_for_notification"."recipient_id"         IS '"id" of the member who receives the notification mail';
COMMENT ON COLUMN "initiative_for_notification"."featured"             IS 'TRUE if the initiative has been included because it was selected by the "featured_initiative" algorithm (see source of function "featured_initiative")';
COMMENT ON COLUMN "initiative_for_notification"."initiative_id"        IS '"id" of the initiative to be included in the notification mail';
COMMENT ON COLUMN "initiative_for_notification"."leading"              IS 'TRUE if the initiative has the highest "supporter_count" in the issue';
COMMENT ON COLUMN "initiative_for_notification"."supported"            IS 'TRUE if initiative is supported by the recipient';
COMMENT ON COLUMN "initiative_for_notification"."new_draft"            IS 'TRUE if a new draft exists (using the "draft_id" column of the "supporter" table in case of "supported" initiatives and the "last_draft_id" column of the "notification_initiative_sent" table in all other cases)';
COMMENT ON COLUMN "initiative_for_notification"."new_suggestion_count" IS 'Number of new suggestions (using the "last_suggestion_id" column of the "notification_initiative_sent" table while ignoring suggestions with an "opinion")';

CREATE VIEW "scheduled_notification_to_send" AS
  SELECT * FROM (
    SELECT
      "id" AS "recipient_id",
      now() - CASE WHEN "notification_dow" ISNULL THEN
        ( "notification_sent"::DATE + CASE
          WHEN EXTRACT(HOUR FROM "notification_sent") < "notification_hour"
          THEN 0 ELSE 1 END
        )::TIMESTAMP + '1 hour'::INTERVAL * "notification_hour"
      ELSE
        ( "notification_sent"::DATE +
          ( 7 + "notification_dow" -
            EXTRACT(DOW FROM
              ( "notification_sent"::DATE + CASE
                WHEN EXTRACT(HOUR FROM "notification_sent") < "notification_hour"
                THEN 0 ELSE 1 END
              )::TIMESTAMP + '1 hour'::INTERVAL * "notification_hour"
            )::INTEGER
          ) % 7 +
          CASE
            WHEN EXTRACT(HOUR FROM "notification_sent") < "notification_hour"
            THEN 0 ELSE 1
          END
        )::TIMESTAMP + '1 hour'::INTERVAL * "notification_hour"
      END AS "pending"
    FROM (
      SELECT
        "id",
        COALESCE("notification_sent", "activated") AS "notification_sent",
        "notification_dow",
        "notification_hour"
      FROM "member"
      WHERE "disable_notifications" = FALSE
      AND "notification_hour" NOTNULL
    ) AS "subquery1"
  ) AS "subquery2"
  WHERE "pending" > '0'::INTERVAL;

COMMENT ON VIEW "scheduled_notification_to_send" IS 'Set of members where a scheduled notification mail is pending';

COMMENT ON COLUMN "scheduled_notification_to_send"."recipient_id" IS '"id" of the member who needs to receive a notification mail';
COMMENT ON COLUMN "scheduled_notification_to_send"."pending"      IS 'Duration for which the notification mail has already been pending';

CREATE VIEW "newsletter_to_send" AS
  SELECT
    "member"."id" AS "recipient_id",
    "newsletter"."id" AS "newsletter_id",
    "newsletter"."published"
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

COMMENT ON VIEW "newsletter_to_send" IS 'List of "newsletter_id"s for each member that are due to be sent out';
COMMENT ON COLUMN "newsletter"."published" IS 'Timestamp when the newsletter was supposed to be sent out (can be used for ordering)';

CREATE OR REPLACE FUNCTION "require_transaction_isolation"()
  RETURNS VOID
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      IF
        current_setting('transaction_isolation') NOT IN
        ('repeatable read', 'serializable')
      THEN
        RAISE EXCEPTION 'Insufficient transaction isolation level' USING
          HINT = 'Consider using SET TRANSACTION ISOLATION LEVEL REPEATABLE READ.';
      END IF;
      RETURN;
    END;
  $$;
 
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
        INSERT INTO "notification_initiative_sent"
          ("member_id", "initiative_id", "last_draft_id", "last_suggestion_id")
          VALUES (
            "recipient_id_p",
            "result_row"."initiative_id",
            "last_draft_id_v",
            "last_suggestion_id_v" )
          ON CONFLICT ("member_id", "initiative_id") DO UPDATE SET
            "last_draft_id" = CASE
              WHEN "notification_initiative_sent"."last_draft_id" > "last_draft_id_v"
              THEN "notification_initiative_sent"."last_draft_id"
              ELSE "last_draft_id_v"
            END,
            "last_suggestion_id" = CASE
              WHEN "notification_initiative_sent"."last_suggestion_id" > "last_suggestion_id_v"
              THEN "notification_initiative_sent"."last_suggestion_id"
              ELSE "last_suggestion_id_v"
            END;
        RETURN NEXT "result_row";
      END LOOP;
      DELETE FROM "notification_initiative_sent"
        USING "initiative", "issue"
        WHERE "notification_initiative_sent"."member_id" = "recipient_id_p"
        AND "initiative"."id" = "notification_initiative_sent"."initiative_id"
        AND "issue"."id" = "initiative"."issue_id"
        AND ( "issue"."closed" NOTNULL OR "issue"."fully_frozen" NOTNULL );
      UPDATE "member" SET
        "notification_counter" = "notification_counter" + 1,
        "notification_sent" = now()
        WHERE "id" = "recipient_id_p";
      RETURN;
    END;
  $$;

COMMENT ON FUNCTION "get_initiatives_for_notification"
  ( "member"."id"%TYPE )
  IS 'Returns rows from view "initiative_for_notification" for a given recipient while updating table "notification_initiative_sent" and columns "notification_counter" and "notification_sent" of "member" table';

CREATE OR REPLACE FUNCTION "delete_member"("member_id_p" "member"."id"%TYPE)
  RETURNS VOID
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      UPDATE "member" SET
        "last_login"                   = NULL,
        "last_delegation_check"        = NULL,
        "login"                        = NULL,
        "password"                     = NULL,
        "authority"                    = NULL,
        "authority_uid"                = NULL,
        "authority_login"              = NULL,
        "locked"                       = TRUE,
        "active"                       = FALSE,
        "notify_email"                 = NULL,
        "notify_email_unconfirmed"     = NULL,
        "notify_email_secret"          = NULL,
        "notify_email_secret_expiry"   = NULL,
        "notify_email_lock_expiry"     = NULL,
        "disable_notifications"        = NULL,
        "notification_counter"         = NULL,
        "notification_sample_size"     = NULL,
        "notification_dow"             = NULL,
        "notification_hour"            = NULL,
        "login_recovery_expiry"        = NULL,
        "password_reset_secret"        = NULL,
        "password_reset_secret_expiry" = NULL,
        "organizational_unit"          = NULL,
        "internal_posts"               = NULL,
        "realname"                     = NULL,
        "birthday"                     = NULL,
        "address"                      = NULL,
        "email"                        = NULL,
        "xmpp_address"                 = NULL,
        "website"                      = NULL,
        "phone"                        = NULL,
        "mobile_phone"                 = NULL,
        "profession"                   = NULL,
        "external_memberships"         = NULL,
        "external_posts"               = NULL,
        "statement"                    = NULL
        WHERE "id" = "member_id_p";
      -- "text_search_data" is updated by triggers
      DELETE FROM "setting"            WHERE "member_id" = "member_id_p";
      DELETE FROM "setting_map"        WHERE "member_id" = "member_id_p";
      DELETE FROM "member_relation_setting" WHERE "member_id" = "member_id_p";
      DELETE FROM "member_image"       WHERE "member_id" = "member_id_p";
      DELETE FROM "contact"            WHERE "member_id" = "member_id_p";
      DELETE FROM "ignored_member"     WHERE "member_id" = "member_id_p";
      DELETE FROM "session"            WHERE "member_id" = "member_id_p";
      DELETE FROM "area_setting"       WHERE "member_id" = "member_id_p";
      DELETE FROM "issue_setting"      WHERE "member_id" = "member_id_p";
      DELETE FROM "ignored_initiative" WHERE "member_id" = "member_id_p";
      DELETE FROM "initiative_setting" WHERE "member_id" = "member_id_p";
      DELETE FROM "suggestion_setting" WHERE "member_id" = "member_id_p";
      DELETE FROM "membership"         WHERE "member_id" = "member_id_p";
      DELETE FROM "delegation"         WHERE "truster_id" = "member_id_p";
      DELETE FROM "non_voter"          WHERE "member_id" = "member_id_p";
      DELETE FROM "direct_voter" USING "issue"
        WHERE "direct_voter"."issue_id" = "issue"."id"
        AND "issue"."closed" ISNULL
        AND "member_id" = "member_id_p";
      RETURN;
    END;
  $$;

CREATE OR REPLACE FUNCTION "delete_private_data"()
  RETURNS VOID
  LANGUAGE 'plpgsql' VOLATILE AS $$
    BEGIN
      DELETE FROM "temporary_transaction_data";
      DELETE FROM "member" WHERE "activated" ISNULL;
      UPDATE "member" SET
        "invite_code"                  = NULL,
        "invite_code_expiry"           = NULL,
        "admin_comment"                = NULL,
        "last_login"                   = NULL,
        "last_delegation_check"        = NULL,
        "login"                        = NULL,
        "password"                     = NULL,
        "authority"                    = NULL,
        "authority_uid"                = NULL,
        "authority_login"              = NULL,
        "lang"                         = NULL,
        "notify_email"                 = NULL,
        "notify_email_unconfirmed"     = NULL,
        "notify_email_secret"          = NULL,
        "notify_email_secret_expiry"   = NULL,
        "notify_email_lock_expiry"     = NULL,
        "disable_notifications"        = NULL,
        "notification_counter"         = NULL,
        "notification_sample_size"     = NULL,
        "notification_dow"             = NULL,
        "notification_hour"            = NULL,
        "login_recovery_expiry"        = NULL,
        "password_reset_secret"        = NULL,
        "password_reset_secret_expiry" = NULL,
        "organizational_unit"          = NULL,
        "internal_posts"               = NULL,
        "realname"                     = NULL,
        "birthday"                     = NULL,
        "address"                      = NULL,
        "email"                        = NULL,
        "xmpp_address"                 = NULL,
        "website"                      = NULL,
        "phone"                        = NULL,
        "mobile_phone"                 = NULL,
        "profession"                   = NULL,
        "external_memberships"         = NULL,
        "external_posts"               = NULL,
        "formatting_engine"            = NULL,
        "statement"                    = NULL;
      -- "text_search_data" is updated by triggers
      DELETE FROM "setting";
      DELETE FROM "setting_map";
      DELETE FROM "member_relation_setting";
      DELETE FROM "member_image";
      DELETE FROM "contact";
      DELETE FROM "ignored_member";
      DELETE FROM "session";
      DELETE FROM "area_setting";
      DELETE FROM "issue_setting";
      DELETE FROM "ignored_initiative";
      DELETE FROM "initiative_setting";
      DELETE FROM "suggestion_setting";
      DELETE FROM "non_voter";
      DELETE FROM "direct_voter" USING "issue"
        WHERE "direct_voter"."issue_id" = "issue"."id"
        AND "issue"."closed" ISNULL;
      RETURN;
    END;
  $$;

COMMIT;
