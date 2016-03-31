BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.2.0', 3, 2, 0))
  AS "subquery"("string", "major", "minor", "revision");

ALTER TABLE "member" ADD COLUMN "disable_notifications" BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE "member" ADD COLUMN "notification_counter" INT4 NOT NULL DEFAULT 0;
ALTER TABLE "member" ADD COLUMN "sample_size" INT4 NOT NULL DEFAULT 3;
ALTER TABLE "member" ADD COLUMN "last_notified_suggestion_id" INT8;

UPDATE "member" SET "disable_notifications" = TRUE WHERE "notify_level" = 'none';
 
CREATE TABLE "subscription_time" (
        "member_id"             INT4            REFERENCES "member" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "day_of_week"           INT2            CONSTRAINT "day_of_week_range" CHECK ("day_of_week" BETWEEN 0 AND 6),
        "time_of_day"           TIME            NOT NULL );
CREATE UNIQUE INDEX "subscription_time_all_days_of_week_time_of_day_idx" ON "subscription_time" ("time_of_day", "member_id") WHERE ("day_of_week" ISNULL);
CREATE UNIQUE INDEX "subscription_time_day_of_week_time_of_day_idx" ON "subscription_time" ("day_of_week", "time_of_day", "member_id");

CREATE TABLE "subscription" (
        PRIMARY KEY ("member_id", "unit_id"),
        "member_id"             INT4            REFERENCES "member" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
        "unit_id"               INT4            REFERENCES "unit" ("id") ON DELETE CASCADE ON UPDATE CASCADE );
CREATE INDEX "subscription_unit_id_idx" ON "subscription" ("unit_id");
 
DROP VIEW "selected_event_seen_by_member";

CREATE VIEW "updated_initiative" AS
  SELECT
    "member"."id" AS "seen_by_member_id",
    CASE WHEN "event"."state" IN (
      'voting',
      'finished_without_winner',
      'finished_with_winner'
    ) THEN
      'voting'::"notify_level"
    ELSE
      CASE WHEN "event"."state" IN (
        'verification',
        'canceled_after_revocation_during_verification',
        'canceled_no_initiative_admitted'
      ) THEN
        'verification'::"notify_level"
      ELSE
        CASE WHEN "event"."state" IN (
          'discussion',
          'canceled_after_revocation_during_discussion'
        ) THEN
          'discussion'::"notify_level"
        ELSE
          'all'::"notify_level"
        END
      END
    END AS "notify_level",
    "event".*
  FROM "member" CROSS JOIN "event"
  LEFT JOIN "issue"
    ON "event"."issue_id" = "issue"."id"
  LEFT JOIN "membership"
    ON "member"."id" = "membership"."member_id"
    AND "issue"."area_id" = "membership"."area_id"
  LEFT JOIN "interest"
    ON "member"."id" = "interest"."member_id"
    AND "event"."issue_id" = "interest"."issue_id"
  LEFT JOIN "ignored_member"
    ON "member"."id" = "ignored_member"."member_id"
    AND "event"."member_id" = "ignored_member"."other_member_id"
  LEFT JOIN "ignored_initiative"
    ON "member"."id" = "ignored_initiative"."member_id"
    AND "event"."initiative_id" = "ignored_initiative"."initiative_id"
  WHERE (
    ( "member"."notify_level" >= 'all' ) OR
    ( "member"."notify_level" >= 'voting' AND
      "event"."state" IN (
        'voting',
        'finished_without_winner',
        'finished_with_winner' ) ) OR
    ( "member"."notify_level" >= 'verification' AND
      "event"."state" IN (
        'verification',
        'canceled_after_revocation_during_verification',
        'canceled_no_initiative_admitted' ) ) OR
    ( "member"."notify_level" >= 'discussion' AND
      "event"."state" IN (
        'discussion',
        'canceled_after_revocation_during_discussion' ) ) )
    TRUE AS "supported",
    EXISTS (
      SELECT NULL FROM "draft"
      WHERE "draft"."initiative_id" = "initiative"."id"
      AND "draft"."id" > "supporter"."draft_id"
    ) AS "new_draft",
    ( SELECT count(1) FROM "suggestion"
      WHERE "suggestion"."initiative_id" = "initiative"."id"
      AND COALESCE(
        "suggestion"."id" > "member"."last_notified_suggestion_id",
        TRUE
      )
    ) AS "new_suggestion_count",
    FALSE AS "featured",
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE
        ("better_initiative"."harmonic_weight", -"better_initiative"."id") >
        ("initiative"."harmonic_weight", -"better_initiative"."id")
    ) AS "leading",
    "initiative".*
  FROM "member" CROSS JOIN "initiative"
  JOIN "issue" ON "issue"."id" = "initiative"."issue_id"
  JOIN "supporter" ON
    "supporter"."member_id" = "member"."id" AND
    "supporter"."initiative_id" = "initiative"."id"
  WHERE "issue"."state" IN ('admission', 'discussion') ISNULL
  AND (
    EXISTS (
      SELECT NULL FROM "draft"
      WHERE "draft"."initiative_id" = "initiative"."id"
      AND "draft"."id" > "supporter"."draft_id"
    ) OR EXISTS (
      SELECT NULL FROM "suggestion"
      WHERE "suggestion"."initiative_id" = "initiative"."id"
      AND COALESCE(
        "suggestion"."id" > "member"."last_notified_suggestion_id",
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
      "sample_size_v"     INT4;
      "member_id_v"       "member"."id"%TYPE;
      "seed_v"            TEXT;
      "result_row"        "initiative"%ROWTYPE;
      "match_v"           BOOLEAN;
      "initiative_id_ary" INT4[];  --"initiative"."id"%TYPE[]
    BEGIN
      SELECT INTO "sample_size_v" "sample_size" FROM "member" WHERE "id" = "member_id_p";
      "initiative_id_ary" := '{}';
      LOOP
        "match_v" := FALSE;
        FOR "member_id_v", "seed_v" IN
          SELECT * FROM (
            SELECT DISTINCT
              "supporter"."member_id",
              md5("member_id" || '-' || "member"."notification_counter" || '-' || "area_id_p") AS "seed"
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
            IF array_length("initiative_id_ary", 1) >= "sample_size_v" THEN
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
    NULL::BOOLEAN AS "new_draft",
    NULL::INTEGER AS "new_suggestion_count",
    TRUE AS "featured",
    NOT EXISTS (
      SELECT NULL FROM "initiative" AS "better_initiative"
      WHERE
        ("better_initiative"."harmonic_weight", -"better_initiative"."id") >
        ("initiative"."harmonic_weight", -"better_initiative"."id")
    ) AS "leading",
    "initiative".*
  FROM "member" CROSS JOIN "area"
  CROSS JOIN LATERAL
    "featured_initiative"("member"."id", "area"."id") AS "initiative";

CREATE VIEW "leading_complement_initiative" AS
  SELECT * FROM (
    SELECT DISTINCT ON ("seen_by_member_id", "initiative"."issue_id")
      "updated_or_featured_initiative"."seen_by_member_id",
      FALSE AS "supported",
      NULL::BOOLEAN AS "new_draft",
      NULL::INTEGER AS "new_suggestion_count",
      FALSE AS "featured",
      TRUE AS "leading",
      "initiative".*
    FROM "updated_or_featured_initiative"
    JOIN "initiative"
    ON "updated_or_featured_initiative"."issue_id" = "initiative"."issue_id"
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

CREATE VIEW "initiative_for_notification" AS
  SELECT * FROM "updated_or_featured_initiative"
  UNION ALL
  SELECT * FROM "leading_complement_initiative";

COMMIT;
