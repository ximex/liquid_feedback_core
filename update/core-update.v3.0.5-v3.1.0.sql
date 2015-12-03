BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.1.0', 3, 1, 0))
  AS "subquery"("string", "major", "minor", "revision");

ALTER TABLE "member" DROP CONSTRAINT "authority_requires_uid_and_vice_versa";
ALTER TABLE "member" ADD CONSTRAINT "authority_requires_uid_and_vice_versa" CHECK (
  ("authority" NOTNULL) = ("authority_uid" NOTNULL) );

DROP TABLE "member_application";
DROP TYPE "application_access_level";

ALTER TABLE "policy" ADD COLUMN "min_admission_time" INTERVAL DEFAULT '0';
ALTER TABLE "policy" ALTER COLUMN "min_admission_time" DROP DEFAULT;
ALTER TABLE "policy" RENAME COLUMN "admission_time" TO "max_admission_time";

ALTER TABLE "policy" DROP CONSTRAINT "timing";
ALTER TABLE "policy" ADD CONSTRAINT "timing" CHECK (
  ( "polling" = FALSE AND
    "min_admission_time" NOTNULL AND "max_admission_time" NOTNULL AND
    "min_admission_time" <= "max_admission_time" AND
    "discussion_time" NOTNULL AND
    "verification_time" NOTNULL AND
    "voting_time" NOTNULL ) OR
  ( "polling" = TRUE AND
    "min_admission_time" ISNULL AND "max_admission_time" ISNULL AND
    "discussion_time" NOTNULL AND
    "verification_time" NOTNULL AND
    "voting_time" NOTNULL ) OR
  ( "polling" = TRUE AND
    "min_admission_time" ISNULL AND "max_admission_time" ISNULL AND
    "discussion_time" ISNULL AND
    "verification_time" ISNULL AND
    "voting_time" ISNULL ) );

ALTER TABLE "policy" DROP CONSTRAINT "issue_quorum_if_and_only_if_not_polling";
ALTER TABLE "policy" ADD CONSTRAINT "issue_quorum_if_and_only_if_not_polling" CHECK (
  "polling" = ("issue_quorum_num" ISNULL) AND
  "polling" = ("issue_quorum_den" ISNULL) );

COMMENT ON COLUMN "policy"."polling" IS 'TRUE = special policy for non-user-generated issues without issue quorum, where certain initiatives (those having the "polling" flag set) do not need to pass the initiative quorum; "min_admission_time" and "max_admission_time" MUST be set to NULL, the other timings may be set to NULL altogether, allowing individual timing for those issues';
COMMENT ON COLUMN "policy"."min_admission_time" IS 'Minimum duration of issue state ''admission''; Minimum time an issue stays open';

ALTER TABLE "issue" ADD COLUMN "min_admission_time" INTERVAL DEFAULT '0';
ALTER TABLE "issue" ALTER COLUMN "min_admission_time" DROP DEFAULT;
ALTER TABLE "issue" RENAME COLUMN "admission_time" TO "max_admission_time";

ALTER TABLE "issue" DROP CONSTRAINT "admission_time_not_null_unless_instantly_accepted";
ALTER TABLE "issue" ADD CONSTRAINT "admission_time_not_null_unless_instantly_accepted" CHECK (
  ("min_admission_time" NOTNULL) = ("max_admission_time" NOTNULL) AND
  ("min_admission_time" NOTNULL OR ("accepted" NOTNULL AND "accepted" = "created")) );

ALTER TABLE "issue" DROP CONSTRAINT "set_both_or_none_of_snapshot_and_latest_snapshot_event";
ALTER TABLE "issue" ADD CONSTRAINT "set_both_or_none_of_snapshot_and_latest_snapshot_event" CHECK (
  (("snapshot" NOTNULL) = ("latest_snapshot_event" NOTNULL)) );

COMMENT ON COLUMN "issue"."closed" IS 'Point in time, when "max_admission_time" or "voting_time" have elapsed, and issue is no longer active; Frontends must ensure that for closed issues additionally to the restrictions for half_frozen and fully_frozen issues a) no voter is added or removed to/from the direct_voter table, b) no votes are added, modified or removed.';
COMMENT ON COLUMN "issue"."min_admission_time" IS 'Copied from "policy" table at creation of issue';

DROP TRIGGER "update_text_search_data" ON "initiative";
ALTER TABLE "initiative" DROP COLUMN "discussion_url";

ALTER TABLE "initiative" DROP CONSTRAINT "all_or_none_of_revoked_and_revoked_by_member_id_must_be_null";
ALTER TABLE "initiative" ADD CONSTRAINT "all_or_none_of_revoked_and_revoked_by_member_id_must_be_null" CHECK (
  ("revoked" NOTNULL) = ("revoked_by_member_id" NOTNULL) );

 CREATE TRIGGER "update_text_search_data"
   BEFORE INSERT OR UPDATE ON "initiative"
   FOR EACH ROW EXECUTE PROCEDURE
   tsvector_update_trigger('text_search_data', 'pg_catalog.simple', "name");

ALTER TABLE "event" DROP CONSTRAINT "null_constraints_for_issue_state_changed";
ALTER TABLE "event" DROP CONSTRAINT "null_constraints_for_initiative_creation_or_revocation_or_new_draft";
ALTER TABLE "event" DROP CONSTRAINT "null_constraints_for_suggestion_creation";

ALTER TABLE "event" ADD CONSTRAINT "null_constr_for_issue_state_changed" CHECK (
  "event" != 'issue_state_changed' OR (
    "member_id"     ISNULL  AND
    "issue_id"      NOTNULL AND
    "state"         NOTNULL AND
    "initiative_id" ISNULL  AND
    "draft_id"      ISNULL  AND
    "suggestion_id" ISNULL  ) );
ALTER TABLE "event" ADD CONSTRAINT "null_constr_for_initiative_creation_or_revocation_or_new_draft" CHECK (
  "event" NOT IN (
    'initiative_created_in_new_issue',
    'initiative_created_in_existing_issue',
    'initiative_revoked',
    'new_draft_created'
  ) OR (
    "member_id"     NOTNULL AND
    "issue_id"      NOTNULL AND
    "state"         NOTNULL AND
    "initiative_id" NOTNULL AND
    "draft_id"      NOTNULL AND
    "suggestion_id" ISNULL  ) );
ALTER TABLE "event" ADD CONSTRAINT "null_constr_for_suggestion_creation" CHECK (
  "event" != 'suggestion_created' OR (
    "member_id"     NOTNULL AND
    "issue_id"      NOTNULL AND
    "state"         NOTNULL AND
    "initiative_id" NOTNULL AND
    "draft_id"      ISNULL  AND
    "suggestion_id" NOTNULL ) );

CREATE OR REPLACE FUNCTION "copy_timings_trigger"()
  RETURNS TRIGGER
  LANGUAGE 'plpgsql' VOLATILE AS $$
    DECLARE
      "policy_row" "policy"%ROWTYPE;
    BEGIN
      SELECT * INTO "policy_row" FROM "policy"
        WHERE "id" = NEW."policy_id";
      IF NEW."min_admission_time" ISNULL THEN
        NEW."min_admission_time" := "policy_row"."min_admission_time";
      END IF;
      IF NEW."max_admission_time" ISNULL THEN
        NEW."max_admission_time" := "policy_row"."max_admission_time";
      END IF;
      IF NEW."discussion_time" ISNULL THEN
        NEW."discussion_time" := "policy_row"."discussion_time";
      END IF;
      IF NEW."verification_time" ISNULL THEN
        NEW."verification_time" := "policy_row"."verification_time";
      END IF;
      IF NEW."voting_time" ISNULL THEN
        NEW."voting_time" := "policy_row"."voting_time";
      END IF;
      RETURN NEW;
    END;
  $$;

CREATE OR REPLACE FUNCTION "check_issue"
  ( "issue_id_p" "issue"."id"%TYPE,
    "persist"    "check_issue_persistence" )
  RETURNS "check_issue_persistence"
  LANGUAGE 'plpgsql' VOLATILE AS $$
    DECLARE
      "issue_row"      "issue"%ROWTYPE;
      "policy_row"     "policy"%ROWTYPE;
      "initiative_row" "initiative"%ROWTYPE;
      "state_v"        "issue_state";
    BEGIN
      PERFORM "require_transaction_isolation"();
      IF "persist" ISNULL THEN
        SELECT * INTO "issue_row" FROM "issue" WHERE "id" = "issue_id_p"
          FOR UPDATE;
        IF "issue_row"."closed" NOTNULL THEN
          RETURN NULL;
        END IF;
        "persist"."state" := "issue_row"."state";
        IF
          ( "issue_row"."state" = 'admission' AND now() >=
            "issue_row"."created" + "issue_row"."max_admission_time" ) OR
          ( "issue_row"."state" = 'discussion' AND now() >=
            "issue_row"."accepted" + "issue_row"."discussion_time" ) OR
          ( "issue_row"."state" = 'verification' AND now() >=
            "issue_row"."half_frozen" + "issue_row"."verification_time" ) OR
          ( "issue_row"."state" = 'voting' AND now() >=
            "issue_row"."fully_frozen" + "issue_row"."voting_time" )
        THEN
          "persist"."phase_finished" := TRUE;
        ELSE
          "persist"."phase_finished" := FALSE;
        END IF;
        IF
          NOT EXISTS (
            -- all initiatives are revoked
            SELECT NULL FROM "initiative"
            WHERE "issue_id" = "issue_id_p" AND "revoked" ISNULL
          ) AND (
            -- and issue has not been accepted yet
            "persist"."state" = 'admission' OR
            -- or verification time has elapsed
            ( "persist"."state" = 'verification' AND
              "persist"."phase_finished" ) OR
            -- or no initiatives have been revoked lately
            NOT EXISTS (
              SELECT NULL FROM "initiative"
              WHERE "issue_id" = "issue_id_p"
              AND now() < "revoked" + "issue_row"."verification_time"
            )
          )
        THEN
          "persist"."issue_revoked" := TRUE;
        ELSE
          "persist"."issue_revoked" := FALSE;
        END IF;
        IF "persist"."phase_finished" OR "persist"."issue_revoked" THEN
          UPDATE "issue" SET "phase_finished" = now()
            WHERE "id" = "issue_row"."id";
          RETURN "persist";
        ELSIF
          "persist"."state" IN ('admission', 'discussion', 'verification')
        THEN
          RETURN "persist";
        ELSE
          RETURN NULL;
        END IF;
      END IF;
      IF
        "persist"."state" IN ('admission', 'discussion', 'verification') AND
        coalesce("persist"."snapshot_created", FALSE) = FALSE
      THEN
        PERFORM "create_snapshot"("issue_id_p");
        "persist"."snapshot_created" = TRUE;
        IF "persist"."phase_finished" THEN
          IF "persist"."state" = 'admission' THEN
            PERFORM "set_snapshot_event"("issue_id_p", 'end_of_admission');
          ELSIF "persist"."state" = 'discussion' THEN
            PERFORM "set_snapshot_event"("issue_id_p", 'half_freeze');
          ELSIF "persist"."state" = 'verification' THEN
            PERFORM "set_snapshot_event"("issue_id_p", 'full_freeze');
            SELECT * INTO "issue_row" FROM "issue" WHERE "id" = "issue_id_p";
            SELECT * INTO "policy_row" FROM "policy"
              WHERE "id" = "issue_row"."policy_id";
            FOR "initiative_row" IN
              SELECT * FROM "initiative"
              WHERE "issue_id" = "issue_id_p" AND "revoked" ISNULL
              FOR UPDATE
            LOOP
              IF
                "initiative_row"."polling" OR (
                  "initiative_row"."satisfied_supporter_count" > 0 AND
                  "initiative_row"."satisfied_supporter_count" *
                  "policy_row"."initiative_quorum_den" >=
                  "issue_row"."population" * "policy_row"."initiative_quorum_num"
                )
              THEN
                UPDATE "initiative" SET "admitted" = TRUE
                  WHERE "id" = "initiative_row"."id";
              ELSE
                UPDATE "initiative" SET "admitted" = FALSE
                  WHERE "id" = "initiative_row"."id";
              END IF;
            END LOOP;
          END IF;
        END IF;
        RETURN "persist";
      END IF;
      IF
        "persist"."state" IN ('admission', 'discussion', 'verification') AND
        coalesce("persist"."harmonic_weights_set", FALSE) = FALSE
      THEN
        PERFORM "set_harmonic_initiative_weights"("issue_id_p");
        "persist"."harmonic_weights_set" = TRUE;
        IF
          "persist"."phase_finished" OR
          "persist"."issue_revoked" OR
          "persist"."state" = 'admission'
        THEN
          RETURN "persist";
        ELSE
          RETURN NULL;
        END IF;
      END IF;
      IF "persist"."issue_revoked" THEN
        IF "persist"."state" = 'admission' THEN
          "state_v" := 'canceled_revoked_before_accepted';
        ELSIF "persist"."state" = 'discussion' THEN
          "state_v" := 'canceled_after_revocation_during_discussion';
        ELSIF "persist"."state" = 'verification' THEN
          "state_v" := 'canceled_after_revocation_during_verification';
        END IF;
        UPDATE "issue" SET
          "state"          = "state_v",
          "closed"         = "phase_finished",
          "phase_finished" = NULL
          WHERE "id" = "issue_id_p";
        RETURN NULL;
      END IF;
      IF "persist"."state" = 'admission' THEN
        SELECT * INTO "issue_row" FROM "issue" WHERE "id" = "issue_id_p"
          FOR UPDATE;
        SELECT * INTO "policy_row"
          FROM "policy" WHERE "id" = "issue_row"."policy_id";
        IF 
          ( now() >=
            "issue_row"."created" + "issue_row"."min_admission_time" ) AND
          EXISTS (
            SELECT NULL FROM "initiative"
            WHERE "issue_id" = "issue_id_p"
            AND "supporter_count" > 0
            AND "supporter_count" * "policy_row"."issue_quorum_den"
            >= "issue_row"."population" * "policy_row"."issue_quorum_num"
          )
        THEN
          UPDATE "issue" SET
            "state"          = 'discussion',
            "accepted"       = coalesce("phase_finished", now()),
            "phase_finished" = NULL
            WHERE "id" = "issue_id_p";
        ELSIF "issue_row"."phase_finished" NOTNULL THEN
          UPDATE "issue" SET
            "state"          = 'canceled_issue_not_accepted',
            "closed"         = "phase_finished",
            "phase_finished" = NULL
            WHERE "id" = "issue_id_p";
        END IF;
        RETURN NULL;
      END IF;
      IF "persist"."phase_finished" THEN
        IF "persist"."state" = 'discussion' THEN
          UPDATE "issue" SET
            "state"          = 'verification',
            "half_frozen"    = "phase_finished",
            "phase_finished" = NULL
            WHERE "id" = "issue_id_p";
          RETURN NULL;
        END IF;
        IF "persist"."state" = 'verification' THEN
          SELECT * INTO "issue_row" FROM "issue" WHERE "id" = "issue_id_p"
            FOR UPDATE;
          SELECT * INTO "policy_row" FROM "policy"
            WHERE "id" = "issue_row"."policy_id";
          IF EXISTS (
            SELECT NULL FROM "initiative"
            WHERE "issue_id" = "issue_id_p" AND "admitted" = TRUE
          ) THEN
            UPDATE "issue" SET
              "state"          = 'voting',
              "fully_frozen"   = "phase_finished",
              "phase_finished" = NULL
              WHERE "id" = "issue_id_p";
          ELSE
            UPDATE "issue" SET
              "state"          = 'canceled_no_initiative_admitted',
              "fully_frozen"   = "phase_finished",
              "closed"         = "phase_finished",
              "phase_finished" = NULL
              WHERE "id" = "issue_id_p";
            -- NOTE: The following DELETE statements have effect only when
            --       issue state has been manipulated
            DELETE FROM "direct_voter"     WHERE "issue_id" = "issue_id_p";
            DELETE FROM "delegating_voter" WHERE "issue_id" = "issue_id_p";
            DELETE FROM "battle"           WHERE "issue_id" = "issue_id_p";
          END IF;
          RETURN NULL;
        END IF;
        IF "persist"."state" = 'voting' THEN
          IF coalesce("persist"."closed_voting", FALSE) = FALSE THEN
            PERFORM "close_voting"("issue_id_p");
            "persist"."closed_voting" = TRUE;
            RETURN "persist";
          END IF;
          PERFORM "calculate_ranks"("issue_id_p");
          RETURN NULL;
        END IF;
      END IF;
      RAISE WARNING 'should not happen';
      RETURN NULL;
    END;
  $$;

COMMIT;
