BEGIN;

CREATE OR REPLACE VIEW "liquid_feedback_version" AS
  SELECT * FROM (VALUES ('3.0.5', 3, 0, 5))
  AS "subquery"("string", "major", "minor", "revision");

ALTER TABLE "unit" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "unit"."external_reference" IS 'Opaque data field to store an external reference';

ALTER TABLE "area" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "area"."external_reference" IS 'Opaque data field to store an external reference';

ALTER TABLE "issue" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "issue"."external_reference" IS 'Opaque data field to store an external reference';

ALTER TABLE "initiative" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "initiative"."external_reference" IS 'Opaque data field to store an external reference';

ALTER TABLE "draft" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "draft"."external_reference" IS 'Opaque data field to store an external reference';

ALTER TABLE "suggestion" ADD COLUMN "external_reference" TEXT;
COMMENT ON COLUMN "suggestion"."external_reference" IS 'Opaque data field to store an external reference';

COMMIT;
