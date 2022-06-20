-- (down) migration to add and support order type markup types (e.g. for QP, QL, and FS)

BEGIN;

ALTER TABLE markups DROP COLUMN IF EXISTS quote;
ALTER TABLE markups DROP COLUMN IF EXISTS project;
ALTER TABLE markups DROP COLUMN IF EXISTS normal;

COMMIT;
