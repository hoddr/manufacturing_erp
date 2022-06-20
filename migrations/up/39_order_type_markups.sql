-- migration to add and support order type markup types (e.g. for QP, QL, and FS)

BEGIN;

ALTER TABLE markups ADD COLUMN IF NOT EXISTS quote REAL NOT NULL DEFAULT 1.0;
ALTER TABLE markups ADD COLUMN IF NOT EXISTS project REAL NOT NULL DEFAULT 1.0;
ALTER TABLE markups ADD COLUMN IF NOT EXISTS normal REAL NOT NULL DEFAULT 1.0;

COMMIT;
