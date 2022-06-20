-- adding notion of customer discount (up)

BEGIN;

ALTER TABLE customers ADD COLUMN IF NOT EXISTS markup REAL NOT NULL DEFAULT 1.0;

COMMIT;
