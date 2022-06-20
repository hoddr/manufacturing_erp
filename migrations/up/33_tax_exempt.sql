-- adds tax exempt flag to customers

BEGIN;

ALTER TABLE customers ADD COLUMN IF NOT EXISTS is_tax_exempt BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
