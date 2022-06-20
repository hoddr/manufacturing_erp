-- removes tax exempt flag from customers

BEGIN;

ALTER TABLE customers DROP COLUMN IF EXISTS is_tax_exempt;

COMMIT;
