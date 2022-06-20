-- drops invoice number field on order, drops billed date, drops complete date

BEGIN;

ALTER TABLE orders DROP COLUMN IF EXISTS invoice_number;
ALTER TABLE orders DROP COLUMN IF EXISTS billed;
ALTER TABLE orders DROP COLUMN IF EXISTS complete;

COMMIT;
