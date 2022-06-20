-- adds invoice number field to order, adds date for billed, adds date for complete

BEGIN;

ALTER TABLE orders ADD COLUMN IF NOT EXISTS invoice_number VARCHAR(8);
ALTER TABLE orders ADD COLUMN IF NOT EXISTS billed TIMESTAMP WITH TIME ZONE;
ALTER TABLE orders ADD COLUMN IF NOT EXISTS complete TIMESTAMP WITH TIME ZONE;

UPDATE orders set billed = created;
UPDATE orders set complete = created;

COMMIT;
