-- support for progressive billing (e.g. BD pieces) up

BEGIN;

ALTER TABLE line_items ADD COLUMN IF NOT EXISTS is_fabbed BOOLEAN NOT NULL DEFAULT TRUE;
ALTER TABLE line_items ADD COLUMN IF NOT EXISTS quant_fabbed REAL NOT NULL DEFAULT 0.00;
ALTER TABLE orders ADD COLUMN IF NOT EXISTS is_progressive_bill BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
