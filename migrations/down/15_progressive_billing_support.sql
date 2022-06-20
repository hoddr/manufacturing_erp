-- support for progressive billing (e.g. BD pieces) down

BEGIN;

ALTER TABLE orders DROP COLUMN IF EXISTS is_progressive_bill;
ALTER TABLE line_items DROP COLUMN IF EXISTS quant_fabbed;
ALTER TABLE line_items DROP COLUMN IF EXISTS is_fabbed;

COMMIT;
