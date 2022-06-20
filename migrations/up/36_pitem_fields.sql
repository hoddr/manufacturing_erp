-- additional fields for preliminary work on purchase item updates

BEGIN;

ALTER TABLE purchase_items ADD COLUMN IF NOT EXISTS generic_name VARCHAR(128) UNIQUE;
ALTER TABLE purchase_items ADD COLUMN IF NOT EXISTS autodesk_id VARCHAR(64) UNIQUE;

UPDATE purchase_items SET generic_name = name;

ALTER TABLE purchase_items ALTER COLUMN generic_name SET NOT NULL;

COMMIT;
