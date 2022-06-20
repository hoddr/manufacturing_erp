-- (down) additional fields for preliminary work on purchase item updates

BEGIN;

ALTER TABLE purchase_items DROP COLUMN IF EXISTS generic_name;
ALTER TABLE purchase_items DROP COLUMN IF EXISTS autodesk_id;

COMMIT;
