-- migration (down) for adding line item piece length for project tracking

BEGIN;

ALTER TABLE line_items DROP COLUMN IF EXISTS piece_length;

COMMIT;
