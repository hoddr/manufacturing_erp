-- support for order completion flag (down)

BEGIN;

ALTER TABLE orders DROP COLUMN IF EXISTS is_complete;

COMMIT;
