-- support for order completion flag (up)

BEGIN;

ALTER TABLE orders ADD COLUMN IF NOT EXISTS is_complete BOOLEAN NOT NULL DEFAULT TRUE;

COMMIT;
