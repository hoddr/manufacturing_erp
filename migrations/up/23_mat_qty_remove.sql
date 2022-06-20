-- removes material quantity field (up)

BEGIN;

ALTER TABLE materials DROP COLUMN unit_per_quantity;

COMMIT;
