-- removes material quantity field (down)

BEGIN;

ALTER TABLE materials ADD COLUMN unit_per_quantity REAL DEFAULT 1.0;

COMMIT;
