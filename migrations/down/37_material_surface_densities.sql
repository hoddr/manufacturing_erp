-- (down) adds surface density (lbs/sq ft) optional for materials

BEGIN;

ALTER TABLE materials DROP COLUMN IF EXISTS surface_density;

COMMIT;
