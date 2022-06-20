-- adds surface density (lbs/sq ft) optional for materials

BEGIN;

ALTER TABLE materials ADD COLUMN IF NOT EXISTS surface_density REAL;

COMMIT;
