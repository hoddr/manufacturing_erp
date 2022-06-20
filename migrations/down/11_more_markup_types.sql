-- migration to add custom fab markups for round, rect, oval duct types

BEGIN;

ALTER TABLE markups DROP COLUMN IF EXISTS rectangular_fab;
ALTER TABLE markups DROP COLUMN IF EXISTS round_fab;
ALTER TABLE markups DROP COLUMN IF EXISTS oval_fab;

COMMIT;
