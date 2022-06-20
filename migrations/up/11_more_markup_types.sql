-- migration to add custom fab markups for round, rect, oval duct types

BEGIN;

ALTER TABLE markups ADD COLUMN IF NOT EXISTS rectangular_fab REAL NOT NULL DEFAULT 1.0;
ALTER TABLE markups ADD COLUMN IF NOT EXISTS round_fab REAL NOT NULL DEFAULT 1.0;
ALTER TABLE markups ADD COLUMN IF NOT EXISTS oval_fab REAL NOT NULL DEFAULT 1.0;

COMMIT;
