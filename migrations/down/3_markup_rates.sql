-- migration (down) for markup rates

BEGIN;

DROP TABLE IF EXISTS markups;

COMMIT;
