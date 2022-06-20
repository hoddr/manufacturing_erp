-- migration (down) for categories for extras filtering

BEGIN;

DROP TABLE IF EXISTS categories;

COMMIT;
