-- removes warm air stock category

BEGIN;

DELETE FROM categories WHERE name = 'WARM AIR STOCK';

COMMIT;
