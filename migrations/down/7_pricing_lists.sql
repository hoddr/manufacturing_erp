-- migration (down) for pricing lists/catalogue support

BEGIN;

DROP TABLE IF EXISTS m_pricing_list_items;
DROP TABLE IF EXISTS pricing_lists;

COMMIT;
