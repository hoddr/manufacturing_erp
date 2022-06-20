-- drops address table (singular table!)

BEGIN;

DROP TABLE IF EXISTS m_address_obj;
DROP TABLE IF EXISTS address_book;

COMMIT;
