-- down migration for curb co additions

BEGIN;

ALTER TABLE m_curbs_quote DROP COLUMN IF EXISTS curb_size;

ALTER TABLE customers ALTER COLUMN customer_type TYPE VARCHAR(16); -- cannot return previous settings...

DROP TABLE IF EXISTS lu_curb_customer_types;

COMMIT;
