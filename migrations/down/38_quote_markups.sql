-- (down) migration to add markup/labor rate tracking to quotes to permit detailed sales data reports on quotes

BEGIN;

ALTER TABLE quotes DROP COLUMN IF EXISTS fk_labor_id;
ALTER TABLE quotes DROP COLUMN IF EXISTS fk_markups_id;
ALTER TABLE quotes DROP COLUMN IF EXISTS customer_markup;

COMMIT;
