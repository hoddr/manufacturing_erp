-- migration to add markup/labor rate tracking to quotes to permit detailed sales data reports on quotes

BEGIN;

ALTER TABLE quotes ADD COLUMN IF NOT EXISTS fk_labor_id SERIAL;
UPDATE quotes SET fk_labor_id = 3; -- current latest value
ALTER TABLE quotes ADD CONSTRAINT fk_labor FOREIGN KEY (fk_labor_id) REFERENCES labor_rate(id);

ALTER TABLE quotes ADD COLUMN IF NOT EXISTS fk_markups_id SERIAL;
UPDATE quotes set fk_markups_id = 8; -- current latest value
ALTER TABLE quotes ADD CONSTRAINT fk_markups FOREIGN KEY (fk_markups_id) REFERENCES markups(id);

ALTER TABLE quotes ADD COLUMN IF NOT EXISTS customer_markup REAL DEFAULT 1.00;

COMMIT;
