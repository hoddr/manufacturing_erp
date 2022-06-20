-- migration to add additional fields to line items/orders for necessary accounting,
-- tracking, cost analysis, margin analysis, etc

BEGIN;

ALTER TABLE orders ADD COLUMN fk_labor_id SERIAL;
UPDATE orders SET fk_labor_id = 3; -- current latest value
ALTER TABLE orders ADD CONSTRAINT fk_labor FOREIGN KEY (fk_labor_id) REFERENCES labor_rate(id);

ALTER TABLE orders ADD COLUMN fk_markups_id SERIAL;
UPDATE orders set fk_markups_id = 8; -- current latest value
ALTER TABLE orders ADD CONSTRAINT fk_markups FOREIGN KEY (fk_markups_id) REFERENCES markups(id);

ALTER TABLE orders ADD COLUMN customer_markup REAL DEFAULT 1.00;

ALTER TABLE line_items ADD COLUMN material_cost REAL DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN liner_cost REAL DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN skin_cost REAL DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN labor_cost REAL DEFAULT 0.00;

COMMIT;
