-- migration to drop additional fields to line items/orders for necessary accounting,
-- tracking, cost analysis, margin analysis, etc

BEGIN;

ALTER TABLE line_items DROP COLUMN labor_cost;
ALTER TABLE line_items DROP COLUMN skin_cost;
ALTER TABLE line_items DROP COLUMN liner_cost;
ALTER TABLE line_items DROP COLUMN material_cost;

ALTER TABLE orders DROP COLUMN customer_markup;
ALTER TABLE orders DROP COLUMN fk_markups_id;
ALTER TABLE orders DROP COLUMN fk_labor_id;

COMMIT;
