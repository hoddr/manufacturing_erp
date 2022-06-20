-- support for cam data changes (down)

BEGIN;

ALTER TABLE line_items DROP COLUMN labor;
ALTER TABLE line_items DROP COLUMN accessory_cost;
ALTER TABLE line_items DROP COLUMN skin_weight;
ALTER TABLE line_items DROP COLUMN skin_name;
ALTER TABLE line_items DROP COLUMN skin_id;

ALTER TABLE line_items DROP COLUMN material_weight;
ALTER TABLE line_items RENAME COLUMN liner_area to liner_weight;

ALTER TABLE orders ADD COLUMN is_ship_kd BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE quotes ADD COLUMN is_ship_kd BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
