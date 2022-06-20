-- support for cam data changes (liner sq ft not weight, double wall support, material weight not total weight)

BEGIN;

ALTER TABLE orders DROP COLUMN is_ship_kd;
ALTER TABLE quotes DROP COLUMN is_ship_kd;

ALTER TABLE line_items RENAME COLUMN liner_weight to liner_area;
ALTER TABLE line_items ADD COLUMN material_weight REAL DEFAULT 0.00;
UPDATE line_items SET material_weight = weight;

ALTER TABLE line_items ADD COLUMN skin_id SMALLINT DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN skin_name VARCHAR(128) DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN skin_weight REAL DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN accessory_cost REAL DEFAULT 0.00;
ALTER TABLE line_items ADD COLUMN labor REAL DEFAULT 0.00;

COMMIT;
