-- sql for adding in mat purchases

BEGIN;

CREATE TABLE IF NOT EXISTS mat_purchases (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(128) NOT NULL,
  units_per_quantity REAL NOT NULL DEFAULT 1.0,
  fk_material_id SERIAL,
  CONSTRAINT fk_material FOREIGN KEY (fk_material_id) REFERENCES materials(id)
);

COMMIT;
