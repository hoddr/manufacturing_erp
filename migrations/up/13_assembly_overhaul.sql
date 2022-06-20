-- assemblies rework/overhaul (up)

BEGIN;

DROP TABLE IF EXISTS m_assembly_items;
DROP TABLE IF EXISTS assemblies;

CREATE TABLE IF NOT EXISTS assemblies (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(128) NOT NULL,
  labor REAL NOT NULL DEFAULT 0.0
);

CREATE TABLE IF NOT EXISTS m_assembly_subitems (
  id SERIAL PRIMARY KEY,
  fk_assembly_id SERIAL,
  fk_inventory_id SERIAL,
  quantity REAL NOT NULL DEFAULT 0.00,
  CONSTRAINT fk_inventory FOREIGN KEY (fk_inventory_id) REFERENCES inventory(id),
  CONSTRAINT fk_assembly FOREIGN KEY (fk_assembly_id) REFERENCES assemblies(id)
);

COMMIT;
