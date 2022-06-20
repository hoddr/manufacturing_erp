-- assemblies rework/overhaul (down)

BEGIN;

DROP TABLE IF EXISTS m_assembly_subitems;
DROP TABLE IF EXISTS assemblies;

CREATE TABLE IF NOT EXISTS assemblies (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  third_party_name VARCHAR(128) NOT NULL UNIQUE,
  add_labor REAL NOT NULL DEFAULT 0.0
);

CREATE TABLE IF NOT EXISTS m_assembly_items (
  id SERIAL PRIMARY KEY,
  fk_assembly_id SERIAL,
  item_ref_id SERIAL NOT NULL,
  quantity REAL NOT NULL DEFAULT 0.0,
  item_type VARCHAR(16) NOT NULL,
  CONSTRAINT fk_assembly FOREIGN KEY (fk_assembly_id) REFERENCES assemblies(id)
);

COMMIT;
