-- migration for project billing prototype

BEGIN;

CREATE TABLE IF NOT EXISTS customers (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  company VARCHAR(128),
  customer_type VARCHAR(16)
);

-- placeholder for inserting default fk_customer_ids if needed
INSERT INTO customers (id, name, company, customer_type) values (0, '_default', 'FOOBAR', 'n/a');

CREATE TABLE IF NOT EXISTS vendors (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  company VARCHAR(128)
);

-- placeholder for inserting default fk_vendor_ids if needed
INSERT INTO vendors (id, name, company) values (0, '_default', 'FOOBAR');

-- 'm' for minutes
-- 'h' for hours
CREATE TABLE IF NOT EXISTS labor_rate (
  id SERIAL PRIMARY KEY,
  shop_rate REAL NOT NULL DEFAULT 0.0,
  overhead_rate REAL NOT NULL DEFAULT 0.0,
  rate REAL GENERATED ALWAYS AS (shop_rate + overhead_rate) STORED,
  is_current BOOLEAN NOT NULL DEFAULT TRUE,
  base_unit VARCHAR(1) NOT NULL DEFAULT 'h',
  added_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- set default labor rate
UPDATE labor_rate SET is_current = FALSE;
INSERT INTO labor_rate (shop_rate, overhead_rate, is_current) VALUES (18.0, 32.0, TRUE);

-- acts as mapping from inventory to one of three items/mats
CREATE TABLE IF NOT EXISTS inventory (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL, -- material, fabrication, purchase
  on_hand REAL NOT NULL DEFAULT 0,
  on_order REAL NOT NULL DEFAULT 0,
  min_on_hand REAL NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS materials (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(128) NOT NULL,
  unit VARCHAR(16) NOT NULL,
  cost_per_unit REAL NOT NULL,
  unit_per_quantity REAL,
  preferred_vendor VARCHAR(128) NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS fab_items (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(128) NOT NULL,
  unit_quantity REAL NOT NULL DEFAULT 0.0,
  labor REAL NOT NULL DEFAULT 0.0,
  fk_material_id SERIAL,
  is_stock BOOLEAN NOT NULL DEFAULT TRUE,
  CONSTRAINT fk_material FOREIGN KEY (fk_material_id) REFERENCES materials(id)
);

CREATE TABLE IF NOT EXISTS purchase_items (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(128) NOT NULL,
  -- price REAL NOT NULL DEFAULT 0.0,
  preferred_vendor VARCHAR(128) NOT NULL DEFAULT '',
  cost REAL NOT NULL DEFAULT 0.0,
  vendor_part_number VARCHAR(36),
  vendor_category VARCHAR(128),
  markup REAL NOT NULL DEFAULT 1.00,
  fk_vendor_id SERIAL, -- intends to track LAST vendor purchased
  CONSTRAINT fk_vendor FOREIGN KEY (fk_vendor_id) REFERENCES vendors(id)
);

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

CREATE TABLE IF NOT EXISTS quotes (
  id SERIAL PRIMARY KEY,
  quote_type VARCHAR(16) NOT NULL,
  created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  quote_number VARCHAR(36) NOT NULL UNIQUE,
  order_id INTEGER, -- also indicates that a quote is "won" or "accepted"
  project_id INTEGER, -- also indicates that a project is "won" or "accepted"
  set_quote_price REAL,
  fk_customer_id SERIAL,
  is_wrapped BOOLEAN NOT NULL DEFAULT FALSE,
  is_residential_so BOOLEAN NOT NULL DEFAULT FALSE,
  is_ship_kd BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id)
);

CREATE TABLE IF NOT EXISTS projects (
  id SERIAL PRIMARY KEY,
  app_id INTEGER NOT NULL UNIQUE,
  name VARCHAR(64) NOT NULL UNIQUE,
  po VARCHAR(32) NOT NULL,
  total_estimated_weight REAL NOT NULL DEFAULT 0.0,
  fk_customer_id SERIAL,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  fk_quote_id SERIAL,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id),
  CONSTRAINT fk_quote FOREIGN KEY (fk_quote_id) REFERENCES quotes(id)
);

CREATE TABLE IF NOT EXISTS m_project_extras (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL, -- material, fabrication, purchase
  reference_name VARCHAR(128) NOT NULL,
  fk_project_id SERIAL,
  CONSTRAINT fk_project FOREIGN KEY (fk_project_id) REFERENCES projects(id)
);

CREATE TABLE IF NOT EXISTS orders (
  id SERIAL PRIMARY KEY,
  order_type VARCHAR(16) NOT NULL,
  po VARCHAR(32) NOT NULL,
  created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  order_number VARCHAR(6) NOT NULL UNIQUE,
  fk_customer_id SERIAL,
  is_wrapped BOOLEAN NOT NULL DEFAULT FALSE,
  is_residential_so BOOLEAN NOT NULL DEFAULT FALSE,
  is_ship_kd BOOLEAN NOT NULL DEFAULT FALSE,
  is_billed BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id)
);

CREATE TABLE IF NOT EXISTS line_items (
  id SERIAL PRIMARY KEY,
  description VARCHAR(256) NOT NULL,
  quantity REAL NOT NULL DEFAULT 0.0,
  weight REAL,
  is_extra BOOLEAN NOT NULL DEFAULT FALSE,
  price REAL NOT NULL DEFAULT 0.00,
  order_id SERIAL,
  quote_id SERIAL,
  material_id SMALLINT,
  material_name VARCHAR(128),
  liner_id SMALLINT,
  liner_name VARCHAR(128),
  liner_weight REAL,
  category VARCHAR(32) NOT NULL
);

CREATE TABLE IF NOT EXISTS m_project_orders (
  id SERIAL PRIMARY KEY,
  fk_project_id SERIAL,
  fk_order_id SERIAL,
  CONSTRAINT fk_project FOREIGN KEY (fk_project_id) REFERENCES projects(id),
  CONSTRAINT fk_order FOREIGN KEY (fk_order_id) REFERENCES orders(id)
);

CREATE TABLE IF NOT EXISTS default_extras (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL, -- material, fabrication, purchase
  reference_name VARCHAR(128) NOT NULL,
  fk_project_id INT DEFAULT 0
);

COMMIT;
