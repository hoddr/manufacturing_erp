-- migration (up) for pricing lists/catalogue support

BEGIN;

CREATE TABLE IF NOT EXISTS pricing_lists (
  id SERIAL PRIMARY KEY,
  description VARCHAR(256) NOT NULL,
  fk_customer_id SERIAL,
  effective_as_of TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  effective_until TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id)
);

CREATE TABLE IF NOT EXISTS m_pricing_list_items (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL,
  reference_name VARCHAR(128) NOT NULL,
  reference_description VARCHAR(128) NOT NULL,
  fixed_price REAL NOT NULL,
  fk_pricing_list_id SERIAL,
  CONSTRAINT fk_pricing_list FOREIGN KEY (fk_pricing_list_id) REFERENCES pricing_lists(id)
);

COMMIT;
