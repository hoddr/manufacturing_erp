-- permits setting of multiple customers per pricing list (up)

BEGIN;

CREATE TABLE IF NOT EXISTS m_catalog_customers (
  id SERIAL PRIMARY KEY,
  fk_customer_id SERIAL,
  fk_pricing_list_id SERIAL,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id),
  CONSTRAINT fk_pricing_list FOREIGN KEY (fk_pricing_list_id) REFERENCES pricing_lists(id)
);

INSERT INTO m_catalog_customers (fk_customer_id, fk_pricing_list_id)
  SELECT fk_customer_id, id from pricing_lists;

ALTER TABLE pricing_lists DROP COLUMN IF EXISTS fk_customer_id;

COMMIT;
