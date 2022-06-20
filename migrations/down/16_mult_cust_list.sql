-- permits setting of multiple customers per pricing list (down)

BEGIN;

ALTER TABLE pricing_lists ADD COLUMN IF NOT EXISTS fk_customer_id SERIAL;

UPDATE pricing_lists SET fk_customer_id = 0; -- placeholder

ALTER TABLE pricing_lists ADD CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id);

DROP TABLE IF EXISTS m_catalog_customers;

COMMIT;
