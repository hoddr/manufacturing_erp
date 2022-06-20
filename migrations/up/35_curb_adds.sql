-- curb additions
-- adds lookup table for customer type and markups/discounts
-- leverages customer type for that field
-- adds curb size field (S = small, M = medium, L = large)

BEGIN;

ALTER TABLE m_curbs_quote ADD COLUMN IF NOT EXISTS curb_size VARCHAR(1) NOT NULL DEFAULT 'S';

ALTER TABLE customers ALTER COLUMN customer_type TYPE VARCHAR(26);

UPDATE customers set customer_type = 'Contractor';

CREATE TABLE IF NOT EXISTS lu_curb_customer_types (
  id SERIAL PRIMARY KEY,
  type_name VARCHAR(26) NOT NULL DEFAULT 'Contractor',
  markup REAL NOT NULL
);

INSERT INTO lu_curb_customer_types (type_name, markup) VALUES
  ('Member List Price', 1.00),
  ('Contractor', 0.95),
  ('Preferred Contractor', 0.90),
  ('Exclusive Contractor', 0.75),
  ('Distributor', 0.65),
  ('Preferred Distributor', 0.60),
  ('Exclusive Distributor', 0.55),
  ('Managed Services Provider', 0.60);

COMMIT;
