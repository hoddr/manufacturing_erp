-- migration (up) for categories for extras filtering

BEGIN;

CREATE TABLE IF NOT EXISTS categories (
  id SERIAL PRIMARY KEY,
  name VARCHAR(64) NOT NULL UNIQUE
);

INSERT INTO categories (name) VALUES
  ('RECTANGULAR DUCT'),
  ('ROUND DUCT'),
  ('OVAL DUCT'),
  ('MISC RECTANGULAR'),
  ('MISC ROUND OVAL'),
  ('STOCK PULL'),
  ('PURCHASED'),
  ('DUCT ACCESSORY'),
  ('SW RECTANGULAR'),
  ('DW RECTANGULAR'),
  ('SW ROUND'),
  ('DW ROUND'),
  ('SW OVAL'),
  ('DW OVAL');

COMMIT;
