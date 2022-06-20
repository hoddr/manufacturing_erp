-- adds address table (singular table!)

BEGIN;

CREATE TABLE IF NOT EXISTS address_book (
  id SERIAL PRIMARY KEY,
  street VARCHAR(256) NOT NULL,
  street2 VARCHAR(128),
  city VARCHAR(32) NOT NULL,
  state VARCHAR(2) NOT NULL,
  zip VARCHAR(10) NOT NULL
);

CREATE TABLE IF NOT EXISTS m_address_obj (
  id SERIAL PRIMARY KEY,
  ref_id INT NOT NULL,
  ref_type VARCHAR(1) NOT NULL, -- C = cust, V = vend, S = ship, P = proj, T = curb co ship
  fk_address_book_id SERIAL,
  UNIQUE (ref_id, ref_type),
  CONSTRAINT fk_address FOREIGN KEY (fk_address_book_id) REFERENCES address_book(id)
);

COMMIT;
