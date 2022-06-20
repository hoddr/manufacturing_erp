-- adding in curb quoting support (billing too)
-- pid -> quote # prefixed by AT-
-- shipping address
-- customer
-- new unit/top
-- old unit/bottom
-- adapter
-- price each
-- quantity
-- shipping costs
-- eventually material support
-- quote validity time?

BEGIN;

ALTER TABLE m_user_permissions ADD COLUMN IF NOT EXISTS curb SMALLINT NOT NULL DEFAULT 0;

CREATE TABLE IF NOT EXISTS curb_quotes (
  id SERIAL PRIMARY KEY,
  anytime_pid VARCHAR(12) NOT NULL UNIQUE,
  fk_customer_id SERIAL,
  is_confirmed BOOLEAN NOT NULL DEFAULT FALSE,
  shipping_costs REAL NOT NULL DEFAULT 0.00,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id)
);

CREATE TABLE IF NOT EXISTS m_curbs_quote (
  id SERIAL PRIMARY KEY,
  fk_curb_quote_id SERIAL,
  old_unit VARCHAR(128) NOT NULL,
  new_unit VARCHAR(128) NOT NULL,
  adapter VARCHAR(128) NOT NULL,
  price_each REAL NOT NULL,
  quantity REAL NOT NULL,
  CONSTRAINT fk_curb_quote FOREIGN KEY (fk_curb_quote_id) REFERENCES curb_quotes(id)
);

COMMIT;
