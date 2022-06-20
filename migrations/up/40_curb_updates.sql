-- adds curb co additions for metal/material weight, gasket ft, fast pass charge

BEGIN;

ALTER TABLE m_curbs_quote ADD COLUMN IF NOT EXISTS gasket_feet REAL NOT NULL DEFAULT 0.00;
ALTER TABLE m_curbs_quote ADD COLUMN IF NOT EXISTS fk_metal_id INT NOT NULL DEFAULT 19; -- GALV 18
ALTER TABLE m_curbs_quote ADD COLUMN IF NOT EXISTS metal_weight REAL NOT NULL DEFAULT 0.00;
ALTER TABLE curb_quotes ADD COLUMN IF NOT EXISTS fast_pass_charge REAL NOT NULL DEFAULT 0.00;

ALTER TABLE m_curbs_quote ADD CONSTRAINT fk_metal FOREIGN KEY (fk_metal_id) REFERENCES materials(id);

CREATE TABLE IF NOT EXISTS m_curb_quote_orders (
  id SERIAL PRIMARY KEY,
  fk_curb_quote_id SERIAL,
  fk_order_id SERIAL,
  CONSTRAINT fk_order FOREIGN KEY (fk_order_id) REFERENCES orders(id),
  CONSTRAINT fk_curb_quote FOREIGN KEY (fk_curb_quote_id) REFERENCES curb_quotes(id)
);

COMMIT;
