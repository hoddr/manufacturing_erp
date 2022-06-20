-- purchase orders, purchase order items, modifications to purchase items and mats for po support (up)

BEGIN;

ALTER TABLE purchase_items ADD COLUMN IF NOT EXISTS balance_category VARCHAR(64) NOT NULL DEFAULT 'TBD';
ALTER TABLE purchase_items ADD COLUMN IF NOT EXISTS is_locked BOOLEAN NOT NULL DEFAULT TRUE;

ALTER TABLE materials ADD COLUMN IF NOT EXISTS balance_category VARCHAR(64) NOT NULL DEFAULT 'Steel';
ALTER TABLE materials ADD COLUMN IF NOT EXISTS is_locked BOOLEAN NOT NULL DEFAULT TRUE;

ALTER TABLE fab_items ADD COLUMN IF NOT EXISTS balance_category VARCHAR(64) NOT NULL DEFAULT 'TBD';

CREATE TABLE IF NOT EXISTS purchase_orders (
  id SERIAL PRIMARY KEY,
  fk_vendor_id SERIAL,
  memo VARCHAR(512),
  order_date TIMESTAMP WITH TIME ZONE,
  expected_receipt_date TIMESTAMP WITH TIME ZONE,
  actual_receipt_date TIMESTAMP WITH TIME ZONE,
  is_order_in_qb BOOLEAN NOT NULL DEFAULT FALSE,
  is_order_sent BOOLEAN NOT NULL DEFAULT FALSE,
  is_order_received BOOLEAN NOT NULL DEFAULT FALSE,
  is_order_closed BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT fk_vendor FOREIGN KEY (fk_vendor_id) REFERENCES vendors(id)
);

CREATE TABLE IF NOT EXISTS m_items_purchase_order (
  id SERIAL PRIMARY KEY,
  fk_purchase_order_id SERIAL,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(24) NOT NULL,
  description VARCHAR(128),
  quantity REAL NOT NULL,
  units_per_quantity REAL DEFAULT 1.00,
  purchase_price_each REAL NOT NULL,
  balance_category VARCHAR(64) NOT NULL,
  CONSTRAINT fk_purchase_order FOREIGN KEY (fk_purchase_order_id) REFERENCES purchase_orders(id)
);

ALTER TABLE m_user_permissions ADD COLUMN IF NOT EXISTS purchase_orders SMALLINT NOT NULL DEFAULT 0;

-- turn on AFTER main db is updated with this migration
-- for testing only
-- UPDATE m_user_permissions SET
--   assemblies = 3,
--   customers = 3,
--   fabrication = 3,
--   feedback_complaints = 3,
--   inventory = 3,
--   materials = 3,
--   orders = 3,
--   pricing = 3,
--   pricing_lists = 3,
--   projects = 3,
--   purchase = 3,
--   quotes = 3,
--   users = 3,
--   vendors = 3,
--   purchase_orders = 3
-- WHERE fk_user_id = 2;

WITH ta AS (
  INSERT INTO purchase_items (
    name,
    description,
    preferred_vendor,
    cost,
    vendor_part_number,
    vendor_category,
    markup,
    balance_category,
    is_locked,
    fk_vendor_id
  ) values ('Generic Purchase', 'TBD', '_default', 0.00, '', '', 1.00, 'Purchase', false, 0)
  returning id
) insert into inventory (
  reference_id,
  reference_type,
  on_hand,
  on_order,
  min_on_hand
) values ((select id from ta), 'Purchase', 0, 0, 0);

WITH ta AS (
  INSERT INTO materials (
    name,
    description,
    unit,
    cost_per_unit,
    unit_per_quantity,
    preferred_vendor,
    balance_category,
    is_locked
  ) values ('Generic Material', 'TBD', 'TBD', 0.00, 1, '', 'TBD', false)
  returning id
) insert into inventory (
  reference_id,
  reference_type,
  on_hand,
  on_order,
  min_on_hand
) values ((select id from ta), 'Material', 0, 0, 0);

COMMIT;
