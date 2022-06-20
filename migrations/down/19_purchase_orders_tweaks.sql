-- purchase orders, purchase order items, modifications to purchase items and mats for po support (down)

BEGIN;

ALTER TABLE m_user_permissions DROP COLUMN IF EXISTS purchase_orders;

DROP TABLE IF EXISTS m_items_purchase_order;
DROP TABLE IF EXISTS purchase_orders;

ALTER TABLE fab_items DROP COLUMN IF EXISTS balance_category;

ALTER TABLE materials DROP COLUMN IF EXISTS is_locked;
ALTER TABLE materials DROP COLUMN IF EXISTS balance_category;

ALTER TABLE purchase_items DROP COLUMN IF EXISTS is_locked;
ALTER TABLE purchase_items DROP COLUMN IF EXISTS balance_category;

COMMIT;
