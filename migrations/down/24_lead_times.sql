-- add lead time field for purchase order items (purchases and mat purchases)
-- down

BEGIN;

ALTER TABLE purchase_items DROP COLUMN IF EXISTS lead_time;
ALTER TABLE mat_purchases DROP COLUMN IF EXISTS lead_time;
ALTER TABLE m_items_purchase_order DROP COLUMN IF EXISTS lead_time;

COMMIT;
