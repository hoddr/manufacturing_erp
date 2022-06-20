-- add lead time field for purchase order items (purchases and mat purchases)

BEGIN;

ALTER TABLE purchase_items ADD COLUMN IF NOT EXISTS lead_time VARCHAR(24);
ALTER TABLE mat_purchases ADD COLUMN IF NOT EXISTS lead_time VARCHAR(24);
ALTER TABLE m_items_purchase_order ADD COLUMN IF NOT EXISTS lead_time VARCHAR(24);

COMMIT;
