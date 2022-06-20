-- update to remove note field to purchase order item

ALTER TABLE m_items_purchase_order DROP COLUMN IF NOT EXISTS note;
