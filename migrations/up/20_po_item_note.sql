-- update to add note field to purchase order item

ALTER TABLE m_items_purchase_order ADD COLUMN IF NOT EXISTS note VARCHAR(128) NOT NULL DEFAULT '';
