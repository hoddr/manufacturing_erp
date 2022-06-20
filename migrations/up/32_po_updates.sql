-- purchase order updates
-- adds multi-step process by date
-- 0) gen in ERP 1) to QB 2) sent 3a) receipt 3b) price verification 4) price adjustments 5) close

BEGIN;

ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS in_qb_dt TIMESTAMP WITH TIME ZONE;
ALTER TABLE purchase_orders RENAME COLUMN order_date TO sent_dt;
ALTER TABLE purchase_orders RENAME COLUMN actual_receipt_date TO receipt_dt;
ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS is_price_verified BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS price_verify_dt TIMESTAMP WITH TIME ZONE;
ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS is_price_adjusted BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS price_adjust_dt TIMESTAMP WITH TIME ZONE;
ALTER TABLE purchase_orders ADD COLUMN IF NOT EXISTS close_dt TIMESTAMP WITH TIME ZONE;

UPDATE purchase_orders SET in_qb_dt = CURRENT_TIMESTAMP WHERE is_order_in_qb = TRUE;
UPDATE purchase_orders SET sent_dt = CURRENT_TIMESTAMP WHERE is_order_sent = TRUE;
UPDATE purchase_orders SET price_verify_dt = receipt_dt, price_adjust_dt = receipt_dt WHERE is_order_received = TRUE;
UPDATE purchase_orders SET close_dt = receipt_dt WHERE is_order_closed = TRUE;
UPDATE purchase_orders SET is_price_verified = is_order_closed, is_price_adjusted = is_order_closed;

COMMIT;
