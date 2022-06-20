-- UNDO purchase order updates
-- adds multi-step process by date
-- 0) gen in ERP 1) to QB 2) sent 3a) receipt 3b) price verification 4) price adjustments 5) close

BEGIN;

ALTER TABLE purchase_orders DROP COLUMN IF EXISTS in_qb_dt;
ALTER TABLE purchase_orders RENAME COLUMN sent_dt TO order_date;
ALTER TABLE purchase_orders RENAME COLUMN receipt_dt TO actual_receipt_date;
ALTER TABLE purchase_orders DROP COLUMN IF EXISTS is_price_verified;
ALTER TABLE purchase_orders DROP COLUMN IF EXISTS price_verify_dt;
ALTER TABLE purchase_orders DROP COLUMN IF EXISTS is_price_adjusted;
ALTER TABLE purchase_orders DROP COLUMN IF EXISTS price_adjust_dt;
ALTER TABLE purchase_orders DROP COLUMN IF EXISTS close_dt;

COMMIT;
