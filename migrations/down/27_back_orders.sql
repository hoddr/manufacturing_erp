-- down migration to help support back orders for purchasing

ALTER TABLE purchase_orders DROP COLUMN po_number;
ALTER TABLE purchase_orders DROP COLUMN is_back_order;
