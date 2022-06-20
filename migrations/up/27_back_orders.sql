-- up migration to help support back orders for purchasing

ALTER TABLE purchase_orders ADD COLUMN is_back_order BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE purchase_orders ADD COLUMN po_number VARCHAR(12);

UPDATE purchase_orders SET po_number = id::TEXT;

ALTER TABLE purchase_orders ALTER COLUMN po_number SET NOT NULL;
ALTER TABLE purchase_orders ADD CONSTRAINT unique_po UNIQUE ( po_number );
