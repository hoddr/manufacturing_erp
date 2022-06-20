-- ensures that project orders can not have duplicate entries

BEGIN;

ALTER TABLE m_project_orders ADD CONSTRAINT unique_order_map UNIQUE(fk_order_id);

COMMIT;
