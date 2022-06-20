-- ensures that project orders can not have duplicate entries (down)

BEGIN;

ALTER TABLE m_project_orders DROP CONSTRAINT unique_order_map;

COMMIT;
