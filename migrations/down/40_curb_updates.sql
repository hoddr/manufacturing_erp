-- adds curb co additions for metal/material weight, gasket ft, fast pass charge

BEGIN;

ALTER TABLE m_curbs_quote DROP COLUMN IF EXISTS gasket_feet;
ALTER TABLE m_curbs_quote DROP COLUMN IF EXISTS fk_metal_id; -- GALV 18
ALTER TABLE m_curbs_quote DROP COLUMN IF EXISTS metal_weight;
ALTER TABLE curb_quotes DROP COLUMN IF EXISTS fast_pass_charge;

DROP TABLE IF EXISTS m_curb_quote_orders;

COMMIT;
