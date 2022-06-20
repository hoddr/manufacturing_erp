-- removes curb co changes (support)

BEGIN;

DROP TABLE IF EXISTS m_curbs_quote;
DROP TABLE IF EXISTS curb_quotes;

ALTER TABLE m_user_permissions DROP COLUMN IF EXISTS curb;

COMMIT;
