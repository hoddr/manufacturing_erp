-- removes table for error log

BEGIN;

ALTER TABLE m_user_permissions DROP COLUMN IF EXISTS error_log;

DROP TABLE IF EXISTS error_log;

COMMIT;
