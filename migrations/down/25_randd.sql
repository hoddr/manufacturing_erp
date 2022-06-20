-- adds fields to error log/feedback complaints for r&d denotion (down)

BEGIN;

ALTER TABLE feedback_complaints DROP COLUMN IF EXISTS randd_cost;
ALTER TABLE feedback_complaints DROP COLUMN IF EXISTS is_randd;

ALTER TABLE error_log DROP COLUMN IF EXISTS randd_cost;
ALTER TABLE error_log DROP COLUMN IF EXISTS is_randd;

COMMIT;
