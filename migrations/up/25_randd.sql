-- adds fields to error log/feedback complaints for r&d denotion

BEGIN;

ALTER TABLE feedback_complaints ADD COLUMN IF NOT EXISTS randd_cost REAL;
ALTER TABLE feedback_complaints ADD COLUMN IF NOT EXISTS is_randd BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE error_log ADD COLUMN IF NOT EXISTS randd_cost REAL;
ALTER TABLE error_log ADD COLUMN IF NOT EXISTS is_randd BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
