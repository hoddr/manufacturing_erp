-- adds table for error log

BEGIN;

CREATE TABLE IF NOT EXISTS error_log (
  id SERIAL PRIMARY KEY,
  fk_customer_id SERIAL,
  entry_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  occurrence_date TIMESTAMP WITH TIME ZONE NOT NULL,
  order_number VARCHAR(6) NOT NULL DEFAULT 'TBD',
  code VARCHAR(24) NOT NULL,
  issue_note VARCHAR(512) NOT NULL,
  resolution_note VARCHAR(512) NOT NULL,
  root_cause VARCHAR(512) NOT NULL,
  assigned_to VARCHAR(24) NOT NULL,
  fk_user_id SERIAL,
  is_resolved BOOLEAN NOT NULL DEFAULT FALSE,
  is_recorded_in_90 BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT fk_user FOREIGN KEY (fk_user_id) REFERENCES users(id),
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id)
);

ALTER TABLE m_user_permissions ADD COLUMN IF NOT EXISTS error_log SMALLINT NOT NULL DEFAULT 0;

COMMIT;
