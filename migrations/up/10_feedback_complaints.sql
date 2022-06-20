-- adds table for feedback complaints

BEGIN;

CREATE TABLE IF NOT EXISTS feedback_complaints (
  id SERIAL PRIMARY KEY,
  fk_customer_id SERIAL,
  order_number VARCHAR(6) NOT NULL DEFAULT 'TBD',
  entry_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  occurrence_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  issue_note VARCHAR(512) NOT NULL,
  resolution_note VARCHAR(512) NOT NULL,
  is_resolved BOOLEAN NOT NULL DEFAULT FALSE,
  is_app_updated BOOLEAN NOT NULL DEFAULT FALSE,
  is_recorded_in_90 BOOLEAN NOT NULL DEFAULT FALSE,
  fk_user_id SERIAL,
  CONSTRAINT fk_customer FOREIGN KEY (fk_customer_id) REFERENCES customers(id),
  CONSTRAINT fk_user FOREIGN KEY (fk_user_id) REFERENCES users(id)
);

COMMIT;
