-- migration (up) for adding line item piece length for project tracking

BEGIN;

ALTER TABLE line_items ADD COLUMN IF NOT EXISTS piece_length REAL NOT NULL DEFAULT 0.00;

COMMIT;
