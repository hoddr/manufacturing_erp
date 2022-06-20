-- migration for markup rates

BEGIN;

CREATE TABLE IF NOT EXISTS markups (
  id SERIAL PRIMARY KEY,
  purchase REAL NOT NULL DEFAULT 1.0,
  fabrication REAL NOT NULL DEFAULT 1.0,
  stock REAL NOT NULL DEFAULT 1.0,
  assembly REAL NOT NULL DEFAULT 1.0,
  material REAL NOT NULL DEFAULT 1.0,
  is_current BOOLEAN NOT NULL DEFAULT TRUE,
  added_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO markups (id) VALUES (0);

COMMIT;