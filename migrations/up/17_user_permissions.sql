-- user (granular) permissions

BEGIN;

CREATE TABLE IF NOT EXISTS m_user_permissions (
  id SERIAL PRIMARY KEY,
  fk_user_id SERIAL,
  assemblies SMALLINT NOT NULL DEFAULT 0,
  customers SMALLINT NOT NULL DEFAULT 0,
  fabrication SMALLINT NOT NULL DEFAULT 0,
  feedback_complaints SMALLINT NOT NULL DEFAULT 0,
  inventory SMALLINT NOT NULL DEFAULT 0,
  materials SMALLINT NOT NULL DEFAULT 0,
  orders SMALLINT NOT NULL DEFAULT 0,
  pricing SMALLINT NOT NULL DEFAULT 0,
  pricing_lists SMALLINT NOT NULL DEFAULT 0,
  projects SMALLINT NOT NULL DEFAULT 0,
  purchase SMALLINT NOT NULL DEFAULT 0,
  quotes SMALLINT NOT NULL DEFAULT 0,
  users SMALLINT NOT NULL DEFAULT 0,
  vendors SMALLINT NOT NULL DEFAULT 0,
  CONSTRAINT fk_user FOREIGN KEY (fk_user_id) REFERENCES users(id)
);

INSERT INTO m_user_permissions (fk_user_id) SELECT users.id FROM users;

-- this should only modify the script user (for tests mainly)
-- main db is already tweaked and will not re-run this migration
UPDATE m_user_permissions SET
  assemblies = 3,
  customers = 3,
  fabrication = 3,
  feedback_complaints = 3,
  inventory = 3,
  materials = 3,
  orders = 3,
  pricing = 3,
  pricing_lists = 3,
  projects = 3,
  purchase = 3,
  quotes = 3,
  users = 3,
  vendors = 3
WHERE fk_user_id = 2;

COMMIT;
