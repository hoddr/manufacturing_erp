-- migration for users and tokens

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  fname VARCHAR(32) NOT NULL,
  lname VARCHAR(32) NOT NULL,
  username VARCHAR(16) NOT NULL UNIQUE,
  password VARCHAR(128) NOT NULL,
  role VARCHAR(16) NOT NULL
);

INSERT INTO users (fname, lname, username, password, role)
  VALUES ('D', 'H', 'dh', '$2b$12$9PJKQMjh28gLWWMeCtbsaenwm0SsO1SI.W/NeN8WRAi3LAWtfKE6a', 'ADMIN'),
         ('ERP', 'Scripts', 'erpscripts', '$2b$12$9PJKQMjh28gLWWMeCtbsaenwm0SsO1SI.W/NeN8WRAi3LAWtfKE6a', 'OFFICE');

CREATE TABLE IF NOT EXISTS tokens (
  id SERIAL PRIMARY KEY,
  fk_user_id SERIAL,
  token UUID UNIQUE DEFAULT uuid_generate_v4(),
  expires TIMESTAMP WITH TIME ZONE,
  is_one_time BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT fk_user_id FOREIGN KEY (fk_user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- base entry for scripts authentication
INSERT INTO tokens (fk_user_id, token) VALUES (2, 'fcd314b1-4f80-41c3-bef0-fefa28d9bb9f');

COMMIT;
