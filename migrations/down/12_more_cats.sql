-- migration (up) for additional categories (mainly for extras)

BEGIN;

DELETE FROM categories WHERE name in
  ('SW LINED RECTANGULAR', 'SW LINED ROUND', 'SW LINED OVAL');

CREATE TABLE IF NOT EXISTS default_extras (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL, -- material, fabrication, purchase
  reference_name VARCHAR(128) NOT NULL,
  fk_project_id INT DEFAULT 0
);

DROP TABLE IF EXISTS m_project_extras;

CREATE TABLE IF NOT EXISTS m_project_extras (
  id SERIAL PRIMARY KEY,
  reference_id SERIAL NOT NULL,
  reference_type VARCHAR(16) NOT NULL, -- material, fabrication, purchase
  reference_name VARCHAR(128) NOT NULL,
  fk_project_id SERIAL,
  CONSTRAINT fk_project FOREIGN KEY (fk_project_id) REFERENCES projects(id)
);

COMMIT;
