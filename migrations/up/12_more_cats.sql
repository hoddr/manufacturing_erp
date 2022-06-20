-- migration (up) for additional categories (mainly for extras)

BEGIN;

INSERT INTO categories (name) VALUES
  ('SW LINED RECTANGULAR'),
  ('SW LINED ROUND'),
  ('SW LINED OVAL');

DROP TABLE IF EXISTS default_extras;
DROP TABLE IF EXISTS m_project_extras;

CREATE TABLE IF NOT EXISTS m_project_extras (
  id SERIAL PRIMARY KEY,
  fk_category_id SERIAL,
  name_check VARCHAR(128), -- nullable
  fk_project_id SERIAL,
  CONSTRAINT fk_category FOREIGN KEY (fk_category_id) REFERENCES categories(id),
  CONSTRAINT fk_project FOREIGN KEY (fk_project_id) REFERENCES projects(id)
);

COMMIT;
