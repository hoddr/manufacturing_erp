BEGIN;

CREATE TABLE IF NOT EXISTS project_sections (
  id SERIAL PRIMARY KEY,
  fk_project_id SERIAL,
  name VARCHAR(128) NOT NULL,
  section_price REAL NOT NULL DEFAULT 0.0,
  section_weight REAL NOT NULL DEFAULT 0.0,
  CONSTRAINT fk_project FOREIGN KEY (fk_project_id) REFERENCES projects(id)
);

ALTER TABLE m_project_orders ADD COLUMN fk_section_id SERIAL;
ALTER TABLE m_project_orders ADD CONSTRAINT fk_section FOREIGN KEY (fk_section_id) REFERENCES project_sections(id);

COMMIT;
