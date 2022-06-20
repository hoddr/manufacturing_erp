BEGIN;

ALTER TABLE m_project_orders DROP CONSTRAINT fk_section;
ALTER TABLE m_project_orders DROP COLUMN fk_section_id;
DROP TABLE IF EXISTS project_sections;

COMMIT;
