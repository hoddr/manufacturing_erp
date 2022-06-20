-- migration for project billing prototype

BEGIN;

DROP TABLE IF EXISTS default_extras;
DROP TABLE IF EXISTS m_project_orders;
DROP TABLE IF EXISTS m_project_extras;
DROP TABLE IF EXISTS projects;
DROP TABLE IF EXISTS quotes;
DROP TABLE IF EXISTS line_items;
DROP TABLE IF EXISTS orders;
DROP TABLE IF EXISTS m_assembly_items;
DROP TABLE IF EXISTS assemblies;
DROP TABLE IF EXISTS purchase_items;
DROP TABLE IF EXISTS fab_items;
DROP TABLE IF EXISTS materials;
DROP TABLE IF EXISTS inventory;
DROP TABLE IF EXISTS labor_rate;
DROP TABLE IF EXISTS vendors;
DROP TABLE IF EXISTS customers;

COMMIT;
