-- adds in column for quote po (nullable)

BEGIN;

ALTER TABLE quotes DROP COLUMN IF EXISTS quote_po;

COMMIT;
