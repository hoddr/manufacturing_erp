-- adds in column for quote po (nullable)

BEGIN;

ALTER TABLE quotes ADD COLUMN IF NOT EXISTS quote_po VARCHAR(36);

COMMIT;
