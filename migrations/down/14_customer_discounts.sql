-- adding notion of customer discount (down)

BEGIN;

ALTER TABLE customers DROP COLUMN IF EXISTS markup;

COMMIT;
