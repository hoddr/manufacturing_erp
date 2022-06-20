-- down migration for users and tokens

BEGIN;

DROP TABLE IF EXISTS tokens;
DROP TABLE IF EXISTS users;
DROP EXTENSION IF EXISTS "uuid-ossp";

COMMIT;
