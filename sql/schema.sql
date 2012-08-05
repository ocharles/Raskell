BEGIN;

DROP SCHEMA IF EXISTS raskell CASCADE;
CREATE SCHEMA raskell;

CREATE TABLE rating (
    project TEXT NOT NULL,
    rater TEXT NOT NULL,
    PRIMARY KEY (project, rater)
);

COMMIT;
