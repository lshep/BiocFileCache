-- TABLE
CREATE TABLE resource (
    rid INTEGER PRIMARY KEY AUTOINCREMENT,
    rname text,
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    access_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    rpath text
);
-- INSERT
INSERT INTO resource (
    rname, rpath
) VALUES (
    '%s', '%s'
);
SELECT rid FROM resource WHERE ROWID = last_insert_rowid();
-- REMOVE
DELETE FROM resource WHERE rid IN (%s);
-- UPDATE_PATH
UPDATE resource 
SET rpath = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%d';
-- UPDATE_TIME
UPDATE resource 
SET access_time = CURRENT_TIMESTAMP
WHERE rid = '%d';
-- UPDATE_RNAME
UPDATE resource 
SET rname = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%d';
