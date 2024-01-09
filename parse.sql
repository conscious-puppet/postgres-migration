--@baseTableName: example
--@partitionSupportEnabled: False

--@changelogVersion: 1
--@tag: v1
--@withAutomaticRollback: False

--@migrationQuery
CREATE TABLE IF NOT EXISTS :tablename (
    id integer NOT NULL,
    key character varying(255) NOT NULL,
    value character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--@rollbackQuery
DROP TABLE IF EXISTS :tablename;

--@changelogVersion: 2
--@tag: v2
--@withAutomaticRollback: True

--@migrationQuery
INSERT INTO :tablename VALUES (1, "key1", "value1", NOW(), NOW());

--@rollbackQuery
DELETE FROM :tablename where id = 1;
