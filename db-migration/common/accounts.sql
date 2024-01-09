--@baseTableName: account
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
