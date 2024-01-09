--@baseTableName: account
--@partitionSupportEnabled: False

--@changelogVersion: 1
--@tag: v1
--@withAutomaticRollback: True

--@migrationQuery
INSERT INTO :tablename VALUES (1, "key1", "value1", NOW(), NOW());

--@rollbackQuery
DELETE FROM :tablename where id = 1;

--@changelogVersion: 2
--@tag: v2
--@withAutomaticRollback: True

--@migrationQuery
INSERT INTO :tablename VALUES (2, "key2", "value2", NOW(), NOW());

--@rollbackQuery
DELETE FROM :tablename where id = 2;
