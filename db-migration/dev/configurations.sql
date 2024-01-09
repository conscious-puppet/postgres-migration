--@baseTableName: configuration
--@partitionSupportEnabled: False

--@changelogVersion: 1
--@tag: v1
--@withAutomaticRollback: True

--@migrationQuery
INSERT INTO :tablename VALUES (1, "key1", "value1", NOW(), NOW());

--@rollbackQuery
DELETE FROM :tablename where id = 1;
