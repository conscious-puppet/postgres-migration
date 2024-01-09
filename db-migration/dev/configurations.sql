--@baseTableName: configuration
--@partitionSupportEnabled: False
--@dbName: dbName

--@changelogVersion: 1
--@tag: v1
--@withAutomaticRollback: True
--@description: description

--@migrationQuery
INSERT INTO @tablename VALUES (1, "key1", "value1", NOW(), NOW());

--@rollbackQuery
DELETE FROM @tablename where id = 1;
