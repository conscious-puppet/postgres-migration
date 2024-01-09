{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (foldM, unless, void, when)
import qualified Crypto.Hash as CH
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe
import qualified Data.Text as DT
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Only (Only))
import qualified Database.PostgreSQL.Simple.Types as PSQL
import GHC.Generics (Generic)
import qualified MigrationTableTypes as MT
import SqlParser
import qualified System.Directory as SD
import qualified Types as T
import GHC.TopHandler (runIO)

main :: IO ()
main = do
  commonMigrations <- getTableMigrations "common"
  mapM_ (verifyVersions "common") commonMigrations
  devMigrations <- getTableMigrations "dev"
  mapM_ (verifyVersions "devMigrations") devMigrations
  -- throw an error if any number is skipped in the version
  -- sort migrations, zipWith [1..] migrations, and next filter
  -- get lastexecuted version for this table
  -- compare all the md5 versions before
  -- select version, rollback_query from table_migration where version = '' and not (md5hash = '') and automatic_rollback = 't';
  -- execute automatic_rollbacks
  -- run the update hash versions
  -- run the new queries
  mapM_ (runMigrations "dev") (commonMigrations <> devMigrations)
  -- print (commonMigrations <> devMigrations)

getTableMigrations :: String -> IO [T.TableMigration]
getTableMigrations env = do
  let dbMigrationFilePath = "db-migration/"
  envs <- SD.listDirectory dbMigrationFilePath
  if env `elem` envs
    then do
      migrationFiles <- SD.listDirectory $ dbMigrationFilePath <> env <> "/"
      mapM (\f -> parseSqlFromFile env (dbMigrationFilePath <> env <> "/" <> f)) migrationFiles
    else return []

verifyVersions :: String -> T.TableMigration -> IO ()
verifyVersions env tableMigration = do
  let invalidMigrations = filter (\(a, b) -> T.changelogVersion a /= b) $ zip (T.changelog tableMigration) [1 ..]
  unless (null invalidMigrations)
    $ error
    $ "Version is not correct for the following query in "
    <> env
    <> "/"
    <> T.baseTableName tableMigration
    <> ":\n\t"
    <> show (T.migrationQuery $ fst . head $ invalidMigrations)

runMigrations :: String -> T.TableMigration -> IO ()
runMigrations env migrations = do
  -- create migration table if it does not exist
  conn <- PSQL.connectPostgreSQL "host=localhost port=5432 dbname=postgres_migration user=bilbo password=baggins"
  let createMigrationTableQuery =
        [sql|

              DO $$
              BEGIN
                  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'enum_migration_table_status') THEN
                        CREATE TYPE "enum_migration_table_status" AS ENUM (
                            'ACTIVE',
                            'REVERTED',
                            'SKIPPED'
                        );
                  END IF;
              END$$;

              CREATE TABLE IF NOT EXISTS migration_table (
                  id SERIAL PRIMARY KEY,
                  env TEXT NOT NULL,
                  tag TEXT NOT NULL,
                  description TEXT NOT NULL,
                  table_name TEXT NOT NULL,
                  changelog_version INTEGER NOT NULL,
                  md5_hash TEXT NOT NULL,
                  applied_at TIMESTAMP WITH TIME ZONE NOT NULL,
                  updated_at TIMESTAMP WITH TIME ZONE NOT NULL,
                  status "enum_migration_table_status" NOT NULL,
                  with_automatic_rollback BOOLEAN NOT NULL,
                  rollback_query TEXT
              );

          |]

  void $ PSQL.execute_ conn createMigrationTableQuery
  -- mapM_ (runMigrationsHelper conn) (T.changelog migrations)
  -- let query = [sql| select * from $x |] tableName
  --     tableName = "migration_table" :: String
  -- void $ PSQL.execute_ conn query (Only tableName)
  pure ()
 where
  -- runMigrationsHelper conn migration = do
  --   let getMigrationFromVersion =
  --         [sql|
  --                 SELECT
  --                   id,
  --                   md5_hash,
  --                   with_automatic_rollback,
  --                   rollback_query,
  --                   changelog_version
  --                 FROM migration_table
  --                 WHERE changelog_version = ?
  --                 AND status = 'ACTIVE'
  --                 AND env = ?
  --                 AND table_name = ?;
  --             |]
  --
  --   let revertMigrationFromVersion =
  --         [sql|
  --                 UPDATE migration_table
  --                 set
  --                   status = 'REVERTED',
  --                   updated_at = NOW()
  --                 WHERE id = ?;
  --             |]
  --
  --   let skippedMigrationFromVersion =
  --         [sql|
  --                 UPDATE migration_table
  --                 set
  --                   status = 'SKIPPED',
  --                   updated_at = NOW()
  --                 WHERE id = ?;
  --             |]
  --
  --   let insertQuery =
  --         [sql|
  --           INSERT INTO migration_table
  --             (   env,
  --                 tag,
  --                 description,
  --                 table_name,
  --                 changelog_version,
  --                 md5_hash,
  --                 status,
  --                 with_automatic_rollback,
  --                 rollback_query,
  --                 applied_at,
  --                 updated_at
  --             )
  --           VALUES
  --             (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW());
  --             |]
  --
  --   dbMigrations :: [(Integer, DT.Text, Bool, Maybe BS.ByteString, Integer)] <-
  --     PSQL.query conn getMigrationFromVersion (T.changelogVersion migration, env, T.baseTableName migrations <> "s")
  --   case (dbMigrations, env) of
  --     ([], "dev") -> do
  --       let insertMigrationQuery = T.migrationQuery migration
  --           tableName = T.baseTableName migrations <> "s"
  --           -- setTableName = "\\set :tablename " <> tableName <> "; "
  --           -- query = [sql| \set :tablename ?; |] <> (PSQL.Query . BSU.fromString $ insertMigrationQuery) <> [sql| ; \unset :tablename; |]
  --           query1 = PSQL.Query . BSU.fromString $ " /set tablename " <> tableName <> ";"
  --           query2 = PSQL.Query . BSU.fromString $ insertMigrationQuery
  --           -- unsetTableName = "; \\unset :tablename;"
  --       -- void $ PSQL.execute conn setTableName (Only tableName)
  --       -- void
  --       --   $ PSQL.execute_ conn (PSQL.Query . BSU.fromString $ setTableName <> insertMigrationQuery <> unsetTableName)
  --       void $ PSQL.execute_ conn query1
  --       void $ PSQL.execute_ conn query2
  --       void
  --         $ PSQL.execute
  --           conn
  --           insertQuery
  --           ( env
  --           , T.tag migration
  --           , T.description migration
  --           , T.baseTableName migrations <> "s"
  --           , T.changelogVersion migration
  --           , T.md5Hash migration
  --           , "ACTIVE" :: DT.Text
  --           , True
  --           , Just $ T.rollbackQuery migration
  --           )
  --     ([], _) -> do
  --       let insertMigrationQuery = T.migrationQuery migration
  --           tableName = T.baseTableName migrations <> "s"
  --           -- setTableName = "\\set :tablename " <> tableName <> "; "
  --           -- query = [sql| \set :tablename ?; |] <> (PSQL.Query . BSU.fromString $ insertMigrationQuery) <> [sql| ; \unset :tablename; |]
  --           query = [sql| \set :tablename ?; |] <> (PSQL.Query . BSU.fromString $ insertMigrationQuery)
  --           -- unsetTableName = "; \\unset :tablename;"
  --       -- void $ PSQL.execute conn setTableName (Only tableName)
  --       -- void
  --       --   $ PSQL.execute_ conn (PSQL.Query . BSU.fromString $ setTableName <> insertMigrationQuery <> unsetTableName)
  --       print query
  --       void $ PSQL.execute conn query (Only tableName)
  --       void
  --         $ PSQL.execute
  --           conn
  --           insertQuery
  --           ( env
  --           , T.tag migration
  --           , T.description migration
  --           , T.baseTableName migrations <> "s"
  --           , T.changelogVersion migration
  --           , T.md5Hash migration
  --           , "ACTIVE" :: DT.Text
  --           , T.withAutomaticRollback migration
  --           , if T.withAutomaticRollback migration then Just $ T.rollbackQuery migration else Nothing
  --           )
  --     (dbMigrations, "dev") -> do
  --       mapM_
  --         ( \(id', hash, _, query, _) -> do
  --             when (hash /= T.md5Hash migration) $ void $ do
  --               PSQL.execute_ conn (PSQL.Query $ fromJust query)
  --               PSQL.execute conn revertMigrationFromVersion (Only id')
  --         )
  --         dbMigrations
  --     (dbMigrations, _) -> do
  --       mapM_
  --         ( \(id', hash, with_automatic_rollback, query, changelog_version) -> do
  --             when (hash /= T.md5Hash migration) $ void $ do
  --               if with_automatic_rollback
  --                 then
  --                   PSQL.execute_ conn (PSQL.Query $ fromJust query)
  --                     >> PSQL.execute conn revertMigrationFromVersion (Only id')
  --                 else do
  --                   putStrLn
  --                     $ "MD5 Hash for the version '"
  --                     <> show changelog_version
  --                     <> "' got changed, skipping it. Consider manual rollback."
  --                   PSQL.execute conn skippedMigrationFromVersion (Only id')
  --         )
  --         dbMigrations
