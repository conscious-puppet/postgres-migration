{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (foldM, unless, void)
import SqlParser
import qualified System.Directory as SD
import Types

main :: IO ()
main = do
  commonMigrations <- getTableMigrations "common"
  mapM_ (verifyVersions "common") commonMigrations
  devMigrations <- getTableMigrations "dev"
  mapM_ (verifyVersions "devMigrations") devMigrations

  print devMigrations
  -- throw an error if any number is skipped in the version
  -- sort migrations, zipWith [1..] migrations, and next filter
  -- get lastexecuted version for this table
  -- compare all the md5 versions before
  -- select version, rollback_query from table_migration where version = '' and not (md5hash = '') and automatic_rollback = 't';
  -- execute automatic_rollbacks
  -- run the update hash versions
  -- run the new queries
  pure ()

verifyVersions :: String -> TableMigration -> IO ()
verifyVersions env tableMigration = do
  let invalidMigrations = filter (\(a, b) -> changelogVersion a /= b) $ zip (changelog tableMigration) [1 ..]
  unless (null invalidMigrations)
    $ error
    $ "Version is not correct for the following query in "
    <> env
    <> "/"
    <> baseTableName tableMigration
    <> ":\n\t"
    <> show (migrationQuery $ fst . head $ invalidMigrations)

getTableMigrations :: String -> IO [TableMigration]
getTableMigrations env = do
  let dbMigrationFilePath = "db-migration/"
  envs <- SD.listDirectory dbMigrationFilePath
  if env `elem` envs
    then do
      migrationFiles <- SD.listDirectory $ dbMigrationFilePath <> env <> "/"
      mapM (\f -> parseSqlFromFile (dbMigrationFilePath <> env <> "/" <> f)) migrationFiles
    else return []
