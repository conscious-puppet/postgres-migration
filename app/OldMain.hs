{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (foldM, unless, void)
import qualified Crypto.Hash as CH
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe
import qualified Data.Text as DT
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Only (Only))
import qualified Database.PostgreSQL.Simple.Types as PSQL
import GHC.Generics (Generic)
import qualified System.Directory as SD
import Text.Regex.TDFA

data MigrationLock = MigrationLock
  { env :: DT.Text
  , migrationLockInfo :: [MigrationLockInfo]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MigrationLockInfo = MigrationLockInfo
  { filePath :: FilePath
  , majorVersion :: DT.Text
  , minorVersion :: DT.Text
  , pointVersion :: DT.Text
  , hash :: DT.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  -- let migrationLockFilePath = "db-migration-lock.json"
  -- fileExists <- SD.doesFileExist migrationLockFilePath
  -- migrationLock <-
  --   if fileExists
  --     then fromMaybe [] . decodeStrict <$> BS.readFile migrationLockFilePath :: IO [MigrationLock]
  --     else BS.writeFile migrationLockFilePath "[]" >> pure []
  -- refreshedMigrationLock <- refreshMigrationLock migrationLock
  -- BS.writeFile migrationLockFilePath (toStrict $ encodePretty refreshedMigrationLock)
  -- runMigrations "dev" refreshedMigrationLock
  pure ()

refreshMigrationLock :: [MigrationLock] -> IO [MigrationLock]
refreshMigrationLock migrationLock = do
  let dbMigrationFilePath = "db-migration/"
  envs <- SD.listDirectory dbMigrationFilePath
  newMigrationLock <-
    foldM
      ( \acc e -> do
          migartionLockInfo <- createMigrationLockInfo (dbMigrationFilePath <> e <> "/")
          pure (MigrationLock (DT.pack e) migartionLockInfo : acc)
      )
      []
      envs
  let migrationLocks =
        [ (migrationLockInfo x, migrationLockInfo y)
        | x <- migrationLock
        , y <- newMigrationLock
        , env x == env y
        ]
  mapM_ verifyMigrationLocks migrationLocks
  return newMigrationLock

verifyMigrationLocks :: ([MigrationLockInfo], [MigrationLockInfo]) -> IO ()
verifyMigrationLocks (_, []) = return ()
verifyMigrationLocks ([], _) = return ()
verifyMigrationLocks (migrationLockInfos, newMigrationLockInfos) = do
  let (latestMigrationFile : newMigartionLockInfos') = reverse newMigrationLockInfos
  let filesChanged =
        [ filePath y
        | x <- migrationLockInfos
        , y <- newMigartionLockInfos'
        , majorVersion x
            == majorVersion y
            && minorVersion x
            == minorVersion y
            && pointVersion x
            == pointVersion y
            && hash x
            /= hash y
        ]
  unless (null filesChanged) $ error $ "Following old migration files have changed. Only latest migration files should be edited, please revert the changes:\n\t" <> show filesChanged
  let removedMigrationLockInfos =
        filter
          ( \f ->
              f
                `notElem` newMigartionLockInfos'
                && ( majorVersion f
                      <> minorVersion f
                      <> pointVersion f
                      /= majorVersion latestMigrationFile
                      <> minorVersion latestMigrationFile
                      <> pointVersion latestMigrationFile
                   )
          )
          migrationLockInfos
  unless (null removedMigrationLockInfos) $ error $ "Following old migration files have been removed, please add them back:\n\t" <> show removedMigrationLockInfos
  let oldMigrationFile = last migrationLockInfos
  let invalidMigrationFiles =
        foldl'
          ( \acc f ->
              if f
                `notElem` migrationLockInfos
                && ( majorVersion f
                      <> minorVersion f
                      <> pointVersion f
                      < majorVersion oldMigrationFile
                      <> minorVersion oldMigrationFile
                      <> pointVersion oldMigrationFile
                   )
                then f : acc
                else acc
          )
          []
          newMigrationLockInfos
  unless (null invalidMigrationFiles) $ error $ "Following new migration files have been added with older versions, please update the version numbers:\n\t" <> show invalidMigrationFiles

createMigrationLockInfo :: FilePath -> IO [MigrationLockInfo]
createMigrationLockInfo e = do
  migrations <- SD.listDirectory e
  let match = filter (\x -> not (x =~ ("^migration_[0-9]{3}_[0-9]{3}_[0-9]{4}_.*[.]sql$" :: FilePath))) migrations
  unless (null match) $ error $ "Following migration files don't match regex 'migration_[0-9]{3}_[0-9]{3}_[0-9]{4}_.*[.]sql:\n\t" <> show match
  migrationLockInfos' <-
    mapM
      ( \x -> do
          let (_ : majorVersion : minorVersion : pointVersion : _) = DT.splitOn "_" $ DT.pack x
          fileHash <- CH.hash <$> BS.readFile (e <> x) :: IO (CH.Digest CH.MD5)
          return $ MigrationLockInfo (e <> x) majorVersion minorVersion pointVersion (DT.pack $ show fileHash)
      )
      migrations
  let groups =
        filter (\g -> length g > 1)
          $ groupBy
            ( \a b ->
                majorVersion a
                  == majorVersion b
                  && minorVersion a
                  == minorVersion b
                  && pointVersion a
                  == pointVersion b
                  && filePath a
                  /= filePath b
            )
            migrationLockInfos'
  unless (null groups) $ error $ "Following migration files have same version:\n\t" <> show (map (map filePath) groups)
  let migrationLockInfos =
        sortBy
          ( \a b ->
              compare
                (majorVersion a <> minorVersion a <> pointVersion a)
                (majorVersion b <> minorVersion b <> pointVersion b)
          )
          migrationLockInfos'
  return migrationLockInfos

runMigrations :: DT.Text -> [MigrationLock] -> IO ()
runMigrations env' migrationLocks = do
  conn <- PSQL.connectPostgreSQL "host=localhost port=5432 dbname=postgres_migration user=bilbo password=baggins"
  let createMigrationTableQuery =
        [sql|
              CREATE TABLE IF NOT EXISTS migration_table (
                  id SERIAL PRIMARY KEY,
                  env TEXT NOT NULL,
                  major_version TEXT NOT NULL,
                  minor_version TEXT NOT NULL,
                  point_version TEXT NOT NULL,
                  file_path TEXT NOT NULL,
                  applied_at TIMESTAMP WITH TIME ZONE NOT NULL
              );
          |]
  void $ PSQL.execute_ conn createMigrationTableQuery

  let commonMigrations = filter (\m -> env m == "common") migrationLocks
      envMigrations = filter (\m -> env m == env') migrationLocks
  mapM_ (runMigrationsHelper conn) (commonMigrations <> envMigrations)
 where
  runMigrationsHelper :: PSQL.Connection -> MigrationLock -> IO ()
  runMigrationsHelper conn migration = do
    putStrLn $ "Running " <> DT.unpack (env migration) <> " migrations..."
    let lastRanMigration =
          [sql|
            SELECT major_version, minor_version, point_version
            FROM migration_table
            WHERE
            env = ?
            ORDER BY
              id DESC
            LIMIT 1;
          |]
    lastRanVersion :: [(DT.Text, DT.Text, DT.Text)] <- PSQL.query conn lastRanMigration (Only (env migration) :: Only DT.Text)
    let newScripts = case lastRanVersion of
          [] -> migrationLockInfo migration
          [(mjV, miV, poV)] -> filter (\c -> majorVersion c <> minorVersion c <> pointVersion c > mjV <> miV <> poV) (migrationLockInfo migration)
          _ -> error "Unexpected error while fetching last executed migration script"
    let updateMigrationTableQuery =
          [sql|
            INSERT INTO migration_table
              (env, major_version, minor_version, point_version, file_path, applied_at)
            VALUES
              (?, ?, ?, ?, ?, NOW());
          |]
    mapM_
      ( \script -> do
          q <- BS.readFile (filePath script)
          putStrLn $ "Executing " <> filePath script <> " ..."
          void $ PSQL.execute_ conn . PSQL.Query $ q
          void
            $ PSQL.execute
              conn
              updateMigrationTableQuery
              ( env migration
              , majorVersion script
              , minorVersion script
              , pointVersion script
              , DT.pack $ filePath script
              )
      )
      newScripts
