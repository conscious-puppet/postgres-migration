module Main where

import Control.Monad (foldM, unless, when)
import qualified Crypto.Hash as CH
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe
import qualified Data.Text as DT
import qualified Database.PostgreSQL.Simple as PSQL
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
  let migrationLockFilePath = "db-migration-lock.json"
  fileExists <- SD.doesFileExist migrationLockFilePath
  migrationLock <-
    if fileExists
      then fromMaybe [] . decodeStrict <$> BS.readFile migrationLockFilePath :: IO [MigrationLock]
      else BS.writeFile migrationLockFilePath "[]" >> pure []
  refreshedMigrationLock <- refreshMigrationLock migrationLock
  BS.writeFile migrationLockFilePath (toStrict $ encodePretty refreshedMigrationLock)
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
  -- mapM_ verifyMigrationLocks migrationLocks
  return newMigrationLock

verifyMigrationLocks :: ([MigrationLockInfo], [MigrationLockInfo]) -> IO ()
verifyMigrationLocks (_, []) = return ()
verifyMigrationLocks ([], _) = return ()
verifyMigrationLocks (migrationLockInfos, newMigrationLockInfos) = do
  let newMigartionLockInfos' = tail newMigrationLockInfos
  let oldMigrationFile = last migrationLockInfos
  let filesChanged =
        [ filePath y
        | x <- migrationLockInfos
        , y <- newMigartionLockInfos'
        , majorVersion x == majorVersion y
            && minorVersion x == minorVersion y
            && pointVersion x == pointVersion y
            && hash x /= hash y
        ]
  unless (null filesChanged) $ error $ "Following old migration files have changed, please revert the changes:\n\t" <> show filesChanged
  let invalidMigrationFiles =
        foldl'
          ( \acc f ->
              if f `notElem` migrationLockInfos
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
  unless (null invalidMigrationFiles) $ error $ "Following new migration files have been added with old version, please update the version numbers:\n\t" <> show invalidMigrationFiles

createMigrationLockInfo :: FilePath -> IO [MigrationLockInfo]
createMigrationLockInfo e = do
  migrations <- SD.listDirectory e
  let match = filter (\x -> not (x =~ ("^migration_[0-9]{3}_[0-9]{3}_[0-9]{4}_.*[.]sql$" :: FilePath))) migrations
  unless (null match) $ error $ "Following migration files don't match regex 'migration_[0-9]{3}_[0-9]{3}_[0-9]{4}_.*[.]sql:\n\t" <> show match
  migrationLockInfos' <-
    mapM
      ( \x -> do
          let (_ : majorVersion : minorVersion : pointRelease : _) = DT.splitOn "_" $ DT.pack x
          fileHash <- CH.hash <$> BS.readFile (e <> x) :: IO (CH.Digest CH.MD5)
          return $ MigrationLockInfo (e <> x) majorVersion minorVersion pointRelease (DT.pack $ show fileHash)
      )
      migrations
  let groups =
        filter (\g -> length g > 1) $
          groupBy
            ( \a b ->
                majorVersion a == majorVersion b
                  && minorVersion a == minorVersion b
                  && pointVersion a == pointVersion b
                  && filePath a /= filePath b
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

runMigration :: IO ()
runMigration = do
  conn <- PSQL.connectPostgreSQL "host=localhost port=5432 dbname=postgres_migration user=bilbo password=baggins"

  -- run common dir
  let commonRelativePath = "db-migration/common/"
  common <- map (commonRelativePath <>) <$> SD.listDirectory commonRelativePath

  commonMigration <- mapM BS.readFile common
  -- mapM_ (PSQL.execute_ conn . PSQL.Query) commonMigration
  let hashCommon = map CH.hash commonMigration
  print (hashCommon :: [CH.Digest CH.MD5])

  -- run the env dir, here env = dev
  let devRelativePath = "db-migration/dev/"
  dev <- map (devRelativePath <>) <$> SD.listDirectory devRelativePath

  devMigration <- mapM BS.readFile dev
  -- mapM_ (PSQL.execute_ conn . PSQL.Query) devMigration
  let hashDev = map CH.hash devMigration
  print (hashDev :: [CH.Digest CH.MD5])
