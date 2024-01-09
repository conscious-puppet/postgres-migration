module Types where

import Data.Text

data TableMigration = TableMigration
  { env :: String
  , baseTableName :: String
  , dbName :: String
  , partitionSupportEnabled :: Bool
  , enableParitionQuery :: Maybe String
  , changelog :: [Changelog]
  }
  deriving (Show, Eq)

data Changelog = Changelog
  { changelogVersion :: Integer
  , tag :: String
  , description :: String
  , withAutomaticRollback :: Bool
  , migrationQuery :: (String, String)
  , rollbackQuery :: (String, String)
  , md5Hash :: Text
  }
  deriving (Show, Eq)
