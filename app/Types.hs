module Types where

import Data.Text

data TableMigration = TableMigration
  { baseTableName :: String
  , partitionSupportEnabled :: Bool
  , enableParitionQuery :: Maybe String
  , changelog :: [Changelog]
  }
  deriving (Show, Eq)

data Changelog = Changelog
  { changelogVersion :: Integer
  , tag :: String
  , withAutomaticRollback :: Bool
  , migrationQuery :: String
  , rollbackQuery :: String
  , hash :: Text
  }
  deriving (Show, Eq)
