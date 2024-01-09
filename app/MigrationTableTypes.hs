module MigrationTableTypes where

import Data.ByteString (ByteString)
import Data.Text
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data MigrationTableStatus = ACTIVE | REVERTED | SKIPPED
  deriving (Show, Generic)

data MigrationTable = MigrationTable
  { id :: Int
  , env :: String
  , tag :: String
  , description :: String
  , table_name :: String
  , changelog_version :: Integer
  , md5_hash :: Text
  , applied_at :: LocalTime
  , updated_at :: LocalTime
  , status :: String
  , with_automatic_rollback :: Bool
  , rollback_query :: Maybe ByteString
  }
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)
