module SqlParser where

import Control.Applicative
import Control.Monad (void, when)
import qualified Crypto.Hash as CH
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import qualified Data.Text as DT

-- import qualified Data.Text.Encoding as DT
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Text.Read (readMaybe)
import Types

parseSqlFromFile :: FilePath -> IO TableMigration
parseSqlFromFile inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = pack $ unlines trimmedLines <> "--@"
  case parseOnly (sqlParser inputFile) finalString of
    Left s -> error $ inputFile <> ":\n\t" <> s
    Right migration -> return migration

sqlParser :: FilePath -> Parser TableMigration
sqlParser inputFile = do
  btn <-
    trim
      <$> ( (string "--@baseTableName:" <|> error ("@baseTableName tag not present for " <> inputFile))
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null btn) $ error $ "@baseTableName value not provided for " <> inputFile
  partitionSupportEnabledMaybe <-
    readMaybe
      . trim
      <$> ( (string "--@partitionSupportEnabled:" <|> error ("@partitionSupportEnabled tag not present for " <> inputFile))
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null partitionSupportEnabledMaybe) $ error $ "@partitionSupportEnabled value not provided, or is an invalid Bool for " <> inputFile
  let pse = fromMaybe False partitionSupportEnabledMaybe
  enablePartitionQuery <- fromMaybe "" <$> optional parseEnableParitionQuery
  when (pse && null enablePartitionQuery) $ error $ "@enablePartitionQuery value is not provided for " <> inputFile
  when (not pse && not (null enablePartitionQuery)) $ error $ "@enablePartitionQuery provided when @partitionSupportEnabled is False for " <> inputFile
  cl <- some (parseChangeLog inputFile)
  return $ TableMigration btn pse (Just enablePartitionQuery) cl

parseEnableParitionQuery :: Parser String
parseEnableParitionQuery = do
  string "--@enablePartitionQuery\n"
    *> manyTill
      anyChar
      (lookAhead $ string "--@")

parseChangeLog :: FilePath -> Parser Changelog
parseChangeLog inputFile = do
  versionMaybe <- readMaybe . trim <$> ((string "--@changelogVersion:" <?> "@changelogVersion tag not for a changelog") *> manyTill anyChar (lookAhead $ string "--@"))
  when (null versionMaybe) $ error $ "@changelogVersion value not provided for a changelog in " <> inputFile
  let version = fromMaybe 0 versionMaybe
  tg <-
    trim
      <$> ( (string "--@tag:" <|> error ("@tag tag not found for a changelog in " <> inputFile))
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null tg) $ error $ "@tag value not provided for a changelog in " <> inputFile
  withAutomaticRollbackMaybe <-
    readMaybe
      . trim
      <$> ( (string "--@withAutomaticRollback:" <|> error ("@withAutomaticRollback tag not found for a changelog in " <> inputFile))
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null withAutomaticRollbackMaybe) $ error $ "@withAutomaticRollback value not provided, or is not a valid Bool for a changelog in " <> inputFile
  let automaticRollback = fromMaybe False withAutomaticRollbackMaybe
  mQuery <- (string "--@migrationQuery\n" <|> error ("@migrationQuery tag not provided for a changelog in " <> inputFile)) *> manyTill anyChar (lookAhead $ string "--@")
  rQuery <- (string "--@rollbackQuery\n" <|> error ("@rollbackQuery tag not provided for a changelog in " <> inputFile)) *> manyTill anyChar (lookAhead $ string "--@")
  let migrationHash = (CH.hash . BSU.fromString $ mQuery) :: (CH.Digest CH.MD5)
  return $ Changelog version tg automaticRollback mQuery rQuery (DT.pack $ show migrationHash)

isEmpty :: String -> Bool
isEmpty = all isSpace

trim :: String -> String
trim input = reverse flippedTrimmed
 where
  trimStart = dropWhile isSpace input
  flipped = reverse trimStart
  flippedTrimmed = dropWhile isSpace flipped

nonNewlineSpace :: Char -> Bool
nonNewlineSpace c = isSpace c && c /= '\n'

consumeLine :: Parser String
consumeLine = do
  str <- Data.Attoparsec.Text.takeWhile (/= '\n')
  void $ char '\n'
  return (unpack str)
