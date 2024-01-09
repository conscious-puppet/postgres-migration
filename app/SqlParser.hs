module SqlParser where

import Control.Applicative
import Control.Monad (void, when)
import qualified Crypto.Hash as CH
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import qualified Data.Text as DT
import Text.Read (readMaybe)
import Types

parseSqlFromFile :: String -> FilePath -> IO TableMigration
parseSqlFromFile env inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = pack $ unlines trimmedLines <> "--@"
  case parseOnly (sqlParser env inputFile) finalString of
    Left s -> error $ inputFile <> ":\n\t" <> s
    Right migration -> return migration

sqlParser :: String -> FilePath -> Parser TableMigration
sqlParser env inputFile = do
  btn <-
    trim
      <$> ( (string "--@baseTableName:" <?> "@baseTableName tag not present")
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null btn) $ error $ "@baseTableName value not provided for " <> inputFile
  partitionSupportEnabledMaybe <-
    readMaybe
      . trim
      <$> ( (string "--@partitionSupportEnabled:" <?> "@partitionSupportEnabled tag not present")
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null partitionSupportEnabledMaybe) $ fail "@partitionSupportEnabled value not provided, or is an invalid Bool"
  let pse = fromMaybe False partitionSupportEnabledMaybe
  dbName <-
    trim
      <$> ( (string "--@dbName:" <?> "@dbName tag not present")
              *> manyTill anyChar (lookAhead $ string "--@")
          )
  when (null dbName) $ error $ "@dbName value not provided for " <> inputFile
  enablePartitionQuery <- fromMaybe "" <$> optional parseEnableParitionQuery
  when (pse && null enablePartitionQuery) $ fail "@enablePartitionQuery value is not provided"
  when (not pse && not (null enablePartitionQuery)) $ fail "@enablePartitionQuery provided when @partitionSupportEnabled is False"
  cl <- some (parseChangeLog inputFile)
  return $ TableMigration env btn dbName pse (Just enablePartitionQuery) cl

parseEnableParitionQuery :: Parser String
parseEnableParitionQuery = do
  string "--@enablePartitionQuery\n"
    *> manyTill
      anyChar
      (lookAhead $ string "--@")

parseChangeLog :: FilePath -> Parser Changelog
parseChangeLog inputFile = do
  clVersion <- optional getChangelogVersion
  tg <- optional getTag
  automaticRollback <- optional getAutomaticRollback
  description <- optional getDescription
  mQuery <- optional getMigrationQuery
  rQuery <- optional getRollbackQuery
  parseChangeLogHelper clVersion tg automaticRollback description mQuery rQuery
 where
  parseChangeLogHelper Nothing Nothing Nothing Nothing Nothing Nothing = fail "Failed to get changelog"
  parseChangeLogHelper (Just clVersion) (Just tg) (Just automaticRollback) (Just description) (Just mQuery) (Just rQuery) = do
    let version = case readMaybe clVersion of
          Nothing -> error $ inputFile <> ":\n\t" <> "@changelogVersion value is not an Integer"
          Just v -> v
    let ar = case readMaybe automaticRollback of
          Nothing -> error $ inputFile <> ":\n\t" <> "@withAutomaticRollback value is not a Bool"
          Just v -> v
    let migrationHash = (CH.hash . BSU.fromString $ mQuery) :: (CH.Digest CH.MD5)

    migrationQuery <- case parseOnly parseSqlQuery (DT.pack mQuery) of
      Left s -> error $ inputFile <> ":\n\t" <> s
      Right q -> return q
    rollbackQuery <- case parseOnly parseSqlQuery (DT.pack rQuery) of
      Left s -> error $ inputFile <> ":\n\t" <> s
      Right q -> return q

    return $ Changelog version (trim tg) (trim description) ar migrationQuery rollbackQuery (DT.pack $ show migrationHash)
  parseChangeLogHelper _ _ _ _ _ _ = error $ inputFile <> ":\n\t" <> "error parsing changelog"

getChangelogVersion :: Parser String
getChangelogVersion = string "--@changelogVersion:" *> manyTill anyChar (lookAhead $ string "--@")

getTag :: Parser String
getTag = string "--@tag:" *> manyTill anyChar (lookAhead $ string "--@")

getAutomaticRollback :: Parser String
getAutomaticRollback = string "--@withAutomaticRollback:" *> manyTill anyChar (lookAhead $ string "--@")

getDescription :: Parser String
getDescription = string "--@description:" *> manyTill anyChar (lookAhead $ string "--@")

getMigrationQuery :: Parser String
getMigrationQuery = string "--@migrationQuery\n" *> manyTill anyChar (lookAhead $ string "--@")

getRollbackQuery :: Parser String
getRollbackQuery = string "--@rollbackQuery\n" *> manyTill anyChar (lookAhead $ string "--@")

parseSqlQuery :: Parser (String, String)
parseSqlQuery = do
  first <- manyTill anyChar (lookAhead (string "@tablename" <?> fail "Failed to find the variable @tablename in the query"))
  void (string "@tablename")
  rest <- takeText
  return (first, unpack rest)

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
