{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Data.Aeson
--import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Prelude hiding (log)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  infile <- parseArgs
  harBytes <- B.readFile infile
  let eHar = eitherDecode harBytes :: Either String Har
  either processParseError processHar eHar

processParseError :: String -> IO ()
processParseError err = die $ "Invalid HAR file: " ++ err

processHar :: Har -> IO ()
processHar har = do
    -- TODO how to present extracted errai messages?
    -- 1) pretty jsons -> too much data
    mapM_ (maybe (return ()) B.putStrLn) prettyErraiJsons

    -- 2) fixed fields -> but don't know which are there and which are relevant
    mapM_ print erraiMessages

    printStats harEntries erraiPostEntries errors erraiMessages
  where
    harEntries = entries $ log har
    erraiPostEntries = filter isErraiEntry harEntries

    prettyErraiJsons = fmap (erraiPostBodyToPrettyJson . erraiMessagesJson) erraiPostEntries

    (errors, listsOfMessages) = partitionEithers $ fmap extractErraiMessages erraiPostEntries
    erraiMessages = concat listsOfMessages

printStats :: [HarEntry] -> [HarEntry] -> [String] -> [ErraiMessage] -> IO ()
printStats allHarEntries erraiPostEntries erraiErrors erraiMsgs = do
    putStrLn $ "Total entries : " ++ shoLen allHarEntries ++ " (" ++ shoLen erraiPostEntries ++ " errai POST entries)"
    putStrLn $ "Errai messages: " ++ shoLen erraiMsgs ++ " decoded successfully, " ++ shoLen erraiErrors ++ " failed to decode"
  where
    shoLen = show . length

erraiPostBodyToPrettyJson :: Text -> Maybe B.ByteString
erraiPostBodyToPrettyJson tbody =
    fmap encodePretty ((decode $ toLbs tbody) :: Maybe Value)

erraiMessagesJson :: HarEntry -> Text
-- fromJust justified because we are sure there's postData based on isErraiEntry
erraiMessagesJson = text . fromJust . postData . request

extractErraiMessages :: HarEntry -> Either String [ErraiMessage]
extractErraiMessages = eitherDecode . toLbs . erraiMessagesJson

parseArgs :: IO FilePath
parseArgs = do
  as <- getArgs
  case as of
    []    -> die "Usage ehe <file.har>"
    (f:_) -> return f

toLbs :: T.Text -> B.ByteString
toLbs = B.fromStrict . encodeUtf8
------------
data ErraiMessage = ErraiMessage
  { toSubject          :: Text
  , commandType        :: Maybe Text
  , replyTo            :: Maybe Text
  , value              :: Maybe Text
  , priorityProcessing :: Maybe Text
  , errorMessage       :: Maybe Text
  , throwable          :: Maybe Text
  }

instance Show ErraiMessage where
  show ErraiMessage{..} =
    T.unpack . T.unlines $
       catMaybes [Just "Errai Message"
       , may "ToSubject" (Just toSubject)
       , may "CommandType" commandType
       -- , may "ReplyTo" replyTo
       , may "Value"  value
       , may "PriorityProcessing" priorityProcessing
       , may "ErrorMessage" errorMessage
       , may "Throwable" throwable
       ]
       where may t = fmap (\v -> T.concat ["  ", t, " = ", v])

instance FromJSON ErraiMessage where
  parseJSON = withObject "ErraiMessage" $ \v -> ErraiMessage
        <$> v .: "ToSubject"
        <*> v .:? "CommandType"
        <*> v .:? "ReplyTo"
        <*> v .:? "Value"
        <*> v .:? "PriorityProcessing"
        <*> v .:? "ErrorMessage"
        <*> v .:? "Throwable"

newtype Har = Har
  { log :: Entries
  } deriving (Show, Generic)

newtype Entries = Entries
  { entries :: [HarEntry]
  } deriving (Show, Generic)

data HarEntry = HarEntry
  { request  :: Request
  , response :: Response
  } deriving (Show, Generic)

data Request = Request
  { postData :: Maybe PostData
  , method   :: Text
  , url      :: Text
  } deriving (Show, Generic)

data PostData = PostData
  { text     :: Text
  , mimeType :: Text
  } deriving (Show, Generic)

newtype Response = Response
  { status :: Int } deriving (Show, Generic)

instance FromJSON Har
instance FromJSON Entries
instance FromJSON HarEntry
instance FromJSON Request
instance FromJSON Response
instance FromJSON PostData

isErraiEntry :: HarEntry -> Bool
isErraiEntry (HarEntry (Request (Just (PostData _ mimeType)) "POST" url) _ ) =
  "erraiBus" `T.isInfixOf` url && "application/json" `T.isInfixOf` mimeType
isErraiEntry _ = False
