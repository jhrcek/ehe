{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Prelude hiding (log)
import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.Vector as V
import Control.Monad (when)

main :: IO ()
main = do
  Config infile debug <- parseArgs
  harBytes <- B.readFile infile
  let eHar = eitherDecode harBytes :: Either String Har
  either processParseError (processHar debug) eHar

data Config = Config FilePath Bool

processParseError :: String -> IO ()
processParseError err = die $ "Invalid HAR file: " ++ err

processHar :: Bool -> Har -> IO ()
processHar debug har = do
    when debug $ traverse_ (B.putStrLn . encodePretty) vectorOfErraiMessages
    let allMessagesByteString = encode $ Array vectorOfErraiMessages
        stringEncodedValue = show allMessagesByteString
        -- TODO write to elm index file
    printStats harEntries erraiPostEntries vectorOfErraiMessages
  where
    harEntries = entries $ log har
    erraiPostEntries = filter isErraiEntry harEntries
    vectorOfErraiMessages = V.concat $ fmap getMessageObjectsFromErraiMessageJson erraiPostEntries

printStats :: [HarEntry] -> [HarEntry] -> V.Vector Value -> IO ()
printStats allHarEntries erraiPostEntries erraiMessages = do
    putStrLn $ "HAR had " ++ show (length allHarEntries ) ++ " entries"
    putStrLn $ "Out of that " ++ show (length erraiPostEntries) ++ " were Errai POST entries"
    putStrLn $ "Errai POST entries contained " ++ show (V.length erraiMessages) ++ " errai messages"

erraiPostToJsonText :: HarEntry -> Text
-- fromJust justified because we are sure there's postData based on isErraiEntry
erraiPostToJsonText = text . fromJust . postData . request

getMessageObjectsFromErraiMessageJson :: HarEntry -> V.Vector Value
getMessageObjectsFromErraiMessageJson entry = array
  where
    eitherValue = eitherDecode . toLbs $ erraiPostToJsonText entry
    val = either ( \err -> error $ "Errai message body didn't contain valid JSON\n  error was: " ++ err ++ "\n  body was:\n" ++ show entry) id eitherValue
    array = case val of
        Array ar -> ar
        _ -> error $ "Errai POST's body's top level value was not array: " ++ show entry


parseArgs :: IO Config
parseArgs = do
  as <- getArgs
  case as of
    []    -> die "Usage ehe <file.har>"
    (file:remainingArgs) ->
      Config <$> verifyFile file <*> pure (isDebug remainingArgs)

verifyFile :: FilePath -> IO FilePath
verifyFile file = do
  exists <- doesFileExist file
  if exists
    then return file
    else die $ "File " ++ file ++ " does not exist\nUsage ehe <file.har>"

isDebug :: [String] -> Bool
isDebug ("--debug":_) = True
isDebug _ = False

toLbs :: T.Text -> B.ByteString
toLbs = B.fromStrict . encodeUtf8
------------


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
