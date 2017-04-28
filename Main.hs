{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (die)

--instance FromJSON
main :: IO ()
main = do
  infile <- parseArgs
  Just v <- parseHar infile
  let Just messages = v ^? key "log" . key "entries" . _Array
      erraiPostMessages = V.filter isErraiPost messages

       -- TODO figure out how to avoid cat maybes and manual deconstructino using traversal?
      postDataFromErraiMessages = flattenMaybe $ fmap postDataText erraiPostMessages :: V.Vector T.Text

      (errors, decodedMessages) = fmap concat . partitionEithers . V.toList $ fmap decodeErraiMessages postDataFromErraiMessages :: ([String], [ErraiMessage])


  mapM_ print decodedMessages
  putStrLn $ unlines
    [ "Total messages = " ++ show (V.length messages)
    , "Errai POST messages = " ++ show (V.length erraiPostMessages)
    , "   " ++ show (length errors) ++ " decoded errors"
    , "   " ++ show (length decodedMessages) ++ " decoded successfully"
    ]

parseArgs :: IO FilePath
parseArgs = do
  as <- getArgs
  case as of
    []    -> die "Usage ehe <file.har>"
    (f:_) -> return f

parseHar :: FilePath -> IO (Maybe Value)
parseHar = fmap decode . B.readFile

flattenMaybe :: V.Vector (Maybe a) -> V.Vector a
flattenMaybe = fmap fromJust . V.filter isJust

method :: Value -> Maybe Value
method msg = msg ^? key "request" . key "method"

url :: Value -> Maybe T.Text
url msg = msg ^? key "request" . key "url" . _String

urlIsErraiBus :: Value -> Bool
urlIsErraiBus msg = maybe False (T.isInfixOf "erraiBus") $ url msg

mimeType :: Value -> Maybe Value
mimeType msg = msg ^? key "request" . key "postData" . key "mimeType"

postDataText :: Value -> Maybe T.Text
postDataText o = o ^? key "request" . key "postData" . key "text" . _String

isPost :: Value -> Bool
isPost msg = method msg == Just (String "POST")

hasJsonText :: Value -> Bool
hasJsonText msg = mimeType msg == Just (String "application/json; charset=utf-8")

isErraiPost :: Value -> Bool
isErraiPost msg = isPost msg && hasJsonText msg && urlIsErraiBus msg

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

decodeErraiMessages :: Text -> Either String [ErraiMessage]
decodeErraiMessages = eitherDecode . toLbs
