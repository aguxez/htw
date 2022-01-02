{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Network.HTTP.Client (Request, applyBearerAuth, httpLbs, newManager, parseRequest,
                            setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)
import System.Environment (getArgs, getEnv)

newtype TweetID = TweetID String
  deriving (Show, Eq)

newtype Token = Token BS.ByteString
  deriving (Show, Eq)

data User = User
  { userFullName :: !String,
    userUsername :: !String
  }
  deriving (Show, Eq)

data Tweet = Tweet
  { tweetId          :: !TweetID,
    tweetInReplyToId :: !(Maybe TweetID),
    tweetText        :: !String,
    tweetUser        :: !User
  }
  deriving (Show, Eq)

instance FromJSON Tweet where
  parseJSON =  withObject "Tweet" $ \obj -> do
    tid <- obj .: "id_str"
    statusIdReply <- obj .:? "in_reply_to_status_id_str"
    text <- obj .: "full_text"
    user <- obj .: "user"
    pure (Tweet {tweetId = TweetID tid, tweetInReplyToId = TweetID <$> statusIdReply, tweetText = text, tweetUser = user})

instance FromJSON User where
  parseJSON = withObject "TweetUser" $ \obj -> do
    name <- obj .: "name"
    username <- obj .: "screen_name"
    pure $ User name username

getBearerToken :: IO Token
getBearerToken = do
  tokenEnv <- getEnv "BEARER_TOKEN"
  (return . Token . BS8.pack) tokenEnv

baseUrl :: String
baseUrl = "https://api.twitter.com/1.1/statuses/show.json"

applyQueryStringAndAuth :: Token -> TweetID -> Request -> Request
applyQueryStringAndAuth (Token rawToken) (TweetID tid) request = queryStr authReq
  where
    idParam = ("id", Just (BS8.pack tid))
    tweetMode = ("tweet_mode", Just "extended")
    queryStr = setQueryString [idParam, tweetMode]
    authReq = applyBearerAuth rawToken request

-- | Gets a Tweet by ID from the API
getTweetResponse :: Token -> TweetID -> IO (Either String Tweet)
getTweetResponse token tid = do
  manager <- newManager tlsManagerSettings
  nakedRequest <- parseRequest baseUrl
  let request = applyQueryStringAndAuth token tid nakedRequest
  response <- httpLbs request manager
  let body = getResponseBody response
  return $
    case getResponseStatusCode response of
      200     -> eitherDecode body
      badCode -> Left ("Got Bad status code: HTTP " <> show badCode <> ": " <> show body)

-- | Get all tweets in a thread, accumulating to the given list
getTweetThread :: Token -> TweetID -> IO [Tweet]
getTweetThread token = recurse []
  where
    recurse tweets tid = do
      response <- getTweetResponse token tid
      case response of
        Right replyTweet ->
          let allTweets = replyTweet : tweets
              maybeReplyId = tweetInReplyToId replyTweet
          in maybe (return allTweets) (recurse allTweets) maybeReplyId
        Left message -> error message

renderTweet :: Tweet -> String
renderTweet tweet = intercalate "\n" fields
  where
    fields =
      [ "username: @" <> userUsername user,
        "name: " <> userFullName user,
        "text: " <> tweetText tweet
      ]
    user = tweetUser tweet

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  token <- getBearerToken
  args <- getArgs
  case args of
    [tid] -> do
      putStrLn $ "Getting thread for " <> tid <> "\n---"
      tweets <- getTweetThread token (TweetID tid)
      putStrLn $ intercalate "\n---\n" (renderTweet <$> tweets)
    _ -> error "Usage: shrink <TWEET ID>"
