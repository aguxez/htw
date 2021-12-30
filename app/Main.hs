{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (void)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, decode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser (..), parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics
import Network.HTTP.Client
  ( Request (method, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    Response (..),
    httpLbs,
    newManager,
    setQueryString,
  )
import Network.HTTP.Client.Conduit (applyBearerAuth)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (getResponseBody, parseRequest)
import System.Environment (getArgs, getEnv)

data TweetUser = TweetUser
  { name :: String,
    username :: String
  }
  deriving (Generic, Show)

data Tweet = Tweet
  { tweetId :: String,
    statusIdReply :: Maybe String,
    text :: String,
    user :: TweetUser
  }
  deriving (Generic, Show)

instance FromJSON Tweet where
  parseJSON = tweetParser

instance FromJSON TweetUser where
  parseJSON = tweetUserParser

tweetParser :: Value -> Parser Tweet
tweetParser = withObject "Tweet" $ \obj -> do
  tweetId <- obj .: "id_str"
  statusIdReply <- obj .:? "in_reply_to_status_id_str"
  text <- obj .: "full_text"
  user <- obj .: "user"
  pure (Tweet {tweetId = tweetId, statusIdReply = statusIdReply, text = text, user = user})

tweetUserParser :: Value -> Parser TweetUser
tweetUserParser = withObject "TweetUser" $ \obj -> do
  name <- obj .: "name"
  username <- obj .: "screen_name"
  pure (TweetUser {name = name, username = username})

bearerToken :: IO (BS.ByteString)
bearerToken = do
  tokenEnv <- getEnv "BEARER_TOKEN"
  return $ BS8.pack tokenEnv

baseUrl :: String
baseUrl = "https://api.twitter.com/1.1/statuses/show.json"

applyQueryStringAndAuth :: String -> BS.ByteString -> Request -> Request
applyQueryStringAndAuth tweetId token request =
  setQueryString [("id", Just (BS8.pack tweetId)), ("tweet_mode", Just "extended")] $ applyBearerAuth token request

tweetResponse :: String -> IO (Tweet)
tweetResponse tweetId = do
  manager <- newManager tlsManagerSettings
  nakedRequest <- parseRequest baseUrl
  token <- bearerToken
  let request = applyQueryStringAndAuth tweetId token nakedRequest
  response <- httpLbs request manager
  return $ fromJust $ decode $ getResponseBody response

moreTweets :: Maybe String -> [Tweet] -> IO ([Tweet])
moreTweets Nothing accTweets = return accTweets
moreTweets (Just replyId) accTweets = do
  newTweet <- tweetResponse replyId
  moreTweets (statusIdReply newTweet) (newTweet : accTweets)

parseTweets :: [Tweet] -> [String]
parseTweets = map convertToStr
  where
    convertToStr x = "\nusername: @" ++ (username $ user x) ++ "\nname: " ++ (name $ user x) ++ "\ntext: " ++ text x ++ "\n---"

getThread :: String -> IO ()
getThread tweetId = do
  tweet <- tweetResponse tweetId
  tweets <- moreTweets (statusIdReply tweet) [tweet]
  let parsedTweets = parseTweets tweets
  mapM_ putStrLn parsedTweets

main :: IO ()
main = do
  void $ loadFile defaultConfig
  [tweetId] <- getArgs
  getThread tweetId
