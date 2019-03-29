{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Either
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.ParserCombinators.ReadP
import Control.DeepSeq
import Control.Exception

serverInfoP :: ReadP (Int, String)
serverInfoP =
    (,) <$> ( string "ConstServerInfo.World_"
              *> readS_to_P reads
              <* skipSpaces <* string "=" <* skipSpaces
            )
        <*> ( string "\""
              *> munch1 (/='\"')
              <* string "\";" <* skipSpaces <* eof
            )

parseServerInfo :: String -> IM.IntMap String
parseServerInfo =
    IM.fromList . mapMaybe parseLine . lines
  where
    parseLine inp = case readP_to_S serverInfoP inp of
        [(a,"")] -> Just a
        _ -> Nothing

serverAddrToIp :: String -> String
serverAddrToIp raw = case readP_to_S ipP raw of
    [(a, [])] -> intercalate "." $ show <$> a
    _ -> error "no parse"
  where
    ipPartP :: ReadP Int
    ipPartP = do
      x <- read <$> munch1 isDigit
      guard $ x >= 0 && x < 256
      pure x
    ipP :: ReadP [Int]
    ipP = do
      _ <- string "http://"
      a <- ipPartP
      [b,c,d] <- count 3 (char '.' *> ipPartP)
      _ <- char '/'
      eof
      pure [a,b,c,d]

-- | check network connection
checkNetwork :: IO Bool
checkNetwork =
    catch checkGoogle (\(_ :: SomeException) -> pure False)
  where
    checkGoogle =  do
      -- one shot manager. in case there are caching related behaviors
      mgr <- newManager tlsManagerSettings
      req <- parseUrlThrow "http://www.google.com/generate_204"
      resp <- httpNoBody req mgr
      let hdrs = responseHeaders resp
      pure $! hdrs `deepseq` True

-- getServerMaps :: Manager -> IO (IM.Map String String)
simpleReq :: Manager -> String -> IO BSL.ByteString
simpleReq mgr url = do
  req <- parseUrlThrow url
  responseBody <$> httpLbs req mgr

getServerMaps :: Manager -> IO (IM.IntMap String)
getServerMaps mgr =
    -- we'd like to return a value even if the network fails
    catch getMapsFromServer (\(_ :: SomeException) -> pure defMap)
  where
    defMap = IM.fromList
      [ (1, "203.104.209.71")
      , (2, "203.104.209.87")
      , (3, "125.6.184.215")
      , (4, "203.104.209.183")
      , (5, "203.104.209.150")
      , (6, "203.104.209.134")
      , (7, "203.104.209.167")
      , (8, "203.104.209.199")
      , (9, "125.6.189.7")
      , (10, "125.6.189.39")
      , (11, "125.6.189.71")
      , (12, "125.6.189.103")
      , (13, "125.6.189.135")
      , (14, "125.6.189.167")
      , (15, "125.6.189.215")
      , (16, "125.6.189.247")
      , (17, "203.104.209.23")
      , (18, "203.104.209.39")
      , (19, "203.104.209.55")
      , (20, "203.104.209.102")
      ]

    getMapsFromServer =
      IM.map serverAddrToIp . parseServerInfo . T.unpack . decodeUtf8 . BSL.toStrict
        <$> simpleReq mgr "http://203.104.209.7/gadget_html5/js/kcs_const.js"

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let fetchVersionData :: String -> IO (Maybe (M.Map String String))
      fetchVersionData ipAddr = do
        let resourceUri = "http://" <> ipAddr <> "/kcs2/version.json"
        raw <- simpleReq mgr resourceUri
        pure (decode' raw)
  sMaps <- getServerMaps mgr
  asyncTasks <- IM.elems <$> mapM (async . fetchVersionData) sMaps
  (errs, results) <- partitionEithers <$> mapConcurrently waitCatch asyncTasks
  if null results
    then do
      n <- checkNetwork
      putStrLn $ "Network connection: " <> show n
    else
      print (let (a:as) = results in all (==a) as)
  mapM_ print errs
