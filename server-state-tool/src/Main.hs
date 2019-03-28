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

checkNetwork :: IO Bool
checkNetwork =
    catch checkGoogle (\(_ :: SomeException) -> pure False)
  where
    checkGoogle =  do
      mgr <- newManager tlsManagerSettings
      req <- parseUrlThrow "http://www.google.com/"
      resp <- httpNoBody req mgr
      let hdrs = responseHeaders resp
      pure $! hdrs `deepseq` True

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let simpleReq url = do
        req <- parseUrlThrow url
        responseBody <$> httpLbs req mgr
      fetchVersionData :: String -> IO (Maybe (M.Map String String))
      fetchVersionData ipAddr = do
        let resourceUri = "http://" <> ipAddr <> "/kcs2/version.json"
        raw <- simpleReq resourceUri
        pure (decode' raw)
  -- TODO: we'll actually encounter connection issue here while
  -- for WhaleChan this step is performed on another thread
  x <- T.unpack . decodeUtf8 . BSL.toStrict
    <$> simpleReq "http://203.104.209.7/gadget_html5/js/kcs_const.js"
  let sMaps = IM.map serverAddrToIp $ parseServerInfo x
  -- xs <- mapConcurrently _ sMaps
  asyncTasks <- IM.elems <$> mapM (async . fetchVersionData) sMaps
  (errs, results) <- partitionEithers <$> mapConcurrently waitCatch asyncTasks
  if null results
    then do
      n <- checkNetwork
      putStrLn $ "Network connection: " <> show n
    else
      print (let (a:as) = results in all (==a) as)
  mapM_ print errs
