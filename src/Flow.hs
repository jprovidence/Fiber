module Flow (
    retrieveData
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import System.IO.Unsafe
import Control.Arrow
import Network.HTTP
import Network.URI
import Text.XML.HXT.Core


type ByteString = B.ByteString


extractLinks :: ByteString -> [String]
extractLinks = proc url -> do
     returnA <<< arr (\(a, b) -> a ++ b) <<< parseItem &&& parseEntry <<< retrieveData -< url


    where parseItem :: IO (String) -> [String]
          parseItem = proc unstr -> do
             str <- arr unsafePerformIO -< unstr
             returnA -< str

          parseEntry :: IO (String) -> [String]
          parseEntry = proc str -> do
              returnA [str]


retrieveData :: ByteString -> IO (String)
retrieveData = proc str -> do
    parsed <- (arr (parseURI . B.unpack)) -< str
    body <- arr get -< parsed
    returnA -< body

    where get :: Maybe URI -> IO (String)
          get Nothing = return "x"
          get (Just uri) = do
              resp <- simpleHTTP (Request uri GET [] "")
              case resp of
                  Left _ -> putStrLn ("_error_g: Error with http request") >> return "x"
                  Right res -> return $ rspBody res




