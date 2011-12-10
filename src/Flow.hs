module Flow (
    retrieveData
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Network.HTTP
import Network.URI
import Text.XML.HXT.Core


type ByteString = B.ByteString


--extractLinks :: String -> [String]
--extractLinks body =

--    where atTag :: String -> String -> String
--          atTag


retrieveData :: ByteString -> IO (String)
retrieveData url =
    case parseURI $ B.unpack url of
        Nothing -> putStrLn ("_error_rd: Error parsing url" ++ (B.unpack url)) >> return "x"
        Just uri -> get uri


get :: URI -> IO (String)
get uri = do
    resp <- simpleHTTP (Request uri GET [] "")
    case resp of
        Left _ -> putStrLn ("_error_g : Error with http request") >> return "x"
        Right res -> return $ rspBody res




