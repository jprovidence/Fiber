module Input (

) where


{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}


import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Network.HTTP as H
import Network.URI
import Database.HDBC
import Database.HDBC.PostgreSQL
import Text.XML.HXT.Core
import Flow


type ByteString = B.ByteString


connStr = "host=localhost dbname=ticket user=postgres password=password"


crawlCycle :: (RealFrac a) => a -> IO ()
crawlCycle reps = do
    repPer <- return (reps / 10)
    chan <- newChan
    conn <- connectPostgreSQL connStr
    replicateM_ 10 (forkIO (repeatedCrawl (round repPer) conn chan))
    replicateM_ 10 (readChan chan)
    disconnect conn

    where repeatedCrawl :: Int -> Connection -> Chan Bool -> IO ()
          repeatedCrawl n conn chan = replicateM_ n (crawlOnce conn) >> writeChan chan True


crawlOnce :: Connection -> IO ()
crawlOnce conn =
    getNext conn >>= \url -> typeAndBody url >>= \tab ->
        case tab of
            (Nothing, Nothing) -> return ()
            (Just ctype, Just body) -> switchOnCtype ctype body url
            _ -> return ()


    where getNext :: Connection -> IO (String)
          getNext c = quickQuery' c "SELECT id FROM html ORDER BY id ASC LIMIT 1;" [] >>= \res ->
                      return ((res !! 0) !! 0) >>= return . M.fromJust . fromSql


switchOnCtype :: String -> String -> String -> IO ()
switchOnCtype ctype body url =
    return (B.pack ctype) >>= return . B.breakSubstring (B.pack "xml") >>= \(a, b) ->
        case b == B.empty of
            True -> runX (readHtml body >>> getLinks) >>= \[links] -> saveCrawlResults "html" links
            False -> saveCrawlResults "xml" [url]


saveCrawlResults :: String -> [String] -> IO ()
saveCrawlResults which ml = do
    conn <- connectPostgreSQL connStr
    case which of
        "xml" -> mapM_ (insertX "xml" conn) ml >> disconnect conn
        "html" -> mapM_ (insertX "html" conn) ml >> disconnect conn

    where insertX :: String -> Connection -> String -> IO ()
          insertX tbl conn x =
              prepare conn "INSERT INTO ? VALUES ('?');" >>= \s -> execute s [toSql tbl, toSql x] >>
              commit conn


typeAndBody :: String -> IO (Maybe String, Maybe String)
typeAndBody url = case parseURI url of
                      Nothing -> return (Nothing, Nothing)
                      Just uri -> get uri

    where get :: URI -> IO (Maybe String, Maybe String)
          get uri = H.simpleHTTP (H.Request uri H.GET [] "") >>= \eresp ->
                    case eresp of
                        Left _ -> return (Nothing, Nothing)
                        Right res -> return ((contentType (H.rspHeaders res)), Just (H.rspBody res))


contentType :: [H.Header] -> Maybe String
contentType hdrs = L.foldl' select Nothing hdrs

    where select :: Maybe String -> H.Header -> Maybe String
          select acc (H.Header name str) = case name of
                                              H.HdrContentType -> Just str
                                              _ -> acc




