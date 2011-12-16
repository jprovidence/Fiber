module Node (
    injectNPrototype
,   relationshipsOn
) where


import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Int
import System.IO
import Control.Monad
import Control.Exception
import Index


type ByteString = B.ByteString

-- path to initial node store
stdNodePath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/node_initial._ticket"

-- path to initial relationship store
stdRelsPath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/rels_initial._ticket"


----------------------------------------------------------------------------------------------------

--

relationshipsOn :: ByteString -> IO (Node Int32)
relationshipsOn bstr =
    lookupStdIdx bstr >>= \lookupRes ->
    case lookupRes of
        (-1) -> return Blank
        indx -> do
            getRelPos indx >>= readRels >>= return . (Node indx)

    where getRelPos :: Int32 -> IO Int32
          getRelPos ind =
              openBinaryFile stdNodePath ReadMode >>= \h -> hSeek h AbsoluteSeek (toInteger ind) >>
              readBytes 4 h >>= evaluate >>= \e -> hClose h >> return e

          readRels :: Int32 -> IO [Int32]
          readRels pos = do
              h <- openBinaryFile stdRelsPath ReadMode
              hSeek h AbsoluteSeek $ toInteger pos
              e <- readBytes 1 h >>= evaluate
              ret <- replicateM e (readBytes 4 h)
              hClose h
              return ret


----------------------------------------------------------------------------------------------------

-- Opens and seeks the node file, delegates actual file writing to #appendRoot

injectNPrototype :: Node String -> IO ()
injectNPrototype (Node root rels)
    | root == "" = putStrLn "_ticket: _ERR_ invalid root" >> return ()
    | otherwise = do
        nodh <- openBinaryFile stdNodePath ReadWriteMode
        hSeek nodh SeekFromEnd $ toInteger 0
        join (evaluate (mapM (appendPlaceholder nodh) rels)) >>= appendRoot nodh root
        hClose nodh


----------------------------------------------------------------------------------------------------

-- writes the node-root and its relationships to disk

appendRoot :: Handle -> String -> [Int32] -> IO ()
appendRoot nodh str rels =
    let bstr = B.pack str
    in lookupStdIdx bstr >>= \lookupRes ->
    case lookupRes of
        (-1) -> do
            eval <- writeRels rels
            pos <- writeRoot nodh eval
            pushStdIdx (IndexPrototype bstr pos)
        indx -> do
            hSeek nodh AbsoluteSeek $ toInteger indx
            exists <- readBytes 4 nodh
            case (exists :: Int32) of
                (-1) ->
                    hSeek nodh AbsoluteSeek (toInteger indx) >> writeRels rels >>= writeBytes 4 nodh
                _ -> return ()

    where writeRoot :: Handle -> Int32 -> IO Int32
          writeRoot h pos = writeBytes 4 h pos >> hTell h >>= evaluate . fromInteger >>= return

          writeRels :: [Int32] -> IO Int32
          writeRels rels = do
              h <- openRelAtEnd
              eval <- hTell h >>= evaluate .fromInteger
              writeBytes 1 h $ L.length rels
              mapM_ (\x -> writeBytes 4 h x) rels
              hClose h
              return eval

          openRelAtEnd :: IO Handle
          openRelAtEnd = do
              h <- openBinaryFile stdRelsPath ReadWriteMode
              hSeek h SeekFromEnd 0
              return h


----------------------------------------------------------------------------------------------------

-- writes the individual placeholder nodes into the node index, returning their position to be added
-- to the root's relationships

appendPlaceholder :: Handle -> String -> IO (Int32)
appendPlaceholder h str =
    let bstr = B.pack str
    in lookupStdIdx bstr >>= \lookupRes ->
    case lookupRes of
        (-1) -> hTell h >>= evaluate . fromInteger >>= \e -> writeBytes 4 h ((-1) :: Int32) >>
               pushStdIdx (IndexPrototype bstr e) >> return e
        indx -> return indx



