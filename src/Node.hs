module Node (
    writeBytes
,   readBytes
) where


import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Int
import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Control.Applicative
import Foreign.Storable
import Index


stdNodePath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/node_initial._ticket"

stdRelsPath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/rels_initial._ticket"



injectNPrototype :: NodePrototype -> IO (Int32)
injectNPrototype (NodePrototype root rels)
    | root == "" = return (-1 :: Int32)
    | otherwise = do
        nodh <- openBinaryFile stdNodePath ReadWriteMode
        hSeek nodh SeekFromEnd 0
        appendRoot nodh root $ mapM (\x -> appendPlaceholder nodh x) rels


appendRoot :: Handle -> String -> IO [Int32] -> IO (Int32)
appendRoot nodh str rels =
    let bstr = B.pack str
    in lookupStdIdx bstr >>= \lookupRes ->
    case lookupRes of
        (-1) -> openRelAtEnd >>= \relh -> join ((evaluate . fromInteger) <$> (hTell relh)) >>= \eval ->
               writeRels relh rels >> hClose relh >> writeRoot nodh eval >>= \pos ->
               pushStdIdx (IndexPrototype bstr pos) >> return pos


    where writeRoot :: Handle -> Int32 -> IO Int32
          writeRoot h pos =
              writeBytes 4 h pos >> join ((evaluate . (4 -) . fromInteger) <$> (hTell h)) >>= return

          writeRels :: Handle -> IO [Int32] -> IO ()
          writeRels h rels =
              let relLen = L.length <$> rels
              in relLen >>= writeBytes 1 h >> rels >>= mapM_ (\x -> (writeBytes 4 h) x)

          openRelAtEnd :: IO Handle
          openRelAtEnd = openBinaryFile stdRelsPath ReadWriteMode >>= \h -> hSeek h SeekFromEnd 0 >>
                         return h


appendPlaceholder :: Handle -> String -> IO (Int32)
appendPlaceholder h str =
    let bstr = B.pack str
    in lookupStdIdx bstr >>= \lookupRes ->
    case lookupRes of
        (-1) -> join ((evaluate . fromInteger) <$> (hTell h)) >>= \eval -> writeBytes 4 h (0 :: Int32) >>
               pushStdIdx (IndexPrototype bstr eval) >> return eval
        indx -> return indx



