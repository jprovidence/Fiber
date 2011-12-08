module Index (
    IndexTree(IndexTree)
,   IndexPrototype(IndexPrototype)
,   pushIndex
,   indexLookup
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary.Put
import Data.Word
import Data.List as L
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable


type ByteString = B.ByteString

type NodeIndex = Word32

type Node = Word32

data IndexTpl = Tpl Word8 (Maybe Word32) (Maybe Word32)

data IndexTree = IndexTree ByteString [IndexTree] (Maybe NodeIndex)

data IndexPrototype = IndexPrototype ByteString NodeIndex

data Ternary a = ReadOnly a
                | Vertical a
                | Horizontal a


maxFilePos = 4294967295

stdIdxPath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/index_initial._ticket"


----------------------------------------------------------------------------------------------------

-- STANDARD DISK INDEX --

----------------------------------------------------------------------------------------------------

-- Handles much the cruft of opening/closing + setting binary mode on the file
-- Delegates the updating of the file given the list of index prototypes to #updateIndexGiven

pushStdIdx :: IndexPrototype -> ()
pushStdIdx (IndexPrototype bstr ni)
    | (B.null bstr) /= True && ((L.head "h") == (B.head bstr)) == True = unsafePerformIO $ do
        h <- openFile stdIdxPath ReadWriteMode
        hSetBinaryMode h True
        _ <- updateIndexGiven (IndexPrototype bstr ni) h 0 (ReadOnly 0)
        hClose h
    | otherwise = unsafePerformIO $ do
        putStrLn "_error_psi: Non-standard URL, ignoring..."


----------------------------------------------------------------------------------------------------

-- This function takes an IndexPrototype and commits it to the actual index store, stored on a flat
-- file. Given the fig 1. & fig 2, this function works as follows

--   Indexing the urls "www.google.com", "www.hoogle.com" and "www.hammer.com"
--
--                                           w           --
--                                           |             |- each of these is an 'index level'
--                                           w           --
--                                           |
--                                           w
--                                           |
--                                           .
--                                           |
--                                           g-->h
--                                           |   |
--                                           o   o-->a   -- an 'index level' indexing multiple
--                                           .   .   |      characters
--                                           .   .   m
--                                           .   .   .
--                                           |   |   .
--                                           o   o   .
--                                           |   |   |
--                                           m   m   m
--                                           |   |   |
--                                           $   $   $
--                                           |   |   |
--                                      (Node)(Node)(Node)
--
--   This is stored on the files in 'IndexTriples' which consist of a grouping Char Word32a Word32b
--   Char,    the character indexed at this position
--   Word32a, the file position of the beginning of the next index level
--   Word32b, the file position of the next char indexed at this index level
--
--   As such, the beginning of this index on disk word look like:
--
--  ['w', 10, 1] ['w', 19, 1] ['w', 28, 1] ['.', 0, 1] ['g', 0, 1] ['o', 0, 1] ['o', 0, 1] ['g', 0, 1]
--    1   2   6   10   11  15   19  20 24  28   29 33   37  38 42   46  47 51   55  56 60  64   65 69

--  ['l', 0, 1] ['e', 0, 1] ['.', 0, 1] ['c', 0, 1] ['o', 0, 1] ['m', 0, 1]


updateIndexGiven :: IndexPrototype -> Handle -> Int -> Ternary Int -> IO ()

updateIndexGiven (IndexPrototype bstr ni) h strPos (ReadOnly filePos) =
    updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1)
                     (seekToDollar h (toInteger filePos) (B.index bstr strPos))

updateIndexGiven (IndexPrototype bstr ni) h strPos (Horizontal filePos) = do
    writePos <- startCleanBlock h (B.index bstr strPos)
    updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1) (Vertical $ backtrack 4 h)
    linkToPrevious h filePos writePos

updateIndexGiven (IndexPrototype bstr ni) h strPos (Vertical filePos) = do
    writePos <- startCleanBlock h (B.index bstr strPos)
    updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1) (Vertical $ backtrack 8 h)
    linkToPrevious h filePos writePos

startCleanBlock :: Handle -> Char -> IO (Int)
startCleanBlock h cha = do
    hSeek h SeekFromEnd 0
    writePos <- liftM fromInteger (hTell h)
    writeBytes 1 h cha
    writeBytes 4 h (0 :: Int)
    writeBytes 4 h (1 :: Int)
    return $ writePos

linkToPrevious :: Handle -> Int -> Int -> IO ()
linkToPrevious h filePos writePos = do
    hSeek h AbsoluteSeek (toInteger filePos)
    writeBytes 4 h (writePos :: Int)


writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj = do
    ptr <- mallocBytes nBytes
    poke ptr obj
    hPutBuf h ptr nBytes
    free ptr


readBytes :: (Storable a) => Int -> Handle -> a
readBytes nBytes h = unsafePerformIO $ do
    ptr <- mallocBytes nBytes
    hGetBuf h ptr nBytes
    ret <- peek ptr
    free ptr
    return $ ret


backtrack :: Int -> Handle -> Int
backtrack i h = (\x -> x - i) . fromInteger . unsafePerformIO $ hTell h


seekToDollar :: Handle -> Integer -> Char -> Ternary Int
seekToDollar h fpos cha
    | fpos == 1 = Horizontal ((fromInteger $ unsafePerformIO $ hTell h) - 4)
    | otherwise = unsafePerformIO $ do
        hSeek h AbsoluteSeek fpos
        derefd <- return $ readBytes 1 h
        case derefd == cha of
            True -> do
                return $ ReadOnly (readBytes 4 h)
            False -> do
                hSeek h RelativeSeek 4
                newPos <- return $ (readBytes 4 h :: Int)
                return $ seekToDollar h (toInteger newPos) cha




----------------------------------------------------------------------------------------------------

-- FLOATING INDEX --

----------------------------------------------------------------------------------------------------

-- delegates floating index lookup to #iLookup providing default parameters. This removes the need
-- to remember to supply these each time

indexLookup :: IndexTree -> ByteString -> (Maybe NodeIndex)
indexLookup tree str = iLookup tree str 0


----------------------------------------------------------------------------------------------------

-- loads IndexTrees recursively based on the ByteString argument. Either
    -- A : The ByteString will unravel, and the IndexTree reached will contain the NodeIndex
    -- B : The ByteString will unravel, and the IndexTree reached will not contain the NodeIndex
    -- C : The ByteString will specify a non-existent path through the IndexTree, in which case
    --     Nothing will be the value of the func

iLookup :: IndexTree -> ByteString -> Int -> (Maybe NodeIndex)
iLookup (IndexTree tbyte ttree tni) str pos
    | (B.length str) == pos = tni
    | otherwise = case (B.index str pos) `B.elem` tbyte of
                      True -> iLookup (ttree !! (elemIdx str tbyte pos)) str (pos + 1)
                      False -> Nothing


----------------------------------------------------------------------------------------------------

-- Delegates insertion into a floating index to #push, specifing default arguments

pushIndex :: IndexTree -> IndexPrototype -> IndexTree
pushIndex tree proto = push tree proto 0


----------------------------------------------------------------------------------------------------

-- Recursively traverses the IndexTree based on the IndexPrototype ByteString to insert the
-- IndexPrototype value. When the path differs from existing paths, new IndexTree 'levels' are
-- created and linked into the existing structure

push :: IndexTree -> IndexPrototype -> Int -> IndexTree
push (IndexTree tbyte ttree tni) (IndexPrototype pbyte pni) idx
    | (B.length pbyte) == idx = (IndexTree tbyte ttree (Just pni))
    | otherwise = case (B.index pbyte idx) `B.elem` tbyte of
                      True -> (IndexTree tbyte (replace ttree
                                                         (elemIdx pbyte tbyte idx)
                                                         (push (ttree !! (elemIdx pbyte tbyte idx))
                                                               (IndexPrototype pbyte pni)
                                                               (idx + 1)))
                                          tni)
                      False -> (IndexTree (tbyte `B.snoc` (B.index pbyte idx))
                                           (ttree ++ [(push (IndexTree B.empty [] Nothing)
                                                           (IndexPrototype pbyte pni)
                                                           (idx + 1))])
                                           tni)

    where replace :: [IndexTree] -> Int -> IndexTree -> [IndexTree]
          replace lst idx other = let (x, _:ys) = L.splitAt idx lst
                                  in x ++ other : ys


----------------------------------------------------------------------------------------------------

-- Given ByteString a & b and int i, this func finds the elem at index i in ByteString a, then
-- returns the index of this element in ByteString b

elemIdx :: ByteString -> ByteString -> Int -> Int
elemIdx pbyte tbyte idx = fromJust $ B.elemIndex (B.index pbyte idx) tbyte

