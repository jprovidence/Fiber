--
--   A brief explanation of the functioning of this index:

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
--   When multiple characters are indexed on the same line a pointer to the next file position
--   is stored on the right-most position of the index triple:
--
--     ["a", 0, *15*]
--
--   Otherwise, there is a one in the right-most position
--
--     ["a", 0, *1*]
--
--   A pointer to the next level of the index is stored in the middle position
--
--     ["a", *82*, 1]
--
--   Finally, the NodeIndex is stored at the middle of a triple indexing a dollarsign
--
--     ["$", (NodeIndex), 1]


module Index (
    IndexTree(IndexTree)
,   IndexPrototype(IndexPrototype)
,   pushIndex
,   indexLookup
,   pushStdIdx
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary.Put
import Data.Word
import Data.List as L
import Data.Maybe
import Data.Int
import System.IO
import System.IO.Error
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Foreign.Marshal.Alloc
import Foreign.Storable


type ByteString = B.ByteString

type NodeIndex = Int32

type Node = Word32

data IndexTpl = Tpl Word8 (Maybe Word32) (Maybe Word32)

data IndexTree = IndexTree ByteString [IndexTree] (Maybe NodeIndex)

data IndexPrototype = IndexPrototype ByteString NodeIndex

data Tertiary a = ReadOnly a
                 | Vertical a
                 | Horizontal a


-- File position after which a new index file is started
maxFilePos = 4294967295

-- Location of the index file on disk
stdIdxPath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/index_initial._ticket"




----------------------------------------------------------------------------------------------------

-- STANDARD DISK INDEX --

----------------------------------------------------------------------------------------------------

-- Handles much the cruft of opening/closing + setting binary mode on the file
-- Delegates the updating of the file given the list of index prototypes to #updateIndexGiven

pushStdIdx :: IndexPrototype -> IO ()
pushStdIdx (IndexPrototype bstr ni)
    | (B.null bstr) /= True && ((L.head "h") == (B.head bstr)) == True = do
        h <- openBinaryFile stdIdxPath ReadWriteMode
        eof <- hIsEOF h
        case eof of
            True -> do
                startCleanBlock h (L.head "x")
                updateIndexGiven (IndexPrototype bstr ni) h 0 (ReadOnly 0)
            False -> do
                updateIndexGiven (IndexPrototype bstr ni) h 0 (ReadOnly 0)
        hClose h
    | otherwise = do
        putStrLn "_error_psi: Non-standard URL, ignoring..."


----------------------------------------------------------------------------------------------------

-- This function takes an IndexPrototype and commits it to the actual index store.
-- It will follow the existing index as far as possible by recursing, passing a 'ReadOnly' filePos.
--
-- When needed, it will deviate from the existing index signifying the need to do this by passing
-- a 'Horizontal' file position to the next level of recursion. This position indicates the triple
-- that needs to be updated with the write position of the new character introduced to that index
-- level.
--
-- As it adds new index levels it will recurse, providing a Vertical file position for the next
-- iteration to update with its write position

updateIndex :: IndexPrototype -> Handle -> Int -> Tertiary Int32 -> IO ()
updateIndex (IndexPrototype bstr ni) h strPos ter
    | strPos == (B.length bstr) = do
        writePos <- hTell h
        writeBytes 1 h (L.head "$")
        writeBytes 4 h ni
        writeBytes 4 h (1 :: Int32)
        hSeek h AbsoluteSeek $ tertiaryVal ter
        writeBytes 4 h ((fromInteger writePos) :: Int32)
    | otherwise = do
        let curChar = B.index bstr $ fromIntegral strPos
        case ter of
            (ReadOnly fPos) -> updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1)
                                                (stepLevel h (toInteger fPos) curChar)
            (Horizontal fPos) -> do
                end <- return $ liftM (unsafePerformIO . evaluate) (getEnd h)
                writeBytes 1 h curChar
                writeBytes 4 h 0
                writeBytes 4 h 1
                hSeek fPos
                writeBytes 4 h end


    where tertiaryVal :: Tertiary Int32 -> Integer
          tertiaryVal (Vertical i) = toInteger i

          getEnd :: Handle -> IO (Int32)
          getEnd h = hSeek h SeekFromEnd 0 >> hTell h >>= \x -> return $ fromInteger x


stepLevel :: Handle -> Integer -> Char -> Tertiary Int32
stepLevel h fPos ch
    | fPos == 1 = unsafePerformIO $ do
        pos <- hTell h
        eval <- evaluate pos
        return $ Horizontal (fromInteger (pos - 4))
    | otherwise = unsafePerformIO $ do
        hSeek h AbsoluteSeek fPos
        x <- return $ readBytes 1 h
        case x == ch of
            True -> evaluate $ ReadOnly (readBytes 4 h)
            False -> return $ stepLevel h (toInteger $ readBytes 4 h) ch



updateIndexGiven :: IndexPrototype -> Handle -> Int32 -> Tertiary Int32 -> IO ()
updateIndexGiven (IndexPrototype bstr ni) h strPos tert
    | fromIntegral strPos == B.length bstr = do
        writePos <- startCleanBlock h (L.head "$")
        back <- evaluate $ backtrack 8 h
        hSeek h AbsoluteSeek $ toInteger back
        writeBytes 4 h ni
        t <- evaluate $ tertiaryVal tert
        hSeek h AbsoluteSeek t
        writeBytes 4 h (writePos :: Int32)
    | otherwise = do
        case tert of
            (ReadOnly filePos) -> updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1)
                                                   (seekToDollar h (toInteger filePos)
                                                                   (B.index bstr $ fromIntegral strPos))
            (Horizontal filePos) -> do
                writePos <- startCleanBlock h (B.index bstr $ fromIntegral strPos)
                eval <- evaluate filePos
                updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1) (Vertical $ backtrack 4 h)
                linkToPrevious h eval writePos
            (Vertical filePos) -> do
                writePos <- startCleanBlock h (B.index bstr $ fromIntegral strPos)
                eval <- evaluate filePos
                updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1) (Vertical $ backtrack 8 h)
                linkToPrevious h eval writePos

    where tertiaryVal :: Tertiary Int32 -> Integer
          tertiaryVal (Vertical i) = toInteger i


----------------------------------------------------------------------------------------------------

-- Seek to the end of the file, write a blank triple given the Char cha

startCleanBlock :: Handle -> Char -> IO (Int32)
startCleanBlock h cha = do
    hSeek h SeekFromEnd (toInteger 0)
    writePos <- liftM fromInteger (hTell h)
    writeBytes 1 h cha
    writeBytes 4 h (0 :: Int32)
    writeBytes 4 h (1 :: Int32)
    return $ writePos


----------------------------------------------------------------------------------------------------

-- writes a link to the current IndexTriple at the previous triple

linkToPrevious :: Handle -> Int32 -> Int32 -> IO ()
linkToPrevious h filePos writePos = do
    hSeek h AbsoluteSeek (toInteger filePos)
    writeBytes 4 h (writePos :: Int32)


----------------------------------------------------------------------------------------------------

-- Writes an object @obj@ of size @nBytes@ to the file handle @h@

writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj = do
    ptr <- mallocBytes nBytes
    poke ptr obj
    hPutBuf h ptr nBytes
    free ptr


----------------------------------------------------------------------------------------------------

-- Reads an object of size @nBytes@ from the file @h@. File cursor must be set by caller

readBytes :: (Storable a) => Int -> Handle -> a
readBytes nBytes h = unsafePerformIO $ do
    ptr <- mallocBytes nBytes
    hGetBuf h ptr nBytes
    ret <- peek ptr
    free ptr
    return $ ret


----------------------------------------------------------------------------------------------------

-- find the file position located @i@ bytes before the current position. Does not actually move
-- the file cursor

backtrack :: Int32 -> Handle -> Int32
backtrack i h = (\x -> x - i) . fromInteger . unsafePerformIO $ hTell h


----------------------------------------------------------------------------------------------------

-- checks all characters on an index level to see if they match the @cha@

seekToDollar :: Handle -> Integer -> Char -> Tertiary Int32
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

