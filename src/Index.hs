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
                writeTrip h (L.head "x") (0 :: Int) (1 :: Int)
                updateIndex (IndexPrototype bstr ni) h 0 (ReadOnly 0)
            False -> do
                updateIndex (IndexPrototype bstr ni) h 0 (ReadOnly 0)
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
    | strPos == (B.length bstr) =
        case ter of
            (Vertical v) -> do
                writePos <- liftM (unsafePerformIO . evaluate) $ getEnd h
                writeTrip h (L.head "$") ni (1 :: Int32)
                hSeek h AbsoluteSeek $ toInteger v
                writeBytes 4 h writePos
            (ReadOnly ro) -> do
                dollarFound <- moveToDollar h ro
                if dollarFound == True
                    then writeBytes 4 h ni
                    else return ()
    | otherwise =
        let curChar = B.index bstr $ fromIntegral strPos
        in case ter of
            (ReadOnly fPos) -> do
                pType <- stepLevel h (toInteger fPos) curChar
                case pType of
                    (ReadOnly res) -> updateIndex (IndexPrototype bstr ni) h (strPos + 1) pType
                    (Horizontal res) -> updateIndex (IndexPrototype bstr ni) h strPos pType
            (Horizontal fPos) -> do
                end <- liftM (unsafePerformIO . evaluate) (getEnd h)
                writeTrip h curChar (0 :: Int32) (1 :: Int32)
                updatePos <- backFour h
                updateIndex (IndexPrototype bstr ni) h (strPos + 1) (Vertical updatePos)
                hSeek h AbsoluteSeek (toInteger fPos)
                writeBytes 4 h end
            (Vertical fPos) -> do
                end <- liftM (unsafePerformIO . evaluate) (getEnd h)
                writeTrip h curChar (0 :: Int32) (1 :: Int32)
                updatePos <- backFour h
                updateIndex (IndexPrototype bstr ni) h (strPos + 1) (Vertical updatePos)
                hSeek h AbsoluteSeek $ toInteger fPos
                writeBytes 4 h end



    where getEnd :: Handle -> IO (Int32)
          getEnd h = hSeek h SeekFromEnd 0 >> hTell h >>= \x -> return $ fromInteger x

          backFour :: Handle -> IO (Int32)
          backFour h = liftM ((\x -> x - 4) . fromInteger . unsafePerformIO . evaluate) (hTell h)


writeTrip :: (Storable a, Storable b) => Handle -> a -> b -> b -> IO ()
writeTrip h a b c = do
    writeBytes 1 h a
    writeBytes 4 h b
    writeBytes 4 h c


stepLevel :: Handle -> Integer -> Char -> IO (Tertiary Int32)
stepLevel h fPos ch
    | fPos == 1 = do
        pos <- hTell h
        eval <- evaluate pos
        return $ Horizontal (fromInteger (pos - 4))
    | otherwise = do
        hSeek h AbsoluteSeek fPos
        x <- return $ readBytes 1 h
        case x == ch of
            True -> evaluate $ ReadOnly (readBytes 4 h)
            False -> do
                hSeek h RelativeSeek 4
                stepLevel h (toInteger $ ((readBytes 4 h) :: Int)) ch


moveToDollar :: Handle -> Int32 -> IO (Bool)
moveToDollar h i
    | i == 1 = return False
    | otherwise = do
        hSeek h AbsoluteSeek $ toInteger i
        x <- return $ readBytes 1 h
        case x == (L.head "$") of
            True -> return True
            False -> hSeek h RelativeSeek 4 >> return (readBytes 4 h) >>= \y -> moveToDollar h y


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

