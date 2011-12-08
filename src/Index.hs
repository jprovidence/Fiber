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
import Foreign.Marshal.Alloc
import Foreign.Storable


type ByteString = B.ByteString

type NodeIndex = Word32

type Node = Word32

data IndexTpl = Tpl Word8 (Maybe Word32) (Maybe Word32)

data IndexTree = IndexTree ByteString [IndexTree] (Maybe NodeIndex)

data IndexPrototype = IndexPrototype ByteString NodeIndex


maxFilePos = 4294967295

stdIdxPath = "/home/providence/Dropbox/_ticket/haskell_devel/fiber/fiber/data/index_initial._ticket"


----------------------------------------------------------------------------------------------------

-- STANDARD DISK INDEX --

----------------------------------------------------------------------------------------------------

pushStdIdx :: IndexPrototype -> ()
pushStdIdx (IndexPrototype bstr ni)
    | (B.null bstr) /= True && ((L.head "h") == (B.head bstr)) == True = unsafePerformIO $ do
        h <- openFile stdIdxPath ReadWriteMode
        hSetBinaryMode h True
        _ <- updateIndexGiven (IndexPrototype bstr ni) h 0 (Just 0)
        hClose h
    | otherwise = unsafePerformIO $ do
        putStrLn "_error_psi: Non-standard URL, ignoring..."


updateIndexGiven :: IndexPrototype -> Handle -> Int -> Maybe Int -> IO ()

updateIndexGiven (IndexPrototype bstr ni) h strPos (Just filePos) = do
    updateIndexGiven (IndexPrototype bstr ni) h (strPos + 1)
                     (seekToDollar h (toInteger filePos) (B.index bstr strPos))


seekToDollar :: Handle -> Integer -> Char -> Maybe Int
seekToDollar h fpos cha
    | fpos == 1 = Nothing
    | otherwise = unsafePerformIO $ do
        ptr <- mallocBytes 1
        hSeek h AbsoluteSeek fpos
        _ <- hGetBuf h ptr 1
        derefd <- peek ptr
        case derefd == cha of
            True -> do
                free ptr
                ptr <- mallocBytes 4
                _ <- hGetBuf h ptr 4
                ret <- peek ptr
                free ptr
                return $ Just ret
            False -> do
                free ptr
                ptr <- mallocBytes 4
                hSeek h RelativeSeek 4
                _ <- hGetBuf h ptr 4
                ret <- peek ptr :: IO (Int)
                free ptr
                return $ seekToDollar h (toInteger ret) cha









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

