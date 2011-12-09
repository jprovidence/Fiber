module Main (
    main
) where

import Index
import qualified Data.ByteString.Char8 as B

main = do
    pushStdIdx (IndexPrototype (B.pack "http://www.test.com/1") 12)
    pushStdIdx (IndexPrototype (B.pack "http://www.mock.com/2") 14)
    res <- lookupStdIdx $ B.pack "http://www.test.com/1"
    putStrLn $ show res
    if res == 12
        then putStrLn "_test: File Index Lookup 1 Successful"
        else putStrLn "_test: _ERR_ File Index Lookup Unsuccessful"
    res <- lookupStdIdx $ B.pack "http://www.mock.com/2"
    if res == 14
        then putStrLn "_test: File Index Lookup 2 Successful"
        else putStrLn "_test: _ERR_ File Index Lookup Unsuccessful"
    bool <- testIndexPush
    if bool == True
        then putStrLn "_test: Index Lookup Successful"
        else putStrLn "_test: _ERR_ Index Lookup Unsuccessful"


testProto1 = IndexPrototype (B.pack "www.test1.com/test/1") 50
testProto2 = IndexPrototype (B.pack "www.test2.com/other/2") 100

testIndex = IndexTree B.empty [] Nothing

testIndexPush :: IO (Bool)
testIndexPush =
    let postPush1 = pushIndex testIndex testProto1
        postPush2 = pushIndex postPush1 testProto2
    in case indexLookup postPush2 (B.pack "www.test2.com/other/2") of
           Just _ -> return True
           Nothing -> return False
