module Main (
    main
) where

import Index
import Flow
import Node
import Input
import qualified Data.ByteString.Char8 as B

main = do
    testStdIndex
    testNodeInject
    testFlow
    testIndexPush


testFlow :: IO ()
testFlow = do
    test1 <- preprocess (B.pack "http://feeds.feedburner.com/zerohedge/feed")
    putStrLn $ show test1
    putStrLn "\n"
    test2 <- preprocess (B.pack "http://feeds.feedburner.com/oilpricecom?fmt=xml")
    putStrLn $ show test2
    putStrLn "\n_test: Flow working correctly"


testNodeInject :: IO ()
testNodeInject = do
    proto1 <- return (Node "http://www.test.com"
                         ["http://www.rel1.com", "http://www.rel2.com", "http://www.rel3.com"])
    injectNPrototype proto1
    (Node res1 lst1) <- relationshipsOn $ B.pack "http://www.test.com"

    proto2 <- return (Node "http://www.rel1.com"
                         ["http://www.oth1.com", "http://www.oth2.com", "http://www.oth3.com"])
    injectNPrototype proto2
    (Node res2 lst2) <- relationshipsOn $ B.pack "http://www.rel1.com"

    putStrLn $ "Fst Node Result: " ++ (show res1) ++ " " ++ (show lst1)
    putStrLn $ "Sec Node Result: " ++ (show res2) ++ " " ++ (show lst2)
    if res2 == (lst1 !! 0)
        then putStrLn "_test: Node Injection appears to be functioning.\n"
        else putStrLn "_test _ERR_ Node Injection not functioning.\n"


testStdIndex :: IO ()
testStdIndex = do
    pushStdIdx (IndexPrototype (B.pack "http://www.test.com/1123") 10)
    pushStdIdx (IndexPrototype (B.pack "http://www.test.com/11") 5)
    pushStdIdx (IndexPrototype (B.pack "http://www.test.com/1") 12)
    pushStdIdx (IndexPrototype (B.pack "http://www.teqst.com") 1)
    pushStdIdx (IndexPrototype (B.pack "http://www.mock.com/2") 14)
    res <- lookupStdIdx $ B.pack "http://www.test.com/1"
    if res == 12
        then putStrLn "_test: File Index Lookup 1 Successful"
        else putStrLn "_test: _ERR_ File Index Lookup Unsuccessful"
    res <- lookupStdIdx $ B.pack "http://www.mock.com/2"
    if res == 14
        then putStrLn "_test: File Index Lookup 2 Successful"
        else putStrLn "_test: _ERR_ File Index Lookup Unsuccessful"



testProto1 = IndexPrototype (B.pack "www.test1.com/test/1") 50
testProto2 = IndexPrototype (B.pack "www.test2.com/other/2") 100
testIndex = IndexTree B.empty [] Nothing

testIndexPush :: IO ()
testIndexPush =
    let postPush1 = pushIndex testIndex testProto1
        postPush2 = pushIndex postPush1 testProto2
    in case indexLookup postPush2 (B.pack "www.test2.com/other/2") of
           Just _ -> putStrLn "_test: Index Lookup Successful"
           Nothing -> putStrLn "_test: _ERR_ Index Lookup Unsuccessful"
