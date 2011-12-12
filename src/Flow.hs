module Flow (
    retrieveData
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Char
import System.IO.Unsafe
import Control.Arrow
import Control.Monad
import Network.HTTP
import Network.URI
import Text.XML.HXT.Core


type ByteString = B.ByteString


data NodePrototype = NodePrototype String [String]



preprocess :: ByteString -> IO [NodePrototype]
preprocess bstr = do
    body <- retrieveData bstr
    res <- return $ prototype body
    lstA <- fst res
    lstB <- snd res
    case L.length lstA == 0 of
        True -> return lstB
        False -> return lstA

    where prototype :: String -> (IO [NodePrototype], IO [NodePrototype])
          prototype = proc str -> do
          returnA <<< arr (parseIE "entry") &&& arr (parseIE "item") -< str



retrieveData :: ByteString -> IO (String)
retrieveData url = case parseURI (B.unpack url) of
                       Nothing -> return "x"
                       Just uri -> get uri

    where get :: URI -> IO (String)
          get uri = simpleHTTP (Request uri GET [] "") >>= \resp ->
                    case resp of
                        Left _ -> return "x"
                        Right res -> return $ rspBody res


parseIE :: String -> String -> IO [NodePrototype]
parseIE sNode str = runX (readXml str >>> (linkAndDescriptions sNode)) >>= \[(links, descs)] ->
    return $ L.map prototype $ L.zip links (L.map linksIn descs)

    where linksIn :: String -> [String]
          linksIn desc = unsafePerformIO $ runX(readHtml desc >>> getLinks) >>= \[links] -> return links

          prototype :: (String, [String]) -> NodePrototype
          prototype (a, b) = NodePrototype a b


linkAndDescriptions :: ArrowXml a => String -> a XmlTree ([String], [String])
linkAndDescriptions sNode = proc str -> do
    descs <- listA (atTag "description" >>> text) <<< atTag sNode -< str
    links <- arr combine <<< origLinks &&& plainLinks -< str
    returnA -< (links, descs)

    where origLinks :: ArrowXml a => a XmlTree [String]
          origLinks = listA (atTag "origLink" >>> text)

          plainLinks :: ArrowXml a => a XmlTree [String]
          plainLinks = listA (atTag "link" >>> text)

          combine :: ([String], [String]) -> [String]
          combine (a, b) = L.map (\(l1, l2) -> l1 ++ l2) (L.zip a b)


getLinks :: ArrowXml a => a XmlTree [String]
getLinks = proc str -> do
    returnA <<< listA (atTagCase "a" >>> text) -< str


readXml :: String -> IOStateArrow s b XmlTree
readXml = readString [withValidate no]


readHtml :: String -> IOStateArrow s b XmlTree
readHtml = readString [withValidate no, withParseHTML yes, withWarnings no]


atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)


atTagCase :: ArrowXml a => String -> a XmlTree XmlTree
atTagCase tag = deep (isElem >>> hasNameWith ((== upTag) . upper . localPart))

    where upTag :: String
          upTag = upper tag

          upper :: String -> String
          upper = map toUpper


text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText



