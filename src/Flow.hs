module Flow (
    preprocess
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Char
import System.IO.Unsafe
import Control.Arrow
import Control.Monad
import Control.Exception
import Control.Applicative
import Network.HTTP
import Network.URI
import Text.XML.HXT.Core


type ByteString = B.ByteString


data NodePrototype = NodePrototype String [String]
    deriving Show


preprocess :: ByteString -> IO [NodePrototype]
preprocess bstr = do
    body <- retrieveData bstr
    --nada <- runX (readXml body >>> atTag "description")
    --putStrLn $ show $ nada
    res <- return $ prototype body
    (++) <$> (fst res) <*> (snd res)

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
parseIE sNode str = runX (readXml str >>> (linkAndDescriptions sNode)) >>= \tpls ->
    return (L.map (\(loc, bdy) -> NodePrototype (L.head loc) (linksIn $ L.head bdy)) tpls)

    where linksIn :: String -> [String]
          linksIn desc = unsafePerformIO $ runX(readHtml desc >>> getLinks) >>= \[links] -> return links


linkAndDescriptions :: ArrowXml a => String -> a XmlTree ([String], [String])
linkAndDescriptions sNode = atTag sNode >>>
    proc str -> do
        descs <- listA (atTag "description" >>> text) -< str
        links <- arr combine <<< origLinks &&& plainLinks -< str
        returnA -< (links, descs)

    where origLinks :: ArrowXml a => a XmlTree [String]
          origLinks = listA (atLocalTag "origLink" >>> text)

          plainLinks :: ArrowXml a => a XmlTree [String]
          plainLinks = listA (atTag "link" >>> text)

          combine :: ([String], [String]) -> [String]
          combine (a, b) = case L.length a == 0 of
                               True -> b
                               False -> a


getLinks :: ArrowXml a => a XmlTree [String]
getLinks = proc str -> do
    returnA <<< listA (atTagCase "a" >>> getAttrValue "href") -< str


readXml :: String -> IOStateArrow s b XmlTree
readXml = readString [withValidate no]


readHtml :: String -> IOStateArrow s b XmlTree
readHtml = readString [withValidate no, withParseHTML yes, withWarnings no]


atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)


atLocalTag :: ArrowXml a => String -> a XmlTree XmlTree
atLocalTag tag = deep (isElem >>> hasNameWith ((== tag) . localPart))


atTagCase :: ArrowXml a => String -> a XmlTree XmlTree
atTagCase tag = deep (isElem >>> hasNameWith ((== upTag) . upper . localPart))

    where upTag :: String
          upTag = upper tag

          upper :: String -> String
          upper = map toUpper


text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText



