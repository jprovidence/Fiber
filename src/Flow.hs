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
import Node
import Index


type ByteString = B.ByteString


----------------------------------------------------------------------------------------------------

-- Preprocess the bytestring url into the series of articles + internal links it represents

preprocess :: ByteString -> IO [NodePrototype]
preprocess bstr =
    retrieveData bstr >>= \body -> if (body == "x")
                                    then return []
                                    else return (prototype body) >>= \res ->
                                         (++) <$> (fst res) <*> (snd res)

    where prototype :: String -> (IO [NodePrototype], IO [NodePrototype])
          prototype = proc str -> do
              returnA <<< arr (parseIE "entry") &&& arr (parseIE "item") -< str


----------------------------------------------------------------------------------------------------

-- Lookup the url provided via HTTP and return "x" or the web-page, dependent on the status of the
-- request

retrieveData :: ByteString -> IO (String)
retrieveData url = case parseURI (B.unpack url) of
                       Nothing -> return "x"
                       Just uri -> get uri

    where get :: URI -> IO (String)
          get uri = simpleHTTP (Request uri GET [] "") >>= \resp ->
                    case resp of
                        Left _ -> return "x"
                        Right res -> return $ rspBody res


----------------------------------------------------------------------------------------------------

-- Extracts the links to a articles in a feed, pairing them to a list of all links contained within
-- the 'description' tag of that article. This func is given a String "item" or "entry" to indicate
-- which element it should parse by.

parseIE :: String -> String -> IO [NodePrototype]
parseIE sNode str = runX (readXml str >>> (linkAndDescriptions sNode)) >>= \tpls ->
    return (L.map (\(loc, bdy) -> NodePrototype (L.head loc) (linksIn $ L.head bdy)) tpls)

    where linksIn :: String -> [String]
          linksIn desc = unsafePerformIO $ runX(readHtml desc >>> getLinks) >>= \[links] -> return links


----------------------------------------------------------------------------------------------------

-- Given a tag "item" or "entry" this function returns a pairing of article links and descriptions
-- for further processing by #parseIE

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


----------------------------------------------------------------------------------------------------

-- An arrow which takes an xml tree, and extracts all href attributes in <a href="www..."> </a> tags.

getLinks :: ArrowXml a => a XmlTree [String]
getLinks = proc str -> do
    returnA <<< listA (atTagCase "a" >>> getAttrValue "href") -< str


----------------------------------------------------------------------------------------------------

-- takes a String representing an XML document and produces an XmlTree. Must be used in IOStateArrow
-- context.

readXml :: String -> IOStateArrow s b XmlTree
readXml = readString [withValidate no]


----------------------------------------------------------------------------------------------------

-- the same as #readXml, except for HTML strings

readHtml :: String -> IOStateArrow s b XmlTree
readHtml = readString [withValidate no, withParseHTML yes, withWarnings no]


----------------------------------------------------------------------------------------------------

-- when given a String representing a tag, returns an arrow from XmlTree -> XmlTree which produces
-- an XmlTree shortened to only those nodes below the tag given

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)


----------------------------------------------------------------------------------------------------

-- same as #atTag, but with namespaces ignored

atLocalTag :: ArrowXml a => String -> a XmlTree XmlTree
atLocalTag tag = deep (isElem >>> hasNameWith ((== tag) . localPart))


----------------------------------------------------------------------------------------------------

-- same as #atTag, but with namespaces & case ignored

atTagCase :: ArrowXml a => String -> a XmlTree XmlTree
atTagCase tag = deep (isElem >>> hasNameWith ((== upTag) . upper . localPart))

    where upTag :: String
          upTag = upper tag

          upper :: String -> String
          upper = map toUpper


----------------------------------------------------------------------------------------------------

-- an arrow from XmlTree -> String, that returns the text of any child nodes of the input tree

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

