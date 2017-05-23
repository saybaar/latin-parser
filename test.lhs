> import Text.XML.Light
> import Network.Download
> import Data.List
> import IOActions
> import qualified Data.ByteString as B
> import Latin
> import Prelude hiding (Word)

> main = getLine
>        >>= \x -> return ("http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup=" ++ x)
>        >>= openURI
>        >>= inIO ((map makeWord) . (map (map getKeyValue)) . getAnalyses . parseEither)
>        >>= mapM_ print

> parseEither :: Either String B.ByteString -> [Content]
> parseEither x = case x of
>                   Left s   -> error ("Error in initial XML parse: " ++ s)
>                   Right bs -> parseXML bs

> getAnalyses :: [Content] -> [[Element]]
> getAnalyses es = case findElement (QName "analyses" Nothing Nothing) (head . tail $ onlyElems es) of
>                   Nothing -> error "analyses not found"
>                   Just e -> (map (onlyElems . getContentList) . elChildren) e
> getContentList :: Element -> [Content]
> getContentList (Element _ _ c _) = c

> getKeyValue :: Element -> (String,String)
> getKeyValue (Element (QName name uri prefix) attrs contents line) = case contents of
>                                                     (Text (CData kind string line)):others -> (name,string)
>                                                     [] -> (name,"")
>                                                     _ -> error "non-text data in element"

> finds s ps = case lookup s ps of
>                      Just str -> str
>                      Nothing -> error (s ++ " not found in XML")
> findt s d = case lookup s d of
>                      Just a -> a
>                      Nothing -> error (s ++ " not found in dict")
> findu s ps d = findt (finds s ps) d
> makeWord :: [(String,String)] -> Word
> makeWord ps = case finds "pos" ps of
>                 "noun" -> Noun (finds "form" ps) (findu "number" ps numberDict) (findu "gender" ps genderDict) (findu "case" ps caseDict)
>                 other -> error (other ++ " not yet supported...")

> genderDict = [("masc",Masc),("fem",Fem),("neut",Neut)]
> numberDict = [("sg",Sg),("pl",Pl)]
> caseDict   = [("nom",Nom),("gen",Gen),("dat",Dat),("acc",Acc),("abl",Abl),("voc",Voc)]
