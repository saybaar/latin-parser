> module Main where

> import qualified Data.ByteString as B
> import Text.XML.Light
> import Network.Download
> import Data.List
> import IOActions
> import LatinTypes
> import Prelude hiding (Word)

> main = getLine
>        >>= \x -> return ("http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup=" ++ x)
>        >>= openURI
>        >>= inIO ((map makeWord) . (map (map getKeyValue)) . getAnalyses . parseEither)
>        >>= mapM_ print

parseEither :: Either String B.ByteString -> [Content]
-- something strange going on with ByteString type...

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

> findTag s obj = case lookup s obj of
>                      Just str -> str
>                      Nothing -> error (s ++ " not found in XML")
> findDef s dict = case lookup s dict of
>                      Just a -> a
>                      Nothing -> error (s ++ " not found in dict")
> findType s obj dict = findDef (findTag s obj) dict
> makeWord :: [(String,String)] -> Word
> makeWord obj = case findTag "pos" obj of
>                 "noun" -> Noun number gender gramcase
>                 "verb" -> Verb person number tense voice mood
>                 "adj"  -> Adj  number gender gramcase
>                 other -> error (other ++ " not yet supported...")
>     where number = findType "number" obj numberDict
>           gramcase = findType "case" obj caseDict
>           gender = findType "gender" obj genderDict
>           person = findType "person" obj personDict
>           tense = findType "tense" obj tenseDict
>           voice = findType "voice" obj voiceDict
>           mood = findType "mood" obj moodDict

> genderDict = [("masc",Masc),("fem",Fem),("neut",Neut)]
> numberDict = [("sg",Sg),("pl",Pl)]
> caseDict   = [("nom",Nom),("gen",Gen),("dat",Dat),("acc",Acc),("abl",Abl),("voc",Voc)]
> personDict = [("1st",First),("2nd",Second),("3rd",Third)]
> tenseDict  = [("pres",Pres),("fut",Fut),("imperf",Imperf),("perf",Perf),
>               ("plup",Pluperf),("futperf",Futperf)]
> voiceDict  = [("act",Act),("pass",Pass)]
> moodDict   = [("ind",Ind),("subj",Subj),("inf",Inf),("imperat",Imperat)]
> 


