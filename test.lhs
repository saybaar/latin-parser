> module Main where

> import qualified Data.ByteString as B
> import Text.XML.Light
> import Network.Download
> import Data.List
> import IOActions
> import LatinTypes
> import Prelude hiding (Word)
> import AUG

> main = do
>        l     <- getLine -- String
>        ws    <- (inIO words) l
>        trees <- mapM wordToTree ws
>        (putStr . unlines . map drawTTree . map treeFilter . fastTtrees) trees

wordToTree uses wordParses to get the list of possible Words a string could represent, then
calls wordTypesNew to get all the possible Types those words could take (Atom or O x y z)
and packages them into a TTree. 

> wordToTree  :: String -> IO (TTree)
> wordToTree s = do         
>                ps   <- wordParses s            -- [Word]
>                ts   <- (inIO wordTypesNew) ps  -- [Type]
>                return (TreeAtom s, ts)         -- TTree

wordParses looks up the given string on Perseus Hopper and parses the XML response into a list of
Word types. 

> wordParses   :: String -> IO [Word] 
> wordParses  x = return x
>                 >>= \x -> return ("http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup=" ++ x)
>                 >>= openURI
>                 >>= inIO ( nub . map makeWord . map (map getKeyValue) . getAnalyses . parseEither) 

fastTtrees will return all possible types for a word or phrase, even the O x y z function types - of
which there are a huge number, especially for nouns. treeFilter limits the output to Atom types, which
are the "complete" types we want to see. 

> treeFilter :: TTree -> TTree
> treeFilter (a,ts) = (a, filter atomType ts)
>   where atomType t = case t of
>           Atom w ->  True
>           O x y z -> False

parseEither :: Either String B.ByteString -> [Content]
-- something strange going on with ByteString type...works without the annotation

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

finds value of tag in XML object, if it exists:

> findTag :: String -> [(String,String)] -> String
> findTag s obj = case lookup s obj of
>                      Just str -> str
>                      Nothing  -> error (s ++ " not found in XML")

finds attribute type matching string in the dictionary: 

> findDef :: String -> [(String, attr)] -> attr
> findDef s dict = case lookup s dict of
>                      Just a  -> a
>                      Nothing -> error (s ++ " not found in attribute dict")
> findType s obj dict = findDef (findTag s obj) dict

> makeWord :: [(String,String)] -> Word
> makeWord obj = case findTag "pos" obj of
>                 "noun" -> Noun number gender gramcase
>                 "verb" -> Verb person number tense voice mood False False False
>                 "adj"  -> Adj  number gender gramcase
>                 "conj" -> Conj
>                 "pron" -> Pron number gender gramcase
>                 other  -> error (other ++ " parsing not yet supported...")
>     where number   = findType "number" obj numberDict
>           gramcase = findType "case" obj caseDict
>           gender   = findType "gender" obj genderDict
>           person   = findType "person" obj personDict
>           tense    = findType "tense" obj tenseDict
>           voice    = findType "voice" obj voiceDict
>           mood     = findType "mood" obj moodDict

> type AttrDict a = [(String, a)]

> genderDict = [("masc",Masc),("fem",Fem),("neut",Neut)]
> numberDict = [("sg",Sg),("pl",Pl)]
> caseDict   = [("nom",Nom),("gen",Gen),("dat",Dat),("acc",Acc),("abl",Abl),("voc",Voc)]
> personDict = [("1st",First),("2nd",Second),("3rd",Third)]
> tenseDict  = [("pres",Pres),("fut",Fut),("imperf",Imperf),("perf",Perf),
>               ("plup",Pluperf),("futperf",Futperf)]
> voiceDict  = [("act",Act),("pass",Pass)]
> moodDict   = [("ind",Ind),("subj",Subj),("inf",Inf),("imperat",Imperat)]
> 


