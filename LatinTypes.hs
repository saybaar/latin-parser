module LatinTypes where

import Prelude hiding (Word)

data Word = Noun Number Gender Case
          | Adj  Number Gender Case
          | Verb Person Number Tense Voice Mood
          | Part Number Gender Case Tense Voice
          | Conj
          | Prep 
          | Pron Number Gender Case
            deriving (Show, Eq)

data Number = Sg | Pl                                        deriving (Eq, Show, Enum)
data Gender = Masc | Fem | Neut                              deriving (Eq, Show, Enum)
data Case   = Nom | Gen | Dat | Acc | Abl | Voc              deriving (Eq, Show, Enum)
data Person = First | Second | Third                         deriving (Eq, Show, Enum)
data Tense  = Fut | Pres | Imperf | Perf | Pluperf | Futperf deriving (Eq, Show, Enum)
data Mood   = Ind | Subj | Imperat | Inf                     deriving (Eq, Show, Enum)
data Voice  = Act | Pass                                     deriving (Eq, Show, Enum)


data Type   = Atom Word | T | S | O Type Type Word deriving Eq

instance Show Type where
  showsPrec d (Atom word)  = showString $ show word
  showsPrec d (O x y word) = showString $ show word

-- A simple lexicon, sufficient for examples in the paper: --------------------

wordTypes  :: TempWord -> [Type]
wordTypes w = findWord w dictionary

wordTypesNew :: [Word] -> [Type]
wordTypesNew xs  = foldl (++) [] $ map sentenceFunctions xs

data Dictionary = Nil | Node TempWord [Type] Dictionary Dictionary
type TempWord       = String

instance Show Dictionary where
   showsPrec d Nil = id
   showsPrec d (Node w ts l r)
     = shows l .
       showString w . showString " :: " . shows ts . showChar '\n' .
       shows r

addWord         :: Type -> TempWord -> Dictionary -> Dictionary
addWord t w Nil  = Node w [t] Nil Nil
addWord t w (Node v ts l r)
        | w == v = Node v (t:ts) l r
        | w <  v = Node v ts (addWord t w l) r
        | w >  v = Node v ts l (addWord t w r)

findWord        :: TempWord -> Dictionary -> [Type]
findWord w Nil   = []
findWord w (Node v ts l r) 
        | w == v = ts
        | w <  v = findWord w l
        | w >  v = findWord w r

vocab       :: [TempWord] -> Type -> Dictionary -> Dictionary
vocab vs t d = foldr (addWord t) d vs

other      :: [(TempWord,Type)] -> Dictionary -> Dictionary
other wts d = foldr ($) d [ addWord t w | (w,t) <- wts ]

dictionary :: Dictionary
dictionary = other miscwords $ Nil

miscwords   = [("puella", Atom (Noun Sg Fem Nom)), ("puella", Atom (Noun Sg Fem Abl)),
               ("puer", Atom (Noun Sg Masc Nom)), ("data", Atom (Noun Pl Neut Nom)),
               ("bonus", nounMod (Adj Sg Masc Nom))]
               ++ [("bona", x) | x <- bona ]
               ++ [("currit", x) | x <- verb currit ]
               ++ [("et", x) | x <- conjunction Conj ]
  
-- Translate web results into word parses (to be displayed), then word
-- parses into lists of these types

currit = Verb Third Sg Pres Act Ind
anyNoun = [ Noun n g c | n <- allNumbers,
                         g <- allGenders,
                         c <- allCases ] 
bonaParses = [Adj Sg Fem Nom, Adj Sg Fem Abl, Adj Pl Neut Nom]
bona = map nounMod bonaParses

allNumbers = enumFrom Sg
allGenders = enumFrom Masc
allCases   = enumFrom Nom
allPersons = enumFrom First
allTenses  = enumFrom Pres
allVoices  = enumFrom Act
allMoods   = enumFrom Ind

nounMod :: Word -> Type
nounMod word =
  case word of
    Adj n g c -> O (Atom (Noun n g c)) (Atom (Noun n g c)) word
    _         -> error("only adjectives can modify nouns")
  -- e.g. adjectives
verb word =
  case word of
    Verb p n t v m -> [ O (Atom (Noun n gs Nom)) (Atom (Verb p n t v m)) word |
                          gs <- allGenders ] 
    _         -> error("only verbs can be verbified")
-- Need an extra type for "puella et..." on its own, that needs another noun to bind to -
-- just treat this as an adjective? 
conjunction word =
    [ O (Atom (Verb p n t1 v m)) (Atom (Verb p n t2 v m)) word |
                          p <- allPersons,
                          n <- allNumbers,
                          t1 <- allTenses,
                          t2 <- allTenses,
                          v <- allVoices,
                          m <- allMoods ] ++
    [ O (Atom (Noun ns gs1 c)) (Atom (Noun Pl gs2 c)) word |
                          ns <- allNumbers,
                          gs1 <- allGenders,
                          gs2 <- allGenders,
                          c <- allCases ]

sentenceFunctions                  :: Word -> [Type]
sentenceFunctions (Noun n g c)     = [atomNoun n g c]
sentenceFunctions (Adj n g c)      = [atomNoun n g c] ++
                                     [nounMod (Adj n g c)]
sentenceFunctions (Verb p n t v m) = [atomVerb p n t v m] ++
                                     verb (Verb p n t v m)

atomNoun x y z = Atom (Noun x y z)
atomVerb p n t v m = Atom (Verb p n t v m)
