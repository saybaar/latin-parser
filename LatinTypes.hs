module LatinTypes where

import Prelude hiding (Word)

data Word = Noun Number Gender Case
          | Adj  Number Gender Case
          | Verb Person Number Tense Voice Mood Bool Bool Bool
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

wordTypesNew :: [Word] -> [Type]
wordTypesNew xs  = foldl (++) [] $ map sentenceFunctions xs

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
allTenses  = enumFrom Fut
allVoices  = enumFrom Act
allMoods   = enumFrom Ind

nounMod :: Word -> [Type]
nounMod word =
  case word of
    Adj  n g c -> [ O (Atom (Noun n g c)) rules word |
                    rules <- sentenceFunctions (Noun n g c) ]
    Pron n g c -> [ O (Atom (Noun n g c)) rules word |
                    rules <- sentenceFunctions (Noun n g c) ]
    _         -> error("error in nounmod-fying")
  -- e.g. adjectives
verbSubj n g c word =
  case c of
    Nom -> [ O (Atom (Verb ps ns ts vs ms False a b))
                        (Atom (Verb ps ns ts vs ms True a b)) word |
                      ps <- allPersons,
                      ns <- allNumbers,
                      ts <- allTenses,
                      vs <- allVoices,
                      ms <- allMoods,
                      a  <- [True,False],
                      b  <- [True,False] ]
    _ -> []
verbDObj n g c word =
  case c of
    Acc -> [ O (Atom (Verb ps ns ts vs ms a False b))
                        (Atom (Verb ps ns ts vs ms a True b)) word |
                      ps <- allPersons,
                      ns <- allNumbers,
                      ts <- allTenses,
                      vs <- allVoices,
                      ms <- allMoods,
                      a  <- [True,False],
                      b  <- [True,False] ]
    _ -> []
verbIObj n g c word =
  case c of
    Dat -> [ O (Atom (Verb ps ns ts vs ms a b False))
                        (Atom (Verb ps ns ts vs ms a b True)) word |
                      ps <- allPersons,
                      ns <- allNumbers,
                      ts <- allTenses,
                      vs <- allVoices,
                      ms <- allMoods,
                      a  <- [True,False],
                      b  <- [True,False] ]
    _ -> []
conjunction =
    [ O (Atom (Verb ps1 ns1 t1 v m a b c)) (singletMod (Verb ps2 ns2 t2 v m d e f) Conj) Conj |
                          ps1 <- allPersons,
                          ps2 <- allPersons,
                          ns1 <- allNumbers,
                          ns2 <- allNumbers,
                          t1 <- allTenses,
                          t2 <- allTenses,
                          v <- allVoices,
                          m <- allMoods,
                          a <- [True,False],
                          b <- [True,False],
                          c <- [True,False],
                          d <- [True,False],
                          e <- [True,False],
                          f <- [True,False] ] ++
    [ O (Atom (Noun ns1 gs1 c)) (modFrom (Noun ns2 gs2 c) (Noun Pl gs2 c) Conj) Conj |
                          ns1 <- allNumbers,
                          ns2 <- allNumbers,
                          gs1 <- allGenders,
                          gs2 <- allGenders,
                          c <- allCases ] ++ 
    [ O (Atom (Adj n g c)) (singletMod (Adj n g c) Conj) Conj |
                          n <- allNumbers,
                          g <- allGenders,
                          c <- allCases ]


sentenceFunctions                  :: Word -> [Type]
sentenceFunctions (Noun n g c)     = nounFunctions n g c (Noun n g c)
sentenceFunctions (Adj n g c)      = nounFunctions n g c (Adj n g c) ++
                                     nounMod (Adj n g c)
sentenceFunctions (Pron n g c)     = nounFunctions n g c (Pron n g c) ++
                                     nounMod (Pron n g c)
sentenceFunctions (Verb p n t v m a b c) = [atomVerb p n t v m]
sentenceFunctions (Conj)           = conjunction

nounFunctions n g c word = [atomNoun n g c] ++
                      verbSubj n g c word ++
                      verbDObj n g c word ++
                      verbIObj n g c word
atomNoun x y z = Atom (Noun x y z)
atomVerb p n t v m = Atom (Verb p n t v m False False False)
                    
modFrom :: Word -> Word -> Word -> Type
modFrom x y z = O (Atom x) (Atom y) z
singletMod :: Word -> Word -> Type
singletMod x z = modFrom x x z
