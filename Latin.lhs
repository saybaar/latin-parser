> module Latin where

> data Word = Noun String Number Gender Case
>           | Adj  String Number Gender Case
>           | Verb String Person Number Tense Mood Voice
>           | Part String Number Gender Case Tense Voice
>           | Conj String
>           | Prep String 
>           | Pron String Number Gender Case
>             deriving Show
> data Number = Sg | Pl                                        deriving Show
> data Gender = Masc | Fem | Neut                              deriving Show
> data Case   = Nom | Gen | Dat | Acc | Abl | Voc              deriving Show
> data Person = First | Second | Third                         deriving Show
> data Tense  = Fut | Pres | Imperf | Perf | Pluperf | Futperf deriving Show
> data Mood   = Ind | Subj | Imperat | Inf                     deriving Show
> data Voice  = Act | Pass                                     deriving Show
