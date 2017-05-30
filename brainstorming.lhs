Word = Noun ... | Adj ... etc.??
Clause = combinations of words (full sentence, subordinate clause, noun phrase, etc.)

Words as functions - e.g.
  Prep :: AblNounPhrase -> PrepPhrase
also have to take into account the number/gender of the PrepPhrase, potentially...much
more complex with e.g. result clauses that have person, voice, mood as well

How to capture case matching?
  NomAdj :: NomNounPhrase -> NomNounPhrase
  GenAdj :: GenNounPhrase -> GenNounPhrase
  AccAdj :: AccNounPhrase -> AccNounPhrase
...
and it's not just Nom/Gen/Acc/etc - they also have to match in number and gender.
  Adj num case gen :: NounPhrase num case gen -> NounPhrase num case gen
where num, case, gen must all match across all three. Is this possible?

We also have multiple possible interpretations of even a single parse of a word -
find some examples...

And may need to develop a hierarchy, e.g.
  Prep :: AblNounPhrase -> PrepPhrase
but not
  AblNounPhrase :: Prep -> PrepPhrase
or vice versa. Would it make sense to have both?

More examples:

  NomNounPhrase num :: Verb num tense voice mood -> Sentence num tense voice mood

If that sentence wants to join with a subordinate clause, then what? How to match
references if necessary? This is the question of hierarchy. Different problems come
up if we try to match all the references before tying them up into clauses; what
would we call that grouping? The latter isn't as suitable for drawing trees.

Nouns and NounPhrases are functionally identical, but VerbPhrases need to be
distinguished by whether they have a subject/object/indirect object. Need better
names for that.

Maybe Verbs carry a 3-tuple for subject/object/DO, each a Maybe NounPhrase?

We only need to carry matching-relevant information, so may drop (?) info on
genitive modifiers to NounPhrases and similar. A NounPhrase will only have a single
set of Noun information. 

