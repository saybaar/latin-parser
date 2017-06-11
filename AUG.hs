-------------------------------------------------------------------------------
-- This file contains Haskell source code for the programs described in:
--
--               Using Types to Parse Natural Language
--
--         Mark P. Jones           Paul Hudak and Sebastian Shaumyan
--     University of Nottingham            Yale University
--      Nottingham, England           New Haven, Connecticut, USA
--
-- Proceedings of the Glasgow Workshop on Functional Programming, July 1995.
-- Published by Springer-Verlag in the Workshops in Computer Science series.
--
-- With some modifications, April 2017.
--
-- This version of the program has been tested using Hugs.  To use with
-- GHCi, uncomment the line marked below.
-------------------------------------------------------------------------------

module AUG where 

import Prelude hiding(Word)  -- uncomment this line for use in GHCi
import Data.List(transpose)
import LatinTypes

-- Trees: -----------------------------------------------------------

type TTree = (Tree,[Type]) 
data Tree  = TreeAtom String | FAp TTree TTree | BAp TTree TTree

tree          :: TTree -> Tree 
tree (tr, ty)  = tr

-- Sentences: -----------------------------------------------------------------

type Sentence = [TTree]

-- Enumerating Types/Trees: ---------------------------------------------------

ttrees       :: Sentence -> [TTree]
ttrees [t]    = [t]
ttrees ts     = [ t | (ls,rs) <- splits ts, l <- ttrees ls,
                                            r <- ttrees rs,
                                            t <- combine l r ]

splits       :: [a] -> [([a],[a])]
splits ts     = zip (inits ts) (tails ts)

inits        :: [a] -> [[a]]
inits [x]     = []
inits (x:xs)  = map (x:) ([]:inits xs)

tails        :: [a] -> [[a]]
tails [x]     = []
tails (x:xs)  = xs : tails xs

combine      :: TTree -> TTree -> [TTree]
combine l r   = app FAp l r ++ app BAp r l

app          :: (TTree -> TTree -> Tree) -> TTree -> TTree -> [TTree]
app op (a,ts) (b,ss)
              = [ (op (a,[O x y w]) (b,[x]), [y]) | (O x y w)<-ts, z<-ss, x==z ]

-- A More Sophisticated Algorithm: --------------------------------------------

fastTtrees      :: Sentence -> [TTree]
fastTtrees       = head . head . cache

cache           :: Sentence -> [[[TTree]]]
cache [x]        = [[[x]]]
cache (x:xs)     = [build x (transpose rs)] ++ rs
                   where rs = cache xs

build           :: TTree -> [[[TTree]]] -> [[TTree]]
build a []       = [[a]]
build a (ts:tss) = g (reverse is) ts : is
 where is      = build a tss
       g is ts = [ r | (i,t) <- zip is ts,
                        ti   <- i,
                        tt   <- t,
                        r    <- combine ti tt ]

-- Drawing trees: -------------------------------------------------------------

type Pic = (Int, Int, Int, [String])

drawTTree    :: TTree -> String
drawTTree tr  = unlines ((show (tree tr) ++ ":\n") : ptr)
 where (_,_,_,ptr)   = tpic tr
       tpic (tr,ty)  = oneAbove (pic tr) (label (show ty))
       pic (TreeAtom w)  = label w
       pic (FAp l r) = sideBySide (tpic l) (tpic r)
       pic (BAp l r) = sideBySide (tpic r) (tpic l)
 
label  :: String -> Pic
label a = (1,  l, c, [ " " ++ a])
 where l = 1 + length a
       c = 1 + l`div`2

sideBySide :: Pic -> Pic -> Pic
sideBySide (hl,wl,cl,pl) (hr,wr,cr,pr) = (h+1,w,c,p)
 where h   = hl `max` hr
       w   = wl + wr
       c   = (cl + wl+cr+1) `div` 2
       p   = zipWith (++) (replicate (h-hl) (replicate wl ' ') ++ pl)
                          (replicate (h-hr) (replicate wr ' ') ++ pr) ++ [tie]
       tie = replicate (cl-1)   ' ' ++ "\\" ++
             replicate (c-cl-1) '_' ++ "_" ++ replicate (cr+wl-c-1) '_' ++
             "/" ++ replicate (wr - cr) ' '

oneAbove :: Pic -> Pic -> Pic
oneAbove (ht,wt,ct,pt) (hb,wb,cb,pb) = (ht+hb, w, c, p)
 where c     = ct `max` cb
       w     = c + ((wt-ct) `max` (wb-cb))
       p     = addMargins (c-ct) ((w+ct)-(wt+c)) pt ++
               addMargins (c-cb) ((w+cb)-(wb+c)) pb

addMargins    :: Int -> Int -> [String] -> [String]
addMargins l r = map (\s -> lm ++ s ++ rm)
 where lm = replicate l ' '
       rm = replicate r ' '

-- Miscellaneous utilities: ---------------------------------------------------

instance Show Tree where
  showsPrec d (TreeAtom s)       = showString s
  showsPrec d (FAp t (TreeAtom s,_)) = shows (tree t)  .  showChar ' '    .
                                   showString s
  showsPrec d (FAp t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'
  showsPrec d (BAp t (TreeAtom s,_)) = shows (tree t)  .
                                   showChar ' '    .
                                   showString s
  showsPrec d (BAp t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'

-------------------------------------------------------------------------------
