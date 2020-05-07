{-# OPTIONS --allow-unsolved-metas #-}
module CS410-19.Exercise.One where

open import Lib.Equality
open import Lib.One
open import Lib.Pi
open import Lib.Sigma
open import Lib.Nat


------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Monoids, Lists, Thinnings, and other stories
------------------------------------------------------------------------------
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- (I) Lists are Very Special Monoids
------------------------------------------------------------------------------

data List (X : Set) : Set where
  []     : List X
  _,-_   : X -> List X -> List X
infixr 60 _,-_

record Monoid (X : Set) : Set where
  field
    neutral : X
    compose : X -> X -> X
    compose-neutral-thing : forall x -> compose neutral x ~ x
    compose-thing-neutral : forall x -> compose x neutral ~ x
    compose-compose       : forall x y z ->
      compose (compose x y) z ~ compose x (compose y z)

module _ where

  open Monoid

  -- Monoid homomorphisms
  record _-Monoid>_ {X Y}(MX : Monoid X)(MY : Monoid Y) : Set where
    field
      transform : X -> Y
      transform-neutral : transform (neutral MX) ~ neutral MY
      transform-compose : forall x0 x1 ->
                          transform (compose MX x0 x1) ~
                          compose MY (transform x0) (transform x1)


------------------------------------------------------------------------------
-- 1.1 Implement concatenation for lists. Prove it yields a monoid.
------------------------------------------------------------------------------

module _ where

  open Monoid

  _+L_ : forall {X} -> List X -> List X -> List X
  _+L_ {X} [] ys = ys
  _+L_ {X} (x ,- xs) ys = x ,- (xs +L ys)
  infixr 60 _+L_

  Monoid-List : forall X -> Monoid (List X)
  neutral               (Monoid-List X) =  []
  compose               (Monoid-List X) xs ys = xs +L ys
  compose-neutral-thing (Monoid-List X) xs = r~
  compose-thing-neutral (Monoid-List X) [] = r~
  {- use $~ to apply func to both sides of an equation -}
  compose-thing-neutral (Monoid-List X) (x ,- xs) = x ,-_ $~ compose-thing-neutral (Monoid-List X) xs
  compose-compose (Monoid-List X) [] ys zs = r~
  {- compose-compose x (compose-compose +L xs ys) zs -}
  compose-compose (Monoid-List X) (x ,- xs) ys zs = x ,-_ $~ (compose-compose (Monoid-List X) xs ys zs)


------------------------------------------------------------------------------
-- 1.2 Implement "reduce" for lists. Prove it yields a monoid homomorphism.
------------------------------------------------------------------------------

module _ {X : Set}(M : Monoid X) where

  open Monoid M
  open _-Monoid>_

  -- reduce takes a list of values in some monoid and composes them all
  -- in sequence, left to right

  reduce : Monoid-List X -Monoid> M
  transform reduce [] = neutral
  {- where for example transform reduce is the sum function, i would add x to the sum of the list recursively -}
  transform reduce (x ,- xs) = compose x (transform reduce xs)
  transform-neutral reduce = r~
  {- ~o indicates symmerty, here the thing we have written is the reservse of the proof we want and so we indicate that 
  those are the same thing -}
  transform-compose reduce [] ys = compose-neutral-thing (transform reduce ys) ~o
  {- ~[ and > are signpost functions - see definition -} 
  transform-compose reduce (x ,- xs) ys =
    (compose x (transform reduce (xs +L ys))) ~[(compose x) $~
    (transform-compose reduce xs ys) > compose x (compose (transform reduce xs) (transform reduce ys)) <
    compose $~ {!!} ~$~ {!transform-compose reduce xs ys!} ]~ compose ( transform reduce (x ,- xs)) (transform reduce ys)
    [QED]


------------------------------------------------------------------------------
-- 1.3 Implement map for lists. Prove it respects identity and composition.
-- Prove, moreover, that map always yields monoid homomorphisms
------------------------------------------------------------------------------

list : forall {X Y} -> (X -> Y) -> List X -> List Y
list f [] = []
list f (x ,- xs) = (f x) ,- (list f xs)

module _ {X}(f : X -> X)(f-identity : forall x -> f x ~ x)
  where

  list-identity : forall xs -> list f xs ~ xs
  list-identity [] = r~
  {- ~$~ applies proof that two functions are equal to the proof that two args are equal -}
  list-identity (x ,- xs) =  _,-_ $~ f-identity x ~$~ list-identity xs

module _ {X Y Z}(f : X -> Y)(g : Y -> Z)(h : X -> Z)
  (f-g-is-h : forall x -> g (f x) ~ h x)
  where

  list-composition : forall xs -> list g (list f xs) ~ list h xs
  list-composition [] = r~
  list-composition (x ,- xs) = _,-_ $~ f-g-is-h x ~$~ list-composition xs

module _ {X Y}(f : X -> Y)
  where

  open _-Monoid>_
  
  list-monoid-homomorphism : Monoid-List X -Monoid> Monoid-List Y
  transform list-monoid-homomorphism = list f
  transform-neutral list-monoid-homomorphism = r~
  transform-compose list-monoid-homomorphism [] ys = r~
  transform-compose list-monoid-homomorphism (x ,- xs) ys =  _,-_ $~ r~ ~$~ transform-compose list-monoid-homomorphism xs ys 


------------------------------------------------------------------------------
-- 1.4 Establish this special fact about monoid homomorphisms from *lists*.
------------------------------------------------------------------------------

module _ {X Y}(MY : Monoid Y)(h : Monoid-List X -Monoid> MY)
  where  -- h is any old 

  open Monoid MY
  open _-Monoid>_

  -- We may construct a function from X to Y as a special case.
  -- Make a singleton list and transform it!

  single : X -> Y
  single x = transform h (x ,- [])

  -- Now, show that single exposes the entire behaviour of h.

  transform-reduce : forall xs ->
    transform h xs ~ transform (reduce MY) (list single xs)
  transform-reduce [] = transform-neutral h
  {- cons with empty list to make singletion list -}
  transform-reduce (x ,- xs) = transform h (x ,- xs) ~[ transform-compose h (x ,- []) xs  > (compose $~ r~ ~$~ transform-reduce xs)


------------------------------------------------------------------------------
-- (II) The "predicate transformer", All
------------------------------------------------------------------------------

data All {X}(P : X -> Set) : List X -> Set where
  []   : All P []
  _,-_ : forall {x xs} ->
         P x -> All P xs -> All P (x ,- xs)


------------------------------------------------------------------------------
-- 1.5 Applicative Functor Structure
------------------------------------------------------------------------------

-- Show that if P is always true, so is All P

pureAll : forall {X}{P : X -> Set} ->
  [ P ] -> [ All P ]
pureAll p {[]} = []
pureAll p {x ,- xs} = p ,- pureAll p

-- Show that
-- if S x implies T x for all x in a list
-- and S holds for all elements of the list
-- then so does T.

_<*All*>_ : forall {X}{S T : X -> Set} ->
  [ All (S -:> T) -:> All S -:> All T ]
fs <*All*> [] = []
(f ,- fs) <*All*> (s ,- ss) = f s ,- (fs <*All*> ss)

-- Use the above two gadgets to define map for All

all : forall {X}{S T : X -> Set} ->
  [ S -:> T ] -> [ All S -:> All T ]
all f [] = []
all f (s ,- ss) = f s ,- all f ss

-- Show that all respects identity and composition.

module _ {X}{P : X -> Set}(f : [ P -:> P ])
  (f-identity : forall {x} (p : P x) -> f p ~ p)
  where

  all-identity : forall {xs}(ps : All P xs) -> all f ps ~ ps
  all-identity [] = r~
  all-identity (p ,- ps) = _,-_ $~ f-identity p ~$~ all-identity ps

module _ {X}{S T U : X -> Set}
  (f : [ S -:> T ])(g : [ T -:> U ])(h : [ S -:> U ])
  (f-g-is-h : forall {x} (s : S x) -> g (f s) ~ h s)
  where

  all-compose : forall {xs}(ss : All S xs) -> all g (all f ss) ~ all h ss
  all-compose [] = r~
  all-compose (s ,- ss) = _,-_ $~ f-g-is-h s ~$~ all-compose ss
  

------------------------------------------------------------------------------
-- (III) Order-Preserving Embeddings (or "thinnings" for short)
------------------------------------------------------------------------------

-- The type xs <: ys represents the possible order-preserving
-- embeddings from xs to ys. That means ys is generated by
-- inserting more stuff anywhere in xs, i.e. "thinning" xs.

data _<:_ {X : Set} : List X -> List X -> Set where
  _^-_ : forall x {ys zs} -> ys <: zs ->       ys  <:  x ,- zs  -- insert new
  _,-_ : forall x {ys zs} -> ys <: zs ->  x ,- ys  <:  x ,- zs  -- keep old
  []   :                                    []     <:    []     -- done!
infixr 60 _^-_

infix 50 _<:_

-- You can also think of xs <: ys as the ways of selecting
-- elements xs from ys, with
--   x ^- ... meaning "x is missing, then ...",
--   x ,- ... meaning "x is present, then ...",
--   [] meaning "end of list".

-- You can also see a thinning in xs <: ys as a vector of bits
-- telling whether each element position in ys is connected
-- from an element position in xs.

--  xs =            2 ,-      4 ,- []
--        0 ^- 1 ^- 2 ,- 3 ^- 4 ,- []  :  xs <: ys
--  ys =  0 ,- 1 ,- 2 ,- 3 ,- 4 ,- []


------------------------------------------------------------------------------
-- Exploration (for comprehension rather than credit)
------------------------------------------------------------------------------

-- Lists of elements of One are a lot like numbers

num : Nat -> List One
num ze     = []
num (su n) = <> ,- num n

-- Using C-c C-a with -l and -s options, generate exhaustive lists of
-- thinnings with the following types.

pick0from4 : List (num 0 <: num 4)
pick0from4 = (<> ^- <> ^- <> ^- <> ^- []) ,- []

pick1from4 : List (num 1 <: num 4)
pick1from4 = (<> ^- <> ^- <> ^- <> ,- []) ,-
             (<> ^- <> ^- <> ,- <> ^- []) ,-
             (<> ^- <> ,- <> ^- <> ^- []) ,-
             (<> ,- <> ^- <> ^- <> ^- []) ,-
             []

pick2from4 : List (num 2 <: num 4)
pick2from4 = (<> ^- <> ^- <> ,- <> ,- []) ,-
             (<> ^- <> ,- <> ^- <> ,- []) ,-
             (<> ^- <> ,- <> ,- <> ^- []) ,-
             (<> ,- <> ^- <> ^- <> ,- []) ,-
             (<> ,- <> ^- <> ,- <> ^- []) ,-
             (<> ,- <> ,- <> ^- <> ^- []) ,-
             []

pick3from4 : List (num 3 <: num 4)
pick3from4 = (<> ^- <> ,- <> ,- <> ,- []) ,-
             (<> ,- <> ^- <> ,- <> ,- []) ,-
             (<> ,- <> ,- <> ^- <> ,- []) ,-
             (<> ,- <> ,- <> ,- <> ^- []) ,-
             []
          
pick4from4 : List (num 4 <: num 4)
pick4from4 = (<> ,- <> ,- <> ,- <> ,- []) ,- []

-- But with more interesting elements, we have fewer options, sometimes.

thinOdds : List (1 ,- 3 ,- 5 ,- [] <: 0 ,- 1 ,- 2 ,- 3 ,- 4 ,- 5 ,- 6 ,- [])
thinOdds = (0 ^- 1 ,- 2 ^- 3 ,- 4 ^- 5 ,- 6 ^- []) ,- []


------------------------------------------------------------------------------
-- 1.6 Categorical Structure
------------------------------------------------------------------------------

module _ {X : Set} where
-- Construct the identity thinning from any list to itself.

  oi : forall {xs : List X} -> xs <: xs
  oi {[]} = []
  {- <: operator selects subset of right arg, filter-like -}
  oi {x ,- xs} = x ,- oi

-- Give composition for thinnings. Minimize the number of cases.

  _-<-_ : forall {X}{xs ys zs : List X} -> xs <: ys -> ys <: zs -> xs <: zs
  {- drop straight away -} 
  th -<- x ^- ph = x ^- (th -<- ph)
  {- the case for keep for filter one drop in filter 2 -}
  .x ^- th -<- x ,- ph = th -<- x ^- ph
  {- keep in both filters -}
  .x ,- th -<- x ,- ph = x ,- (th -<- ph)
  th -<- [] = th
  

  infixl 40 _-<-_

-- Prove the following laws. Minimize the number of cases (which will
-- depend on your definition of _-<-_).

  oi-<- : forall {xs ys : List X}(ph : xs <: ys) -> oi -<- ph ~ ph
  oi-<- (x ^- ph) = x ^-_ $~ oi-<- ph
  oi-<- (x ,- ph) = x ,-_ $~ oi-<- ph
  oi-<- [] = r~

  _-<-oi : forall {xs ys : List X}(th : xs <: ys) -> th -<- oi ~ th
  (x ^- th) -<-oi = x ^-_ $~ (th -<-oi)
  (x ,- th) -<-oi = x ,-_ $~ (th -<-oi)
  [] -<-oi = r~

  assoc-<- : forall {ws xs ys zs : List X}
               (th0 : ws <: xs)(th1 : xs <: ys)(th2 : ys <: zs) ->
               (th0 -<- th1) -<- th2 ~ th0 -<- (th1 -<- th2)
  assoc-<- th0 (x ^- th1) (x₁ ^- th2) = x₁ ^-_ $~ assoc-<- th0 (x ^- th1) th2
  assoc-<- th0 (x ^- th1) (.x ,- th2) = x ^-_ $~ assoc-<- th0 th1 th2
  assoc-<- th0 (x ,- th1) (x₁ ^- th2) =  x₁ ^-_ $~ assoc-<- th0 (x ,- th1 ) th2
  assoc-<- (.x ^- th0) (x ,- th1) (.x ,- th2) = x ^-_ $~ assoc-<- th0 th1 th2 -- probably drop this duplicate
  assoc-<- (.x ,- th0) (x ,- th1) (.x ,- th2) = x ,-_ $~ assoc-<- th0 th1 th2
  assoc-<- th0 [] (x ^- th2) = x ^-_ $~ (assoc-<- th0 [] th2)
  assoc-<- th0 [] [] = r~


------------------------------------------------------------------------------
-- 1.7 Selecting
------------------------------------------------------------------------------

-- We can use the "selection" interpretation of thinnings to act
-- on data indexed by lists.
-- The type All P ys has elements of type P y for each y in ys.
-- If xs <: ys, show that we can get P x for each x in xs.

  select : forall {xs ys : List X}{P : X -> Set} ->
           xs <: ys -> All P ys -> All P xs
  select (x ^- th) (x₁ ,- pys) = select th pys
  select (x ,- th) (x₁ ,- pys) = x₁ ,- select th pys
  select [] pys = pys

-- Now prove the following laws relating to selecting by the
-- identity and composition.

  select-oi : forall {xs : List X}{P : X -> Set} -> (pxs : All P xs) ->
              select oi pxs ~ pxs
  select-oi [] = r~
  select-oi (x ,- pxs) = x ,-_ $~ select-oi pxs

  select-<- : forall {xs ys zs : List X}{P : X -> Set} ->
              (th : xs <: ys)(ph : ys <: zs) -> (pzs : All P zs) ->
              select (th -<- ph) pzs ~ select th (select ph pzs)
  select-<- (x ^- th) (x₁ ^- ph) (x₂ ,- pzs) = select-<- (x ^- th) ph pzs
  select-<- (x ^- th) (.x ,- ph) (x₁ ,- pzs) = select-<- th ph pzs
  select-<- (x ,- th) (x₁ ^- ph) (x₂ ,- pzs) = select-<- (x ,- th) ph pzs
  select-<- (x ,- th) (.x ,- ph) (x₁ ,- pzs) = x₁ ,-_ $~ select-<- th ph pzs
  select-<- [] (x ^- ph) (x₁ ,- pzs) = select-<- [] ph pzs
  select-<- [] [] [] = r~

-- Prove, moreover, that mapping can move through selecting.

  select-all : forall {xs ys : List X}{S T : X -> Set}
    (th : xs <: ys)(f : [ S -:> T ])(ss : All S ys) ->
    select th (all f ss) ~ all f (select th ss)
  select-all (x ^- th) f (x₁ ,- ss) = select-all th f ss
  select-all (x ,- th) f (x₁ ,- ss) = f x₁ ,-_ $~ select-all th f ss
  select-all [] f [] = r~


------------------------------------------------------------------------------
-- 1.8 Emptiness
------------------------------------------------------------------------------

-- Show that there is a unique thinning from the empty list to any other.

  oe : forall {xs : List X} -> [] <: xs
  oe {[]} = []
  oe {x ,- xs} = x ^- oe

  oe~ : forall {xs : List X}(th ph : [] <: xs) -> th ~ ph
  oe~ (x ^- th) (.x ^- ph) = x ^-_ $~ oe~ th ph
  oe~ [] [] = r~

-- Show that selecting by the empty thinning gives you an empty list

  select-oe : forall {P : X -> Set}{xs : List X}(ps : All P xs) ->
    select oe ps ~ []
  select-oe [] = r~
  select-oe (x ,- ps) = select-oe ps


------------------------------------------------------------------------------
-- 1.9 Antisymmetry
------------------------------------------------------------------------------

-- Show that if two lists are mutually embeddable, they are equal
-- and the embeddings are the identity.

  antisym : forall {xs ys : List X}
             (th : xs <: ys)(ph : ys <: xs) ->
             xs ~ ys >< \ { r~ -> th ~ oi * ph ~ oi }
  antisym (x ^- th) (x₁ ^- ph) = {!antisym th ?!}
  antisym (x ^- th) (.x ,- ph) = {!antisym ? ?!}
  antisym (x ,- th) (.x ^- ph) = {!antisym ? ?!}
  -- only these two cases are actually possible 
  antisym (x ,- th) (.x ,- ph) with antisym th ph
  antisym (x ,- th) (.x ,- ph) | r~ , fst₁ , snd₁ = r~ , ((x ,-_ $~ (fst₁)) , ( x ,-_ $~ snd₁))
  -- * is the non dependant of >< eq. haskell pairs
  antisym [] [] = r~ , r~ , r~

-- Deduce that oi is unique.

  oi~ : forall {xs : List X}(th ph : xs <: xs) -> th ~ ph
  oi~ (x ^- th) (.x ^- ph) = x ^-_ $~ {!!}
  oi~ (x ^- th) (.x ,- ph) = {!!}
  oi~ (x ,- th) (.x ^- ph) = {!!}
  -- possible cases
  oi~ (x ,- th) (.x ,- ph) = x ,-_ $~ oi~ th ph
  oi~ [] [] = r~


------------------------------------------------------------------------------
-- 1.10 Splittings
------------------------------------------------------------------------------

-- If we have two thinnings,
--   th : xs <: zs
--   ph : ys <: zs
-- we can say what it means for th and ph to *split* zs:
-- every element position in zs is connected from either
-- a position in xs or from a position in ys, but *not both*.

  data Splitting : {xs ys zs : List X}
                   (th : xs <: zs)(ph : ys <: zs) 
                -> Set where
    _^,-_ : forall w {xs ys zs}{th : xs <: zs}{ph : ys <: zs} ->
               Splitting th ph ->
               Splitting (w ^- th) (w ,- ph)
    _,^-_ : forall w {xs ys zs}{th : xs <: zs}{ph : ys <: zs} ->
               Splitting th ph ->
               Splitting (w ,- th) (w ^- ph)
    [] : Splitting [] []
  infixr 60 _^,-_ _,^-_

-- Show that if we know how xs <: zs, we can find a splitting of zs by
-- computing...

  thinSplit : {xs zs : List X}(th : xs <: zs) ->
    List X   >< \ ys ->    -- ...what wasn't from xs...
    ys <: zs >< \ ph ->  -- ...but was in zs...
    Splitting th ph        -- ...hence forms a splitting.
{-  thinSplit {xs} (_^-_ x {zs = zs} th) = x ,- zs , ({! th!} , {!thinSplit ?!})
  thinSplit (_,-_ x {ys} {zs} th) = {!!} , ({!!} , {!!}) -}
  thinSplit (x ^- th) with thinSplit th
  ... | ys , ph , s = x ,- ys , (x ,- ph , x ^,- s)
  thinSplit (x ,- th) with thinSplit th
  ... | ys , ph , s = ys , (x ^- ph , x ,^- s)
  thinSplit [] = _ , (_ , [])

-- Given a splitting, show that we can "riffle" together a bunch
-- of "All P"-s for each selection to get an "All P" for the whole.

  riffle : forall {xs ys zs : List X}
                {th : xs <: zs}{ph : ys <: zs}
                {P : X -> Set} ->
                All P xs -> Splitting th ph -> All P ys ->
                All P zs
  riffle pxs (w ^,- s) (p ,- pys) = p ,- (riffle pxs s pys)
  riffle (p ,- pxs) (w ,^- s) pys = p ,- riffle pxs s pys
  riffle [] [] pys = pys

-- Moreover, we can use a splitting to invert "riffle", dealing
-- out an "All P" for the whole list into the parts for each
-- selection in the splitting, and making sure that the parts
-- riffle back together to make the whole.

  data Deal {xs ys zs : List X}
          {th : xs <: zs}{ph : ys <: zs}(s : Splitting th ph)
          {P : X -> Set} :
            All P zs -> Set where
    dealt : (pxs : All P xs)(pys : All P ys) -> Deal s (riffle pxs s pys)

  deal : {xs ys zs : List X}
       {th : xs <: zs}{ph : ys <: zs}(s : Splitting th ph)
       {P : X -> Set}(pzs : All P zs) -> Deal s pzs
  deal (w ^,- s) (y ,- pys) with deal s pys
  deal (w ^,- s) (y ,- .(riffle pxs s pys)) | dealt pxs pys = dealt pxs (y ,- pys)
  deal (w ,^- s) (y ,- pys) with deal s pys
  deal (w ,^- s) (y ,- .(riffle pxs s pys)) | dealt pxs pys = dealt (y ,- pxs) pys
  deal [] y = dealt [] y

-- We say that Deal is a *view* of All.
