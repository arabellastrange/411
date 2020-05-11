module CS410-19.Exercise.Two where

open import Agda.Primitive

open import Lib.Zero
open import Lib.One
open import Lib.Two
open import Lib.Pi
open import Lib.Equality
open import Lib.Sigma
open import Lib.Nat
open import Lib.Sum
open import Cat.Setoid
open import Cat.Cat

open import CS410-19.Exercise.One

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- How To Cut Things Up
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- The following record type gives a way to describe the possible ways to
-- cut shapes into finitely many pieces. The shape of each piece is given
-- by a value in type I. You give a type, Cut, to specify the ways things can
-- be cut, then a function which gives the list of the shapes of the pieces
-- the cut produces.

record CutInto (I : Set) : Set1 where
  constructor Space
  field
    Cut    : Set
    pieces : Cut -> List I

open CutInto public

-- We will often use the following notion of arrow, to talk about how to cut
-- up an "outside" shape of type O into a bunch of pieces whose "inside"
-- shapes have type I. Think of these things as templates, where O gives the
-- shapes of the templates and I gives the shapes of their holes.

_>8_ : Set -> Set -> Set1
I >8 O = O -> CutInto I

-- Let's have an example straight away. If we think of numbers as lengths,
-- we can specify how to cut a length into two pieces which sum to it.

Length : Nat >8 Nat
Cut    (Length n) = Nat >< \ i   -- length of first piece
                 -> Nat >< \ j   -- length of second piece
                 -> i +N j ~ n   -- proof that they add to what we're cutting
pieces (Length n) (i , j , _) = i ,- j ,- []   -- the two pieces

-- Suppose we have some P : I -> Set which specifies stuff fitting a given
-- inside shape. Well then, given some outside shape, we can say how to
-- cut it into pieces with inside shapes which we fill with Ps.

_-Frag_ : {I O : Set}(C : I >8 O) -> (I -> Set) -> (O -> Set)
(C -Frag P) o = Cut (C o) >< pieces (C o) - All P


------------------------------------------------------------------------------
-- 2.1 "Identity" and "Composition"
------------------------------------------------------------------------------

-- Define the identity cutting, where there is one piece, and the inside shape
-- is the same as the output shape.
-- cut nothing return everything
Id>8 : forall {I} -> I >8 I
-- simplest set?
Cut (Id>8 i) = One
pieces (Id>8 i) =  λ x → i ,- []

-- Define the composition of cuttings.
-- Here, a composite cut should explain how to cut a K into Js,then each J
-- into Is. The pieces you end up with should be all the Is from all the Js.
-- Hints: (i) IJ - Cut : J -> Set; (ii) you may need a helper function to
-- assemble the pieces.

-- Check that -Frag makes them behave like identity and composition, up to
-- isomorphism.

-- assembler (turns list of lists into list)
hassembler : ∀ {J} {I} (w : List J) (IJ : J → CutInto I) → All (λ x → Cut (IJ x)) w → List I
hassembler [] y [] = []
hassembler (x ,- x1) y (z ,- z1) = pieces (y x) z +L hassembler x1 y z1

_->8-_ : forall {I J K} -> I >8 J -> J >8 K -> I >8 K
Cut ((IJ ->8- JK) k) = (JK -Frag λ x → Cut (IJ x)) k
pieces ((IJ ->8- JK) k) (fst , snd) with JK k
... | Space cut piecesOf = hassembler (piecesOf fst) IJ snd

---------------

toId>8 : forall {I}{P : I -> Set} -> [ P -:> Id>8 -Frag P ]
fst (toId>8 p) = <>
snd (toId>8 p) = p ,- []

fromId>8 : forall {I}{P : I -> Set} -> [ Id>8 -Frag P -:> P ]
fromId>8 {I} {P} {i} (fst , (s ,- snd)) = s

-- prove composition of cuttings
help2 : ∀ {J} {xs : List J} {ys : List J} {P : J → Set} →
        All P xs →
        All P ys →
        All P (xs +L ys)
help2 [] ys = ys
help2 (x ,- xs) ys = x ,- help2 xs ys
        
help : ∀ {I} {J} {js : List J} {IJ : I >8 J} {P : I → Set}
          (ijps : All (IJ -Frag P) js) →
          All P (hassembler js IJ (all fst ijps))
help [] = []
help ((fst , snd) ,- ijps) = help2 snd (help ijps)

to->8- : forall {I J K}(IJ : I >8 J)(JK : J >8 K){P : I -> Set} ->
  [ JK -Frag (IJ -Frag P) -:> (IJ ->8- JK) -Frag P ]
to->8- IJ JK (c , ijps) = (c , all fst ijps) ,  help ijps

{- help : ∀ {I} {J} {K} {IJ : I >8 J} {JK : J >8 K} {P : I → Set} {i : K} {fst = fst₁ : Cut (JK i)} { js : List J }
       (snd : All (λ x → Cut (IJ x)) (js)) →
       All P (hassembler (pieces (JK i) fst₁) IJ {!snd!}) →
       All (λ o → Cut (IJ o) >< (λ a → All P (pieces (IJ o) a))) (js)
help x ijps = {!ijps!} -}

head : ∀ {J} {xs : List J} {ys : List J} {P : J → Set} →
      All P (xs +L ys) →
      All P xs
head {xs = []} {ys} ijps = []
head {xs = x ,- xs} {ys} {P} (x1 ,- ijps) = x1 ,- head ijps

tail : ∀ {J} {xs : List J} {ys : List J} {P : J → Set} →
      All P (xs +L ys) →
      All P ys
tail {xs = []} {ys} ijps = ijps
tail {xs = x ,- xs} {[]} ijps = []
tail {xs = x ,- xs} {y ,- ys} (x1 ,- ijps) = tail ijps

breakPieces : forall {I} {J} {K} {P : I → Set} {js : List J}
        (IJ : I >8 J) -> (JK : J >8 K) ->
        (snd : All (λ x → Cut (IJ x)) (js)) → (ijps : All P (hassembler js IJ snd)) →
        All (λ o → Cut (IJ o) >< (λ a → All P (pieces (IJ o) a))) (js)
breakPieces IJ JK [] ijps = []
breakPieces IJ JK (x ,- xs)  ijps = (x , head ijps) ,- breakPieces IJ JK xs (tail ijps)

from->8- : forall {I J K}(IJ : I >8 J)(JK : J >8 K){P : I -> Set} ->
  [ (IJ ->8- JK) -Frag P -:> JK -Frag (IJ -Frag P) ]
from->8- IJ JK ((fst , snd) , ijps) = fst ,  breakPieces IJ JK snd ijps  

-- The "to"s and "from"s should be mutally inverse, but I'll let you off that
-- proof task.


------------------------------------------------------------------------------
-- Trees made by repeated cutting
------------------------------------------------------------------------------

module _
  {I : Set}            -- shapes of things
  (C : I >8 I)         -- a way of cutting, where insides are like outsides
  (Leaf : I -> Set)    -- leaves of a given shape
  where

  data _-Tree_ (i : I) : Set where  -- trees have shapes in I

      -- if you have a leaf the right shape, it can be a tree
    leaf  : Leaf i -> _-Tree_ i

      -- otherwise cut into pieces where you plug in subtrees
    [8<_] : (C -Frag _-Tree_) i -> _-Tree_ i


------------------------------------------------------------------------------
-- 2.2 Folding over Trees
------------------------------------------------------------------------------

module _ {I : Set}{C : I >8 I} where

-- Define fold for trees. I.e., show that you can make a V from a tree if
--   (i)  you can make a V from a leaf
--   (ii) you can make a V from a tree whose subtrees have already been
--          made into Vs
-- To appease the termination checker, you need to define this mutually with
-- allFold (which is morally the same as all (fold ...))


{- all : forall {X}{S T : X -> Set} -> [ S -:> T ] -> [ All S -:> All T ]
all f [] = []
all f (s ,- ss) = f s ,- all f ss -}

  fold : forall {L V}
      -> [ L -:> V ]
      -> [ C -Frag V -:> V ]   -- an "algebra"
      -> [ C -Tree L -:> V ]
  allFold : forall {L V}
      -> [ L -:> V ]
      -> [ C -Frag V -:> V ]   -- an "algebra"
      -> [ All (C -Tree L) -:> All V ]
  fold leaf' algebra (leaf t) = leaf' t
  -- 4: snd
  fold leaf' algebra [8< fst , snd ] = algebra (fst , allFold leaf' algebra snd)
  allFold leaf' algebra [] = []
  -- 5: ts
  -- allFold leaf' algebra (leaf t ,- ts) = leaf' t ,- allFold leaf' algebra ts
  -- 6: snd, 7: ts
  -- allFold leaf' algebra ([8< fst , snd ] ,- ts) = algebra (fst , allFold leaf' algebra snd) ,- allFold leaf' algebra ts
  allFold leaf' algebra (t ,-  ts) = fold leaf' algebra t ,- allFold leaf' algebra ts
 

-- Prove that if you plug the constructors for trees into fold, you get
-- the identity.

  fold-rebuild : forall {L}{i}(t : (C -Tree L) i) ->
    fold leaf [8<_] t ~ t
  allFold-rebuild : forall {L}{is}(ts : All (C -Tree L) is) ->
    allFold leaf [8<_] ts ~ ts
  fold-rebuild (leaf x) = r~
  fold-rebuild [8< fst , snd ] rewrite allFold-rebuild snd = r~
  allFold-rebuild [] = r~
  allFold-rebuild (t ,- ts) rewrite fold-rebuild t | allFold-rebuild ts =  r~

-- Prove the following fusion law, which holds when the first fold acts
-- only at leaves.

  module _ {L U V}
    (l : [ L -:> C -Tree U ])
    (f : [ U -:> V ])
    (g : [ C -Frag V -:> V ])
    where

    fold-fusion : forall {i}(t : (C -Tree L) i) -> fold f g (fold l [8<_] t) ~ fold (l - fold f g) g t
    allFold-fusion : forall {is}(ts : All (C -Tree L) is) -> allFold f g (allFold l [8<_] ts) ~ allFold (l - fold f g) g ts
    fold-fusion (leaf x) = r~
    fold-fusion [8< fst , snd ] rewrite allFold-fusion snd = r~
    allFold-fusion [] = r~
    allFold-fusion (t ,- ts) rewrite fold-fusion t | allFold-fusion ts = r~

-- Prove that fold is *extensional*, i.e., that two fold behave the same way
-- if their base and step cases always behave the same way.

  module _ {L V}
    (f0 f1 : [ L -:> V ])         (fq : forall {i}(l : L i) -> f0 l ~ f1 l)
    (g0 g1 : [ C -Frag V -:> V ]) (gq : forall {i}(vs : (C -Frag V) i) -> g0 vs ~ g1 vs)
    where
    fold-ext    : forall {i}(t : (C -Tree L) i) -> fold f0 g0 t ~ fold f1 g1 t
    allFold-ext : forall {is}(ts : All (C -Tree L) is) -> allFold f0 g0 ts ~ allFold f1 g1 ts
    fold-ext (leaf x) = fq x
    fold-ext [8< fst , snd ] rewrite allFold-ext snd = gq (fst , allFold f1 g1 snd)
    allFold-ext [] = r~
    allFold-ext (t ,- ts) rewrite fold-ext t | allFold-ext ts = r~


------------------------------------------------------------------------------
-- 2.3 Flattening Trees
------------------------------------------------------------------------------

-- By way of an example of folding, let us flatten.

-- Here are lists indexed by length (a.k.a. "vectors").

data _-Vec_ (X : Set) : Nat -> Set where
  [] : X -Vec 0
  _,-_ : forall {n} -> X -> X -Vec n -> X -Vec su n

-- Define their concatenation.

_+V_ : forall {X m n} -> X -Vec m -> X -Vec n -> X -Vec (m +N n)
[] +V [] = []
[] +V (y ,- ys) = y ,- ys
(x ,- xs) +V  ys = x ,- (xs +V ys)

infixr 10 _+V_

-- We may define singletons of Xs (to put at the leaves of trees).

data OneOf (X : Set) : Nat -> Set where
  oneOf : X -> OneOf X 1
-- none : {n : _} -> Zero -> OneOf X n

{- OneOf : Set -> Nat -> Set
OneOf X 1 = X
OneOf X _ = Zero -}

-- A (Length -Tree OneOf X) n is a binary tree with n leaves, each of
-- which stores *one* X. Show how to flatten such a tree to a vector,
-- using fold. Hint: you may need to define some helpers or learn to
-- use fancy lambdas
-- \ { pattern1 -> expression1 ; .. ; patternk -> expressionk }

{-  flatten : Tree -> List
    flatten leaf         = []
    flatten (node l x r) = flatten l ++ x ,- flatten r -}
    
helper : ∀ {X} {i} → OneOf X i → X -Vec i
helper (oneOf x) =  x ,- []

helper2 : ∀ {X} {i} →
          (Nat >< (λ i₁ → Nat >< (λ j → (i₁ +N j) ~ i))) ><
          (λ a → All (_-Vec_ X) (fst a ,- fst (snd a) ,- [])) →
          X -Vec i
-- helper2 ((.0 , snd₁) , [] ,- thrd) = {!!}
-- helper2 ((.(su _) , snd₁) , (x ,- snd₂) ,- thrd) = {!!}
helper2 ((fst , snd , r~) , x1 ,- x2 ,- []) = x1 +V x2


flatten : forall {X} ->
          [ Length -Tree OneOf X -:> (X -Vec_) ]
flatten {X} = fold {V = (X -Vec_)} (helper) helper2


------------------------------------------------------------------------------
-- 2.4 Binary Search Trees, Revisited
------------------------------------------------------------------------------

-- Remember this business from Lecture One?

module BST
  (Key : Set)
  (Le : Key -> Key -> Set)
  (owoto : (x y : Key) -> Le x y + Le y x)
  where

  data Bound : Set where
    -inf : Bound
    key  : Key -> Bound
    +inf : Bound

  LeB : Bound -> Bound -> Set
  LeB -inf    u       = One
  LeB (key x) (key y) = Le x y
  LeB l       +inf    = One
  LeB _       _       = Zero

  {-
  data Tree (l u : Bound) : Set where
  
    leaf : LeB l u -> Tree l u
    
    node : (k : Key) -> Tree l (key k) -> Tree (key k) u
        -> Tree l u
  -}

-- We can see these trees as a special case of our cutting-up constructions.

  BSTCut : (Bound * Bound) >8 (Bound * Bound)
  Cut    (BSTCut (l , u)) = Key
  pieces (BSTCut (l , u)) k = (l , key k) ,- (key k , u) ,- []

  LeB' : Bound * Bound -> Set
  LeB' (l , u) = LeB l u

  BST : Bound * Bound -> Set
  BST = BSTCut -Tree LeB'

-- Check that you can still write insert.

{-  insert : Key -> Tree -> Tree
    insert k leaf           = node leaf k leaf
    insert k (node lt x rt) with leKey k x
    insert k (node lt x rt) | tt = node (insert k lt) x rt
    insert k (node lt x rt) | ff = node lt x (insert k rt) 

  leKey : Nat -> Nat -> Two
  leKey ze     y      = tt
  leKey (su x) ze     = ff
  leKey (su x) (su y) = leKey x y 
-}
       

  insert : [ (BSTCut -Frag LeB') -:> BST -:> BST ]
  insert (k , lk ,- ku ,- []) (leaf x) = [8<_] (k , leaf lk ,- (leaf ku ,- []))
{-  insert (k , lk ,- ku ,- []) [8< fst , snd ] with LeB fst k
  ... | Le fst k  = [8<_] (k , insert ({!!} , ({!!} ,- {!!} ,- [])) {!!} ,- {!!} )
  ... | Le k fst  = ? -}
  insert (k , lk ,- ku ,- []) [8< fst , snd ,- thrd ,- [] ] with owoto fst k
  ... | inl  fstk = [8<_] (fst , (snd ,- ((insert (k , (fstk ,- (ku ,- []))) thrd) ,- [])))
  ... | inr kfst  = [8<_] (fst , ((insert (k , (lk ,- (kfst ,- []))) snd) ,- (thrd ,- [])))


------------------------------------------------------------------------------
-- 2.5 Two-Three Trees
------------------------------------------------------------------------------

-- [2-3 trees](https://en.wikipedia.org/wiki/2%E2%80%933_tree) are a variant
-- on binary search trees which are well enough *balanced* to ensure
-- logarithmic access times. Crucially
--
--  * they have uniform height, i.e. all paths from the root to a leaf have
--    the same length
--  * each node has either one key  and   two subtrees
--                      or two keys and three subtrees

-- Your mission is to build 2-3 trees as an instance of -Tree by explaining
-- the meaningful ways to cut up intervals. The shape of a 2-3 tree is given
-- by a        Nat  -- the height
--         * Bound  -- the lower bound
--         * Bound  -- the upper bound

-- Please complete this datatype:

{-
  data Tree (l u : Bound) : Set where
    leaf : LeB l u -> Tree l u
    node : (k : Key) -> Tree l (key k) -> Tree (key k) u
        -> Tree l u

  data Tree : Set where
    leaf : Tree
    node : Tree -> Key -> Tree -> Tree

  module _
    {I : Set}            -- shapes of things
    (C : I >8 I)         -- a way of cutting, where insides are like outsides
    (Leaf : I -> Set)    -- leaves of a given shape
  where

  data _-Tree_ (i : I) : Set where  -- trees have shapes in I
      -- if you have a leaf the right shape, it can be a tree
    leaf  : Leaf i -> _-Tree_ i
      -- otherwise cut into pieces where you plug in subtrees
    [8<_] : (C -Frag _-Tree_) i -> _-Tree_ i

  -}
  
  data Cut23 : Nat * Bound * Bound -> Set where
    twoNode : {!!}
    threeNode : {!!}
    -- 2-node : (k :  Key) -> (l : Bound) -> (u : Bound) -> (h : Nat) -> Cut23 {!!} -> Cut23 {!!}  -> Cut23 {!!}
    -- 3-node : (k1 : Key) -> (k2 : Key) -> (l :  Bound) -> (u : Bound) -> (h : Nat) -> Cut23 {!!} -> Cut23 {!!} -> Cut23 {!!}
    -- give a constructor for 2-nodes
    -- give a constructor for 3-nodes

-- With that done, explain the subtree structure:

  TwoThree : (Nat * Bound * Bound) >8 (Nat * Bound * Bound)
  Cut (TwoThree hlu) = Cut23 hlu
  pieces (TwoThree hlu) c = {!!}

-- Meanwhile, leaves must have height exactly 0, and should, as before,
-- contain ordering evidence.

  Leafy : Nat * Bound * Bound -> Set
  Leafy (0 , l , u) = LeB l u
  Leafy _           = Zero

-- We obtain 2-3 trees, enforcing both order and balance.

  Tree23 : Nat * Bound * Bound -> Set
  Tree23 = TwoThree -Tree Leafy

-- *** JAMES: I have managed to write an example starting as follows.

  example : (a b c d : Key) → Le a b → Le b c → Le c d → Tree23 (2 , -inf , +inf)
  example a b c d ab bc cd = {!!}
  
{- A possible tree using the data given by the arguments looks like this:

           __c__
         _/     \_
        /         \
      a,b          d
    _/ | \_       / \
   /   |   \     /   \
  <>  ab   bc   cd   <>
-}

-- Now, let's figure out how to do insertion with *rebalancing*. When you
-- insert into a tree of a given height, most of the time, there's enough
-- slack in the tree to return a tree of the same height. E.g., if you
-- insert into a 2-node of height 1, you can give back a 3-node of height 1.
-- But sometimes, the original tree is *full*, so you have no option but
-- to increase the height. E.g., if you insert into a 3-node of height 1,
-- you have no choice but to give back a tree of height 2.

-- We can explain these possibilities as a way of cutting.

-- You will need to add information to the following datatype, but wait
-- until you see what you need.

  data CutInsert (nlu : Nat * Bound * Bound) : Set where
    asSmall : CutInsert nlu
    tooTall : {- what goes here? -> -} CutInsert nlu

-- Likewise, explain what pieces you get when we're too tall.

  Insert : (Nat * Bound * Bound) >8 (Nat * Bound * Bound)
  Cut (Insert nlu) = CutInsert nlu
  pieces (Insert nlu) asSmall                      = nlu ,- []
  pieces (Insert nlu) (tooTall {- what's here?-})  = {!!}

-- What you need the above to be should arise from trying to implement
-- the following.

  insert23 : [ (snd - BSTCut -Frag LeB') -:> Tree23 -:> Insert -Frag Tree23 ]
  insert23 (k , lk ,- ku ,- []) t = {!!}

-- This gives us the guts of an n * log n complexity sorting algorithm,
-- provided you can flatten efficiently.

-- First, wrap 2-3 trees as trees of some height between the widest bounds.

  BalanceTree : Set
  BalanceTree = Nat >< \ height -> Tree23 (height , -inf , +inf)

-- Now, you wrap insertion.

  insertBal : Key -> BalanceTree -> BalanceTree
  insertBal k (h , t) = {!!}

-- So that we have makeTree as before:

  makeBalTree : List Key -> BalanceTree
  makeBalTree []        = 0 , leaf <>
  makeBalTree (k ,- ks) = insertBal k (makeBalTree ks)

-- In lecture 1, we defined ordered lists as follows
  {-
  data OList (l u : Bound) : Set where
  
    nil  : LeB l u -> OList l u
    
    cons : (k : Key) -> LeB l (key k) -> OList (key k) u
        -> OList l u
  -}

-- Refactor that definition in terms of -Tree.

  OList : Bound * Bound -> Set
  OList = {!!} -Tree LeB'

-- Use fold to define flatten.

  flattenBalTree : BalanceTree -> OList (-inf , +inf)
  flattenBalTree = {!!}

-- (There is a bonus for avoiding quadratic complexity by eliminating
-- left-nested concatenations.)

-- We should now have, as before:

  sort : List Key -> OList (-inf , +inf)
  sort = makeBalTree - flattenBalTree


------------------------------------------------------------------------------
-- 2.6 Hutton's Razor with Two and Nat
------------------------------------------------------------------------------

-- Prof Graham Hutton of the University of Nottingham (he of Haskell book
-- fame) is fond of exploring programming language concepts in a minimalist
-- setting. There is always
--   * some notion of value (he chooses numbers)
--   * some way to compute with values (he chooses addition)
--   * the minimal extra to illustrate the concept.
-- We call the language generated by numbers and adding "Hutton's Razor" as
-- it's pretty much the simplest starting point.

module HUTTON where

-- Let's explore *types*. To be interesting, we should have at least two
-- types, so to be minimalist, let us have exactly two types.

  data Ty : Set where two nat : Ty

-- Now, let me give a grammar of expressions. We have constants:

-- <two> ::= ff | tt                   -- call these b
-- <nat> ::= 0 | 1 | 2 | ...           -- call these n

-- We have variables:
-- <var> ::= x | y | z | ...

-- We have ways to make big expressions from little expressions

-- <expr> ::= <two>
--          | <nat>
--          | <expr> + <expr>
--          | if <expr> then <expr> else <expr>
--          | <var>
--   -- call these e

-- Suppose Var : Ty -> Set maps each type to the set of variables which
-- are in scope and have that type. We can write a type system.

--         x in Var T
--       --------------      (variables are well typed if in scope)
--            x : T

--       -----------      -----------    (constants have types)
--         b : two          n : Nat

--         e0 : nat     e1 : nat
--       -------------------------  (adding takes numbers to a number)
--             e0 + e1 : nat

--         e0 : two    e1 : T    e2 : T     (if-then-else uses a Boolean
--       --------------------------------    to choose between two things
--          if e0 then e1 else e2 : T        of the same type)

-- Your mission is to define a type of syntax trees which represent only
-- and exactly the well typed and well scoped terms.

  data Node : Ty -> Set where
    synBol : Node two
    synNum : Node nat
    synVar : {!Node!}
    -- can u add two bools?
    synExp : Node nat -> Node nat -> Node nat
    -- abstract T -> T -> T?
    synIf  : Node two -> {!Node!} -> {!!} -> {!!}
  
    -- you fill this in

  Syntax : Ty >8 Ty
  Cut    (Syntax T) = Node T
  pieces (Syntax .two) synBol = two ,- []
  pieces (Syntax .nat) synNum = nat ,- []
  pieces (Syntax .{!!}) synVar = {!!}
  pieces (Syntax .nat) (synExp c c₁) = nat ,- []
  pieces (Syntax .{!!}) (synIf c x x₁) = {!!}

  Expr : (Ty -> Set)   -- which variables are in scope
      -> (Ty -> Set)   -- which expressions are well typed
  Expr Var = Syntax -Tree Var

-- We start to give a semantics for this language by mapping its types
-- to Agda types.

  Value : Ty -> Set
  Value two = Two
  Value nat = Nat

-- An *environment* is a function which maps variables in scope to values
-- of the right type. That is, it's a type-preserving function from
-- variables to values, [ Var -:> Value ].

-- Write an interpreter for this language as a fold.

  interpret : ∀ {i} → Node i >< (λ a → All Value (pieces (Syntax i) a)) → Value i
  -- instantiate types kinda randomly -> might try to base it off of snd? -- DONE?
  interpret (synBol , x ,- snd) = x
  interpret (synNum , x ,- snd) = x
  interpret (synVar , snd) = {!!}
  interpret (synExp fst₁ fst₂ , x ,- snd) = x
  interpret (synIf fst₁ x x₁ , snd) = {!!}

  eval : forall {Var}
      -> [      Var -:> Value ]  -- you know the values of the variables,
      -> [ Expr Var -:> Value ]  -- so what is the value of an expression?
  eval gamma = fold gamma interpret


------------------------------------------------------------------------------
-- 2.7 Monadic Structure
------------------------------------------------------------------------------

makeTree : ∀ {I} {C : I → CutInto I} {T : I → Set} {i : I} → Cut (C i) >< (λ a → All (C -Tree T) (pieces (C i) a)) → (C -Tree T) i
makeTree (fst , snd) = [8<_] (fst , snd)

-- A "shape-preserving" function from S to C-trees of Ts is called a "Kleisli
-- arrow.

_-K_>_ : forall {I} -> (I -> Set) -> I >8 I -> (I -> Set) -> Set
S -K C > T = [ S -:> C -Tree T ]

module _ where

  open Setoid
  open Cat

-- Construct the Setoid of Kleisli arrows with pointwise equality.

{-  transitive leNat ze y z xy yz = <>
    transitive leNat (su x) (su y) (su z) xy yz = transitive leNat x y z xy yz -}
    
  _=K_>_ : forall {I} -> (I -> Set) -> I >8 I -> (I -> Set) -> Setoid lzero
  Carrier (S =K C > T) = S -K C > T
  Eq      (S =K C > T) f g = forall {i}(s : S i) -> f s ~ g s
  reflEq  (S =K C > T) = λ s → r~
  symEq   (S =K C > T) = λ x s → x s ~o
  transEq (S =K C > T) = λ x y s →  x s -~- y s

-- Show that Kleisli arrows form a category.

-- Actually, don't do that yet, do this...

  module _ {I : Set}{C : I >8 I} where

    _=<<_ : forall {S T}
         ->           S  -K C > T    -- how to replace leaves by trees
         ->  (C -Tree S) -K C > T    -- doing that to whole trees
    k =<< t = fold (λ x → k x) makeTree t

-- The above is the key to giving the composition that you should use to
-- build the category.

    _-K-_ : forall {R S T}
        ->  R  -K C >  S
        ->  S  -K C >  T
        ->  R  -K C >  T
    (j -K- k) t = k =<< j t

-- Now build the Kleisli category.

-- Hint: fold-rebuild, fold-fusion and fold-ext.

{-

Failed to solve the following constraints:
  _1018
    := λ {I} C {R} {S} {T} {U} f g h {i} s →
         fold-fusion (?16 (C = C) (f = f) (g = g) (h = h) (s = s))
         (?17 (C = C) (f = f) (g = g) (h = h) (s = s))
         (?18 (C = C) (f = f) (g = g) (h = h) (s = s))
         (?19 (C = C) (f = f) (g = g) (h = h) (s = s))
    [blocked on problem 2959]
  [2959, 2963] fold (λ {i} → ?16 - (λ {a} → fold ?17 ?18)) ?18 ?19
                 = fold (down (compose (KLEISLI C) g h)) makeTree (down f s)
                 : _1017
  [2959, 2962] fold ?17 ?18 (fold ?16 (λ {i} → [8<_]) ?19)
                 = fold (down h) makeTree (fold (down g) makeTree (down f s))
                 : _1017
  [2959] _1016
           := λ {I} C {R} {S} {T} {U} f g h {i} s →
                _V_1010 (C = C) (f = f) (g = g) (h = h) (s = s)
                (_i_1014 (C = C) (f = f) (g = g) (h = h) (s = s))
           [blocked on problem 2961]
  [2959, 2961] _V_1010 _i_1014 = (C -Tree U) i₁ : Set
-}

{-
 [2911, 2915] fold ?16 ?19 ?21 = fold (down g1) makeTree (down f1 s)
                 : _1011
  [2911, 2914] fold ?15 ?18 ?21 = fold (down g0) makeTree (down f0 s)
                 : _1011
  [2911] _1010
           := λ {I} C {R} {S} {T} {f0} {f1} f {g0} {g1} g {i} s →
                _V_1001 (C = C) (f = f) (g = g) (s = s)
                (_i_1008 (C = C) (f = f) (g = g) (s = s))
           [blocked on problem 2913]
  [2911, 2913] _V_1001 _i_1008 = (C -Tree T) i : Set

-}

  KLEISLI : forall {I}(C : I >8 I) -> Cat \ S T -> UpS (S =K C > T)
  identity (KLEISLI C) = up λ x → leaf x
  compose (KLEISLI C) (up f) (up g) = up (f -K- g)
--  compose-respect (KLEISLI C) {f0 = f0} {f1} f {g0} {g1} g = up (λ s → fold-ext (down g0) (down g1) (down g) (λ x → down g0 {!!}) {!!} {!!} {!down f1 s!})
  compose-respect (KLEISLI C) {f0 = f0} {f1} f {g0} {g1} g = up (λ s → fold-ext (down g0) (down g1) (down g) (λ x → down (compose (KLEISLI C) f0 g0) s) (λ x → down (compose (KLEISLI C) f1 g1) s) (λ vs → {!down f s!}) {!!})
  compose-identity-arrow (KLEISLI C) = λ f → up (λ s → r~)
  compose-arrow-identity (KLEISLI C) = λ f → up (λ s → fold-rebuild (down f s))
  compose-compose (KLEISLI C) {U = U} f g h = up λ s → fold-fusion (down g) (down h) (λ x → down (compose (KLEISLI C) (compose (KLEISLI C) f g) h) {!!}) {!!} 


------------------------------------------------------------------------------
-- 2.8 Combining Ways of Cutting
------------------------------------------------------------------------------

-- Suppose we know two ways, C and D, of cutting up outside shapes O to leave
-- inside pieces in I. Show how to represent the choice of cutting in either
-- the C way or the D way. (E.g., if we know how to cut rectangles vertically
-- and we know how to cut rectangles horizontally, we should know how to cut
-- rectangles either vertically or horizontally.)

-- Compositon/And ?
{-_->8-_ : forall {I J K} -> I >8 J -> J >8 K -> I >8 K
Cut ((IJ ->8- JK) k) = (JK -Frag λ x → Cut (IJ x)) k
pieces ((IJ ->8- JK) k) (fst , snd) with JK k
... | Space cut piecesOf = assembler {js = piecesOf fst} {IJ} snd -}

-- OR
_+>8+_ : forall {I O} -> I >8 O -> I >8 O -> I >8 O
-- Cut ((C +>8+ D) o) = ( C -Frag λ x → Cut (C o) ) o
-- pieces ((C +>8+ D) o) with D o
-- ... | Space cut piecesOf = {!assembler!}
Cut ((C +>8+ D) o) = (C -Frag λ x → Cut (C o)) o
pieces ((C +>8+ D) o) = {!!}

-- Suppose we know a way of cutting Os to make Is. Show that we can add an
-- "orthogonal extra dimension" X to both inside and outside shapes, either
-- on the left or on the right. Cutting in one dimension should preserve the
-- shape in all orthogonal directions. E.g., if we know how to cut lengths,
-- we should know how to make a vertical cut somewhere in the width of a
-- rectangle, giving the pieces the same height as the rectangle. Ditto,
-- swapping the roles of heights and widths.

_>8|_ : forall {I O}
    -> I >8 O
    -> forall X
    -> (I * X) >8 (O * X)
Cut ((C >8| X) (fst , snd)) = X
pieces ((C >8| X) (fst , snd)) = {!!}

_|>8_ : forall {I O}
    -> forall X
    -> I >8 O
    -> (X * I) >8 (X * O)
Cut ((X |>8 C) (fst , snd)) = X
pieces ((X |>8 C) (fst , snd)) = {!!}

module _ {I J : Set} where

-- Using the above three operators, show how to combine ways of cutting
--   C in the I-dimension
--   D in the J-dimension
-- to give the two-dimensional cutting which is
--   either like C in the I-dimension, leaving the J-dimension alone
--       or like D in the J-dimension, leaving the I-dimension alone

  _|+|_ : I >8 I -> J >8 J -> (I * J) >8 (I * J)
  C |+| D = {!!}

-- Congratulations! If you've made it this far, you can now talk about how
-- to *tile* rectangular spaces, like your screen.

Tiling : (Nat * Nat -> Set) -> (Nat * Nat -> Set)
Tiling X = (Length |+| Length) -Tree X

-- And that's how we'll be working with ideas of "application display" in
-- Exercise Three...
