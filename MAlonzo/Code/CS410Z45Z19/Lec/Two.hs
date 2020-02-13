{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.CS410Z45Z19.Lec.Two where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Lib.Equality
import qualified MAlonzo.Code.Lib.Nat
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Splatoid

name6 = "CS410-19.Lec.Two.LeNat"
d6 :: Integer -> Integer -> MAlonzo.Code.Lib.Splatoid.T6
d6 = erased
name20 = "CS410-19.Lec.Two.Preorder"
d20 a0 a1 = ()
data T20
  = C145 (AgdaAny -> AgdaAny)
         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name40 = "CS410-19.Lec.Two.Preorder.reflexive"
d40 :: T20 -> AgdaAny -> AgdaAny
d40 v0
  = case coe v0 of
      C145 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name48 = "CS410-19.Lec.Two.Preorder.transitive"
d48 ::
  T20 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d48 v0
  = case coe v0 of
      C145 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name54 = "CS410-19.Lec.Two._.leNat"
d54 :: T20
d54
  = coe
      (C145
         (coe
            (\ v0 ->
               case coe v0 of
                 0 -> erased
                 _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
                      coe d40 d54 v1))
         (coe
            (\ v0 ->
               case coe v0 of
                 0 -> erased
                 _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
                      coe
                        (\ v2 ->
                           let v3 = subInt (coe v2) (coe (1 :: Integer)) in
                           \ v4 ->
                             let v5 = subInt (coe v4) (coe (1 :: Integer)) in
                             \ v6 v7 -> coe d48 d54 v1 v3 v5 v6 v7))))
name76 = "CS410-19.Lec.Two.Extra"
d76 :: Integer -> Integer -> ()
d76 = erased
name88 = "CS410-19.Lec.Two.extra"
d88 :: Integer -> Integer -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16
d88 v0 v1 v2 = du88 v0 v1
du88 :: Integer -> Integer -> MAlonzo.Code.Lib.Sigma.T16
du88 v0 v1
  = case coe v0 of
      0 -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe v1) erased)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe (du88 (coe v2) (coe v3))
name116 = "CS410-19.Lec.Two.Monoid"
d116 a0 = ()
data T116 = C907 AgdaAny (AgdaAny -> AgdaAny -> AgdaAny)
name140 = "CS410-19.Lec.Two.Monoid.neutral"
d140 :: T116 -> AgdaAny
d140 v0
  = case coe v0 of
      C907 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name142 = "CS410-19.Lec.Two.Monoid.compose"
d142 :: T116 -> AgdaAny -> AgdaAny -> AgdaAny
d142 v0
  = case coe v0 of
      C907 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name146 = "CS410-19.Lec.Two.Monoid.compose-neutral-thing"
d146 :: T116 -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d146 = erased
name150 = "CS410-19.Lec.Two.Monoid.compose-thing-neutral"
d150 :: T116 -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d150 = erased
name158 = "CS410-19.Lec.Two.Monoid.compose-compose"
d158 ::
  T116 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d158 = erased
name164 = "CS410-19.Lec.Two._.monoid+N"
d164 :: T116
d164
  = coe
      (\ v0 v1 v2 v3 v4 -> C907 v0 v1) (0 :: Integer)
      MAlonzo.Code.Lib.Nat.d34 erased erased erased
name192 = "CS410-19.Lec.Two.Monotone"
d192 a0 a1 a2 a3 a4 a5 = ()
data T192
  = C1149 (AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name214 = "CS410-19.Lec.Two.Monotone.transport"
d214 :: T192 -> AgdaAny -> AgdaAny
d214 v0
  = case coe v0 of
      C1149 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name220 = "CS410-19.Lec.Two.Monotone.respect"
d220 :: T192 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d220 v0
  = case coe v0 of
      C1149 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name226 = "CS410-19.Lec.Two._.monotone-x+N"
d226 :: Integer -> T192
d226 v0
  = coe
      (C1149
         (coe (MAlonzo.Code.Lib.Nat.d34 (coe v0)))
         (case coe v0 of
            0 -> coe (\ v1 v2 v3 -> v3)
            _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
                 coe (\ v2 v3 v4 -> coe d220 (d226 (coe v1)) v2 v3 v4)))
