{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Nat where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Cat.Smol
import qualified MAlonzo.Code.Lib.Datoid
import qualified MAlonzo.Code.Lib.Equality
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Two
import qualified MAlonzo.Code.Lib.Z90Zero

name4 = "Lib.Nat.Nat"
d4 = ()
data T4 = C6 | C8 Integer
name10 = "Lib.Nat.NatD"
d10 :: MAlonzo.Code.Lib.Datoid.T12
d10
  = coe
      (\ v0 v1 -> MAlonzo.Code.Lib.Datoid.C22 v1) erased
      (\ v0 ->
         case coe v0 of
           0 -> coe
                  (\ v1 ->
                     case coe v1 of
                       0 -> coe
                              (MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C8) erased)
                       _ -> coe
                              (MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C6) erased))
           _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
                coe
                  (\ v2 ->
                     case coe v2 of
                       0 -> coe
                              (MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C6) erased)
                       _ -> let v3 = subInt (coe v2) (coe (1 :: Integer)) in
                            let v4 = coe MAlonzo.Code.Lib.Datoid.d20 d10 v1 v3 in
                            case coe v4 of
                              MAlonzo.Code.Lib.Sigma.C30 v5 v6
                                -> coe
                                     (seq
                                        (coe v5) (coe (MAlonzo.Code.Lib.Sigma.C30 (coe v5) erased)))
                              _ -> MAlonzo.RTE.mazUnreachableError))
name34 = "Lib.Nat._+N_"
d34 :: Integer -> Integer -> Integer
d34 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (addInt (coe (1 :: Integer)) (coe (d34 (coe v2) (coe v1))))
name42 = "Lib.Nat.abacusPlus"
d42 :: Integer -> Integer -> Integer
d42 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (d42 (coe v2) (coe (addInt (coe (1 :: Integer)) (coe v1))))
name50 = "Lib.Nat.symmetricPlus"
d50 :: Integer -> Integer -> Integer
d50 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe v0
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (addInt (coe (2 :: Integer)) (coe (d50 (coe v2) (coe v3))))
name62 = "Lib.Nat.MONOID+N.monoid+N"
d62 :: MAlonzo.Code.Cat.Smol.T8
d62
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Cat.Smol.C39 v0 v1)
      (\ v0 -> 0 :: Integer) (\ v0 v1 v2 -> d34) erased erased erased
name67 = "Lib.Nat..absurdlambda"
d67 ::
  Integer ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Z90Zero.T4
d67 = erased
name107 = "Lib.Nat..absurdlambda"
d107 ::
  Integer ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Z90Zero.T4
d107 = erased
