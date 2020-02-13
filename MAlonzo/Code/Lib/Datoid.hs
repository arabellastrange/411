{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Datoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Lib.Equality
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Splatoid
import qualified MAlonzo.Code.Lib.Two
import qualified MAlonzo.Code.Lib.Z90Zero

name4 = "Lib.Datoid.Dec~"
d4 :: () -> ()
d4 = erased
name12 = "Lib.Datoid.Datoid"
d12 = ()
newtype T12
  = C22 (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16)
name18 = "Lib.Datoid.Datoid.Data"
d18 :: T12 -> ()
d18 = erased
name20 = "Lib.Datoid.Datoid.eq?"
d20 :: T12 -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16
d20 v0
  = case coe v0 of
      C22 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name34 = "Lib.Datoid._.DatInj"
d34 :: T12 -> () -> MAlonzo.Code.Lib.Equality.T178 -> T12
d34 v0 v1 v2 = du34 v0 v2
du34 :: T12 -> MAlonzo.Code.Lib.Equality.T178 -> T12
du34 v0 v1
  = coe
      (\ v2 v3 -> C22 v3) erased
      (\ v2 v3 ->
         let v4
               = coe
                   d20 v0 (coe MAlonzo.Code.Lib.Equality.d184 v1 v2)
                   (coe MAlonzo.Code.Lib.Equality.d184 v1 v3) in
         case coe v4 of
           MAlonzo.Code.Lib.Sigma.C30 v5 v6
             -> case coe v5 of
                  MAlonzo.Code.Lib.Two.C6
                    -> coe
                         (MAlonzo.Code.Lib.Sigma.C30 (coe v5) (coe (\ v7 -> coe v6 erased)))
                  MAlonzo.Code.Lib.Two.C8
                    -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe v5) erased)
                  _ -> MAlonzo.RTE.mazUnreachableError
           _ -> MAlonzo.RTE.mazUnreachableError)
name58 = "Lib.Datoid.DatSplat"
d58 :: MAlonzo.Code.Lib.Splatoid.T6 -> T12
d58 v0 = du58
du58 :: T12
du58
  = coe
      (\ v0 v1 -> C22 v1) erased
      (\ v0 v1 ->
         MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C8) erased)
name64 = "Lib.Datoid.DatZero"
d64 :: T12
d64 = coe du58
name66 = "Lib.Datoid.DatOne"
d66 :: T12
d66 = coe du58
name70 = "Lib.Datoid.DatEq"
d70 :: () -> AgdaAny -> AgdaAny -> T12
d70 v0 v1 v2 = du70
du70 :: T12
du70 = coe du58
name76 = "Lib.Datoid._+D+_"
d76 :: T12 -> T12 -> T12
d76 v0 v1
  = coe
      (\ v2 v3 -> C22 v3) erased
      (\ v2 ->
         case coe v2 of
           MAlonzo.Code.Lib.Sigma.C30 v3 v4
             -> case coe v3 of
                  MAlonzo.Code.Lib.Two.C6
                    -> coe
                         (\ v5 ->
                            case coe v5 of
                              MAlonzo.Code.Lib.Sigma.C30 v6 v7
                                -> case coe v6 of
                                     MAlonzo.Code.Lib.Two.C6
                                       -> let v8 = coe d20 v0 v4 v7 in
                                          case coe v8 of
                                            MAlonzo.Code.Lib.Sigma.C30 v9 v10
                                              -> coe
                                                   (seq
                                                      (coe v9)
                                                      (coe
                                                         (MAlonzo.Code.Lib.Sigma.C30
                                                            (coe v9) erased)))
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     MAlonzo.Code.Lib.Two.C8
                                       -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe v3) erased)
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError)
                  MAlonzo.Code.Lib.Two.C8
                    -> coe
                         (\ v5 ->
                            case coe v5 of
                              MAlonzo.Code.Lib.Sigma.C30 v6 v7
                                -> case coe v6 of
                                     MAlonzo.Code.Lib.Two.C6
                                       -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe v6) erased)
                                     MAlonzo.Code.Lib.Two.C8
                                       -> let v8 = coe d20 v1 v4 v7 in
                                          case coe v8 of
                                            MAlonzo.Code.Lib.Sigma.C30 v9 v10
                                              -> coe
                                                   (seq
                                                      (coe v9)
                                                      (coe
                                                         (MAlonzo.Code.Lib.Sigma.C30
                                                            (coe v9) erased)))
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError)
                  _ -> MAlonzo.RTE.mazUnreachableError
           _ -> MAlonzo.RTE.mazUnreachableError)
name158 = "Lib.Datoid.DatTwo"
d158 :: T12
d158 = coe (d76 (coe d66) (coe d66))
name164 = "Lib.Datoid._>D<_"
d164 :: T12 -> (AgdaAny -> T12) -> T12
d164 v0 v1
  = coe
      (\ v2 v3 -> C22 v3) erased
      (\ v2 ->
         case coe v2 of
           MAlonzo.Code.Lib.Sigma.C30 v3 v4
             -> coe
                  (\ v5 ->
                     case coe v5 of
                       MAlonzo.Code.Lib.Sigma.C30 v6 v7
                         -> let v8 = coe d20 v0 v3 v6 in
                            case coe v8 of
                              MAlonzo.Code.Lib.Sigma.C30 v9 v10
                                -> case coe v9 of
                                     MAlonzo.Code.Lib.Two.C6
                                       -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe v9) erased)
                                     MAlonzo.Code.Lib.Two.C8
                                       -> let v11 = coe d20 (coe v1 v3) v4 v7 in
                                          case coe v11 of
                                            MAlonzo.Code.Lib.Sigma.C30 v12 v13
                                              -> coe
                                                   (seq
                                                      (coe v12)
                                                      (coe
                                                         (MAlonzo.Code.Lib.Sigma.C30
                                                            (coe v12) erased)))
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError
                       _ -> MAlonzo.RTE.mazUnreachableError)
           _ -> MAlonzo.RTE.mazUnreachableError)
name2131 = "Lib.Datoid..absurdlambda"
d2131 ::
  T12 ->
  T12 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Z90Zero.T4
d2131 = erased
name2393 = "Lib.Datoid..absurdlambda"
d2393 ::
  T12 ->
  T12 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Z90Zero.T4
d2393 = erased
