{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Sum where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Two

name14 = "Lib.Sum._._+_"
d14 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> () -> ()
d14 = erased
name40 = "Lib.Sum._._._<?>_"
d40 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  (MAlonzo.Code.Lib.Sigma.T16 -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Lib.Sigma.T16 -> AgdaAny
d40 v0 v1 v2 v3 v4 v5 v6 v7 = du40 v5 v6 v7
du40 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Lib.Sigma.T16 -> AgdaAny
du40 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Lib.Sigma.C30 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Lib.Two.C6 -> coe v0 v4
             MAlonzo.Code.Lib.Two.C8 -> coe v1 v4
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name56 = "Lib.Sum.Dec"
d56 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> ()
d56 = erased
