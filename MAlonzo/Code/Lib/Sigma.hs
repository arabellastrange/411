{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Sigma where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

name16 = "Lib.Sigma._._><_"
d16 a0 a1 a2 a3 = ()
data T16 = C30 AgdaAny AgdaAny
name26 = "Lib.Sigma._._><_.fst"
d26 :: T16 -> AgdaAny
d26 v0
  = case coe v0 of
      C30 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name28 = "Lib.Sigma._._><_.snd"
d28 :: T16 -> AgdaAny
d28 v0
  = case coe v0 of
      C30 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name32 = "Lib.Sigma._._*_"
d32 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> () -> () -> ()
d32 = erased
name50 = "Lib.Sigma._:*_"
d50 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d50 = erased
name64 = "Lib.Sigma.<_>"
d64 ::
  MAlonzo.Code.Agda.Primitive.T4 -> () -> (AgdaAny -> ()) -> ()
d64 = erased
name76 = "Lib.Sigma._<$>_"
d76 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> T16 -> T16
d76 v0 v1 v2 v3 v4 v5 = du76 v4 v5
du76 :: (AgdaAny -> AgdaAny -> AgdaAny) -> T16 -> T16
du76 v0 v1
  = case coe v1 of
      C30 v2 v3 -> coe (C30 (coe v2) (coe v0 v2 v3))
      _ -> MAlonzo.RTE.mazUnreachableError
