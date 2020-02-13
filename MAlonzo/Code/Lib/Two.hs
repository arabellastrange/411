{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Two where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

name4 = "Lib.Two.Two"
d4 = ()
data T4 = C6 | C8
name20 = "Lib.Two._._<2>_"
d20 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  (T4 -> ()) -> AgdaAny -> AgdaAny -> T4 -> AgdaAny
d20 v0 v1 v2 v3 v4 = du20 v2 v3 v4
du20 :: AgdaAny -> AgdaAny -> T4 -> AgdaAny
du20 v0 v1 v2
  = case coe v2 of
      C6 -> coe v0
      C8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name30 = "Lib.Two.not"
d30 :: T4 -> T4
d30 = coe (du20 (coe C8) (coe C6))
name32 = "Lib.Two.So"
d32 :: T4 -> ()
d32 = erased
name48 = "Lib.Two.if_then_else_"
d48 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> T4 -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny
d48 v0 v1 v2 v3 v4 = du48 v2 v3 v4
du48 ::
  T4 -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny
du48 v0 v1 v2
  = case coe v0 of
      C6 -> coe v2 erased
      C8 -> coe v1 erased
      _ -> MAlonzo.RTE.mazUnreachableError
