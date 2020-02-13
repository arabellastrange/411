{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Pi where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

name12 = "Lib.Pi._>>_"
d12 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> () -> (AgdaAny -> ()) -> ()
d12 = erased
name22 = "Lib.Pi._!>_"
d22 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> () -> (AgdaAny -> ()) -> ()
d22 = erased
name40 = "Lib.Pi.id"
d40 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> AgdaAny
d40 v0 v1 v2 = du40 v2
du40 :: AgdaAny -> AgdaAny
du40 v0 = coe v0
name48 = "Lib.Pi._:`_"
d48 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> AgdaAny
d48 v0 v1 v2 = du48 v2
du48 :: AgdaAny -> AgdaAny
du48 v0 = coe v0
name70 = "Lib.Pi._.ko"
d70 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d70 v0 v1 v2 v3 v4 v5 = du70 v4
du70 :: AgdaAny -> AgdaAny
du70 v0 = coe v0
name82 = "Lib.Pi._._._$o_"
d82 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  (AgdaAny -> ()) -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d82 v0 v1 v2 v3 v4 v5 = du82 v4 v5
du82 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du82 v0 v1 = coe v1 v0
name104 = "Lib.Pi._._._._$$_"
d104 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d104 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du104 v6 v7 v8
du104 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du104 v0 v1 v2 = coe v0 v2 (coe v1 v2)
name138 = "Lib.Pi._._-_"
d138 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d138 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du138 v6 v7 v8
du138 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du138 v0 v1 v2 = coe v1 v2 (coe v0 v2)
name156 = "Lib.Pi._-:>_"
d156 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d156 = erased
name172 = "Lib.Pi.[_]"
d172 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> () -> (AgdaAny -> ()) -> ()
d172 = erased
