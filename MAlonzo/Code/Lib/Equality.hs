{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Equality where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Lib.Sigma

name14 = "Lib.Equality._._~_"
d14 a0 a1 a2 a3 = ()
data T14 = C18
name24 = "Lib.Equality._._~o"
d24 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> T14 -> T14
d24 = erased
name32 = "Lib.Equality._._-~-_"
d32 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> T14 -> T14 -> T14
d32 = erased
name42 = "Lib.Equality._._~[_>_"
d42 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> T14 -> T14 -> T14
d42 = erased
name56 = "Lib.Equality._._<_]~_"
d56 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> T14 -> T14 -> T14
d56 = erased
name66 = "Lib.Equality._._[QED]"
d66 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> T14
d66 = erased
name76 = "Lib.Equality.rf"
d76 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> T14
d76 = erased
name100 = "Lib.Equality._._~$~_"
d100 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> T14 -> T14 -> T14
d100 = erased
name108 = "Lib.Equality._._$~_"
d108 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () -> AgdaAny -> AgdaAny -> (AgdaAny -> AgdaAny) -> T14 -> T14
d108 = erased
name120 = "Lib.Equality._._~$"
d120 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> T14 -> T14
d120 = erased
name134 = "Lib.Equality._._:[_>"
d134 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> AgdaAny -> T14 -> AgdaAny
d134 v0 v1 v2 v3 v4 = du134 v3
du134 :: AgdaAny -> AgdaAny
du134 v0 = coe v0
name138 = "Lib.Equality._.<_]:_"
d138 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> T14 -> AgdaAny -> AgdaAny
d138 v0 v1 v2 v3 v4 = du138 v4
du138 :: AgdaAny -> AgdaAny
du138 v0 = coe v0
name154 = "Lib.Equality._.Injective"
d154 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d154 = erased
name156 = "Lib.Equality._.Surjective"
d156 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d156 = erased
name158 = "Lib.Equality._.Iso"
d158 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d158 = erased
name178 = "Lib.Equality._._`->_"
d178 a0 a1 a2 = ()
newtype T178 = C1991 (AgdaAny -> AgdaAny)
name184 = "Lib.Equality._._`->_.inj"
d184 :: T178 -> AgdaAny -> AgdaAny
d184 v0
  = case coe v0 of
      C1991 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name186 = "Lib.Equality._._`->_.injective"
d186 :: T178 -> AgdaAny -> AgdaAny -> T14 -> T14
d186 = erased
name188 = "Lib.Equality._._->>_"
d188 a0 a1 a2 = ()
data T188
  = C2057 (AgdaAny -> AgdaAny)
          (AgdaAny -> MAlonzo.Code.Lib.Sigma.T16)
name194 = "Lib.Equality._._->>_.sur"
d194 :: T188 -> AgdaAny -> AgdaAny
d194 v0
  = case coe v0 of
      C2057 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name196 = "Lib.Equality._._->>_.surjective"
d196 :: T188 -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16
d196 v0
  = case coe v0 of
      C2057 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name198 = "Lib.Equality._._<~>_"
d198 a0 a1 a2 = ()
data T198
  = C2125 (AgdaAny -> AgdaAny)
          (AgdaAny -> MAlonzo.Code.Lib.Sigma.T16)
name206 = "Lib.Equality._._<~>_.iso"
d206 :: T198 -> AgdaAny -> AgdaAny
d206 v0
  = case coe v0 of
      C2125 v1 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name208 = "Lib.Equality._._<~>_.isoInj"
d208 :: T198 -> AgdaAny -> AgdaAny -> T14 -> T14
d208 = erased
name210 = "Lib.Equality._._<~>_.isoSur"
d210 :: T198 -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16
d210 v0
  = case coe v0 of
      C2125 v1 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name212 = "Lib.Equality._._<~>_.inv"
d212 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> T198 -> AgdaAny -> AgdaAny
d212 v0 v1 v2 v3 v4 = du212 v3 v4
du212 :: T198 -> AgdaAny -> AgdaAny
du212 v0 v1 = coe (MAlonzo.Code.Lib.Sigma.d26 (coe d210 v0 v1))
name218 = "Lib.Equality._._<~>_.iso-inv"
d218 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> T198 -> AgdaAny -> T14
d218 = erased
name234 = "Lib.Equality._._<~>_.inv-iso"
d234 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> T198 -> AgdaAny -> T14
d234 = erased
name250 = "Lib.Equality._.flip<~>"
d250 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> () -> T198 -> T198
d250 v0 v1 v2 v3 = du250 v3
du250 :: T198 -> T198
du250 v0
  = coe
      (\ v1 v2 v3 -> C2125 v1 v3)
      (\ v1 -> MAlonzo.Code.Lib.Sigma.d26 (coe d210 v0 v1)) erased
      (\ v1 -> MAlonzo.Code.Lib.Sigma.C30 (coe d206 v0 v1) erased)
