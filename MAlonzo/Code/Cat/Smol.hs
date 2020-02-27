{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Cat.Smol where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Lib.Equality
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Splatoid

name8 = "Cat.Smol.SmolCat"
d8 a0 a1 = ()
data T8
  = C39 (AgdaAny -> AgdaAny)
        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name60 = "Cat.Smol.SmolCat.identity"
d60 :: T8 -> AgdaAny -> AgdaAny
d60 v0
  = case coe v0 of
      C39 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name68 = "Cat.Smol.SmolCat.compose"
d68 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d68 v0
  = case coe v0 of
      C39 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name76 = "Cat.Smol.SmolCat.compose-identity-arrow"
d76 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d76 = erased
name84 = "Cat.Smol.SmolCat.compose-arrow-identity"
d84 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d84 = erased
name100 = "Cat.Smol.SmolCat.compose-compose"
d100 ::
  T8 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d100 = erased
name110 = "Cat.Smol._._=[_>_"
d110 ::
  () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> T8 -> AgdaAny -> ()
d110 = erased
name120 = "Cat.Smol.Preorder"
d120 ::
  () -> (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Splatoid.T6) -> ()
d120 = erased
name148 = "Cat.Smol._.preorder"
d148 ::
  () ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Splatoid.T6) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  T8
d148 v0 v1 v2 v3 = du148 v2 v3
du148 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  T8
du148 v0 v1
  = coe
      (\ v2 v3 v4 v5 v6 -> C39 v2 v3) (\ v2 -> coe v0 v2)
      (\ v2 v3 v4 -> coe v1 v2 v3 v4) erased erased erased
name156 = "Cat.Smol._.Discrete"
d156 :: () -> T8
d156 v0 = du156
du156 :: T8
du156 = coe (du148 erased erased)
name160 = "Cat.Smol.Monoid"
d160 :: () -> ()
d160 = erased
name182 = "Cat.Smol._.OP"
d182 :: () -> (AgdaAny -> AgdaAny -> ()) -> T8 -> T8
d182 v0 v1 v2 = du182 v2
du182 :: T8 -> T8
du182 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2) (\ v1 -> coe d60 v0 v1)
      (\ v1 v2 v3 v4 v5 -> coe d68 v0 v3 v2 v1 v5 v4) erased erased
      erased
name210 = "Cat.Smol._._-SmolCat>_"
d210 a0 a1 a2 a3 a4 a5 = ()
data T210
  = C1683 (AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name236 = "Cat.Smol._._-SmolCat>_.Map"
d236 :: T210 -> AgdaAny -> AgdaAny
d236 v0
  = case coe v0 of
      C1683 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name242 = "Cat.Smol._._-SmolCat>_.map"
d242 :: T210 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d242 v0
  = case coe v0 of
      C1683 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name246 = "Cat.Smol._._-SmolCat>_.map-identity"
d246 :: T210 -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d246 = erased
name258 = "Cat.Smol._._-SmolCat>_.map-compose"
d258 ::
  T210 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d258 = erased
name272 = "Cat.Smol._._.compose"
d272 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d272 v0 = coe (d68 (coe v0))
name274 = "Cat.Smol._._.compose-arrow-identity"
d274 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d274 = erased
name276 = "Cat.Smol._._.compose-compose"
d276 ::
  T8 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d276 = erased
name278 = "Cat.Smol._._.compose-identity-arrow"
d278 ::
  T8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d278 = erased
name280 = "Cat.Smol._._.identity"
d280 :: T8 -> AgdaAny -> AgdaAny
d280 v0 = coe (d60 (coe v0))
name282 = "Cat.Smol._._->Set"
d282 a0 a1 a2 = ()
newtype T282
  = C1949 (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name312 = "Cat.Smol._._->Set.Map"
d312 :: T282 -> AgdaAny -> ()
d312 = erased
name318 = "Cat.Smol._._->Set.map"
d318 :: T282 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d318 v0
  = case coe v0 of
      C1949 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name324 = "Cat.Smol._._->Set.map-identity"
d324 :: T282 -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d324 = erased
name338 = "Cat.Smol._._->Set.map-compose"
d338 ::
  T282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d338 = erased
name350 = "Cat.Smol._.From"
d350 :: () -> (AgdaAny -> AgdaAny -> ()) -> T8 -> AgdaAny -> ()
d350 = erased
name358 = "Cat.Smol._.Triangle"
d358 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T8 ->
  AgdaAny ->
  MAlonzo.Code.Lib.Sigma.T16 -> MAlonzo.Code.Lib.Sigma.T16 -> ()
d358 = erased
name382 = "Cat.Smol._.triEq"
d382 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T8 ->
  AgdaAny ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Equality.T14
d382 = erased
name392 = "Cat.Smol._._._-FROM_"
d392 :: () -> (AgdaAny -> AgdaAny -> ()) -> T8 -> AgdaAny -> T8
d392 v0 v1 v2 v3 = du392 v2
du392 :: T8 -> T8
du392 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2)
      (\ v1 ->
         case coe v1 of
           MAlonzo.Code.Lib.Sigma.C30 v2 v3
             -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe d60 v0 v2) erased)
           _ -> MAlonzo.RTE.mazUnreachableError)
      (\ v1 ->
         case coe v1 of
           MAlonzo.Code.Lib.Sigma.C30 v2 v3
             -> coe
                  (\ v4 ->
                     case coe v4 of
                       MAlonzo.Code.Lib.Sigma.C30 v5 v6
                         -> coe
                              (\ v7 ->
                                 case coe v7 of
                                   MAlonzo.Code.Lib.Sigma.C30 v8 v9
                                     -> coe
                                          (\ v10 ->
                                             case coe v10 of
                                               MAlonzo.Code.Lib.Sigma.C30 v11 v12
                                                 -> coe
                                                      (\ v13 ->
                                                         case coe v13 of
                                                           MAlonzo.Code.Lib.Sigma.C30 v14 v15
                                                             -> coe
                                                                  (MAlonzo.Code.Lib.Sigma.C30
                                                                     (coe d68 v0 v2 v5 v8 v11 v14)
                                                                     erased)
                                                           _ -> MAlonzo.RTE.mazUnreachableError)
                                               _ -> MAlonzo.RTE.mazUnreachableError)
                                   _ -> MAlonzo.RTE.mazUnreachableError)
                       _ -> MAlonzo.RTE.mazUnreachableError)
           _ -> MAlonzo.RTE.mazUnreachableError)
      erased erased erased
name442 = "Cat.Smol._._-TO_"
d442 :: () -> (AgdaAny -> AgdaAny -> ()) -> T8 -> AgdaAny -> T8
d442 v0 v1 v2 v3 = du442 v2
du442 :: T8 -> T8
du442 v0 = coe (du392 (coe (du182 (coe v0))))
