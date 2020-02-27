{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.CS410Z45Z19.Lec.Three where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.CS410Z45Z19.Lec.Two
import qualified MAlonzo.Code.Lib.Equality
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Splatoid

name10 = "CS410-19.Lec.Three.SmolCat"
d10 a0 a1 = ()
data T10
  = C39 (AgdaAny -> AgdaAny)
        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name62 = "CS410-19.Lec.Three.SmolCat.identity"
d62 :: T10 -> AgdaAny -> AgdaAny
d62 v0
  = case coe v0 of
      C39 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name70 = "CS410-19.Lec.Three.SmolCat.compose"
d70 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d70 v0
  = case coe v0 of
      C39 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name78 = "CS410-19.Lec.Three.SmolCat.compose-identity-arrow"
d78 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d78 = erased
name86 = "CS410-19.Lec.Three.SmolCat.compose-arrow-identity"
d86 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d86 = erased
name102 = "CS410-19.Lec.Three.SmolCat.compose-compose"
d102 ::
  T10 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d102 = erased
name124 = "CS410-19.Lec.Three._.PREORDER"
d124 ::
  () ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Splatoid.T6) ->
  MAlonzo.Code.CS410Z45Z19.Lec.Two.T20 -> T10
d124 v0 v1 v2 = du124 v2
du124 :: MAlonzo.Code.CS410Z45Z19.Lec.Two.T20 -> T10
du124 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2)
      (\ v1 -> coe MAlonzo.Code.CS410Z45Z19.Lec.Two.d40 v0 v1)
      (\ v1 v2 v3 ->
         coe MAlonzo.Code.CS410Z45Z19.Lec.Two.d48 v0 v1 v2 v3)
      erased erased erased
name160 = "CS410-19.Lec.Three._.MONOID"
d160 :: () -> MAlonzo.Code.CS410Z45Z19.Lec.Two.T116 -> T10
d160 v0 v1 = du160 v1
du160 :: MAlonzo.Code.CS410Z45Z19.Lec.Two.T116 -> T10
du160 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2)
      (\ v1 -> MAlonzo.Code.CS410Z45Z19.Lec.Two.d140 (coe v0))
      (\ v1 v2 v3 -> MAlonzo.Code.CS410Z45Z19.Lec.Two.d142 (coe v0))
      erased erased erased
name168 = "CS410-19.Lec.Three._.DISCRETE"
d168 :: () -> T10
d168 v0 = du168
du168 :: T10
du168
  = coe
      (\ v0 v1 v2 v3 v4 -> C39 v0 v1) erased erased erased erased erased
name184 = "CS410-19.Lec.Three._.OP"
d184 :: () -> (AgdaAny -> AgdaAny -> ()) -> T10 -> T10
d184 v0 v1 v2 = du184 v2
du184 :: T10 -> T10
du184 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2) (\ v1 -> coe d62 v0 v1)
      (\ v1 v2 v3 v4 v5 -> coe d70 v0 v3 v2 v1 v5 v4) erased erased
      erased
name212 = "CS410-19.Lec.Three._._-SmolCat>_"
d212 a0 a1 a2 a3 a4 a5 = ()
data T212
  = C2781 (AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name238 = "CS410-19.Lec.Three._._-SmolCat>_.Map"
d238 :: T212 -> AgdaAny -> AgdaAny
d238 v0
  = case coe v0 of
      C2781 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name244 = "CS410-19.Lec.Three._._-SmolCat>_.map"
d244 :: T212 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d244 v0
  = case coe v0 of
      C2781 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name248 = "CS410-19.Lec.Three._._-SmolCat>_.map-identity"
d248 :: T212 -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d248 = erased
name260 = "CS410-19.Lec.Three._._-SmolCat>_.map-compose"
d260 ::
  T212 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d260 = erased
name284 = "CS410-19.Lec.Three._.EXTRA"
d284 :: T212
d284
  = coe
      (\ v0 v1 v2 v3 -> C2781 v0 v1) erased
      (\ v0 v1 v2 ->
         MAlonzo.Code.Lib.Sigma.d26
           (coe (MAlonzo.Code.CS410Z45Z19.Lec.Two.du88 (coe v0) (coe v1))))
      erased erased
name328 = "CS410-19.Lec.Three._.From"
d328 :: () -> (AgdaAny -> AgdaAny -> ()) -> T10 -> AgdaAny -> ()
d328 = erased
name336 = "CS410-19.Lec.Three._.Triangle"
d336 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T10 ->
  AgdaAny ->
  MAlonzo.Code.Lib.Sigma.T16 -> MAlonzo.Code.Lib.Sigma.T16 -> ()
d336 = erased
name360 = "CS410-19.Lec.Three._.triEq"
d360 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T10 ->
  AgdaAny ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Sigma.T16 ->
  MAlonzo.Code.Lib.Equality.T14 -> MAlonzo.Code.Lib.Equality.T14
d360 = erased
name370 = "CS410-19.Lec.Three._._._-FROM_"
d370 :: () -> (AgdaAny -> AgdaAny -> ()) -> T10 -> AgdaAny -> T10
d370 v0 v1 v2 v3 = du370 v2
du370 :: T10 -> T10
du370 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C39 v1 v2)
      (\ v1 ->
         case coe v1 of
           MAlonzo.Code.Lib.Sigma.C30 v2 v3
             -> coe (MAlonzo.Code.Lib.Sigma.C30 (coe d62 v0 v2) erased)
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
                                                                     (coe d70 v0 v2 v5 v8 v11 v14)
                                                                     erased)
                                                           _ -> MAlonzo.RTE.mazUnreachableError)
                                               _ -> MAlonzo.RTE.mazUnreachableError)
                                   _ -> MAlonzo.RTE.mazUnreachableError)
                       _ -> MAlonzo.RTE.mazUnreachableError)
           _ -> MAlonzo.RTE.mazUnreachableError)
      erased erased erased
name420 = "CS410-19.Lec.Three._._-TO_"
d420 :: () -> (AgdaAny -> AgdaAny -> ()) -> T10 -> AgdaAny -> T10
d420 v0 v1 v2 v3 = du420 v2
du420 :: T10 -> T10
du420 v0 = coe (du370 (coe (du184 (coe v0))))
name434 = "CS410-19.Lec.Three._._.compose"
d434 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d434 v0 = coe (d70 (coe v0))
name436 = "CS410-19.Lec.Three._._.compose-arrow-identity"
d436 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d436 = erased
name438 = "CS410-19.Lec.Three._._.compose-compose"
d438 ::
  T10 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d438 = erased
name440 = "CS410-19.Lec.Three._._.compose-identity-arrow"
d440 ::
  T10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d440 = erased
name442 = "CS410-19.Lec.Three._._.identity"
d442 :: T10 -> AgdaAny -> AgdaAny
d442 v0 = coe (d62 (coe v0))
name444 = "CS410-19.Lec.Three._._->Set"
d444 a0 a1 a2 = ()
newtype T444
  = C6839 (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
name474 = "CS410-19.Lec.Three._._->Set.Map"
d474 :: T444 -> AgdaAny -> ()
d474 = erased
name480 = "CS410-19.Lec.Three._._->Set.map"
d480 :: T444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d480 v0
  = case coe v0 of
      C6839 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name486 = "CS410-19.Lec.Three._._->Set.map-identity"
d486 :: T444 -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d486 = erased
name500 = "CS410-19.Lec.Three._._->Set.map-compose"
d500 ::
  T444 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d500 = erased
name508 = "CS410-19.Lec.Three._.TUPLE"
d508 :: () -> T444
d508 v0 = du508
du508 :: T444
du508
  = coe
      (\ v0 v1 v2 v3 -> C6839 v1) erased
      (\ v0 v1 ->
         case coe v1 of
           0 -> erased
           _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
                let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                coe
                  (\ v4 v5 ->
                     case coe v5 of
                       MAlonzo.Code.Lib.Sigma.C30 v6 v7
                         -> coe
                              (MAlonzo.Code.Lib.Sigma.C30 (coe v6) (coe d480 du508 v3 v2 v4 v7))
                       _ -> MAlonzo.RTE.mazUnreachableError))
      erased erased
