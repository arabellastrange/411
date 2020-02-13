{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.CS410Z45Z19.Lec.One where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Lib.Pi
import qualified MAlonzo.Code.Lib.Sigma
import qualified MAlonzo.Code.Lib.Two

name12 = "CS410-19.Lec.One.TREE-UNSAFE.Tree"
d12 a0 a1 = ()
data T12 = C14 | C16 T12 AgdaAny T12
name18 = "CS410-19.Lec.One.TREE-UNSAFE.List"
d18 a0 a1 = ()
data T18 = C20 | C22 AgdaAny T18
name24 = "CS410-19.Lec.One.TREE-UNSAFE.insert"
d24 ::
  () ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) ->
  AgdaAny -> T12 -> T12
d24 v0 v1 v2 v3 = du24 v1 v2 v3
du24 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) ->
  AgdaAny -> T12 -> T12
du24 v0 v1 v2
  = case coe v2 of
      C14 -> coe (C16 (coe v2) (coe v1) (coe v2))
      C16 v3 v4 v5
        -> let v6 = coe v0 v1 v4 in
           case coe v6 of
             MAlonzo.Code.Lib.Two.C6
               -> coe
                    (C16 (coe v3) (coe v4) (coe (du24 (coe v0) (coe v1) (coe v5))))
             MAlonzo.Code.Lib.Two.C8
               -> coe
                    (C16 (coe (du24 (coe v0) (coe v1) (coe v3))) (coe v4) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name56 = "CS410-19.Lec.One.TREE-UNSAFE.makeTree"
d56 ::
  () -> (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) -> T18 -> T12
d56 v0 v1 v2 = du56 v1 v2
du56 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) -> T18 -> T12
du56 v0 v1
  = case coe v1 of
      C20 -> coe C14
      C22 v2 v3
        -> coe (du24 (coe v0) (coe v2) (coe (du56 (coe v0) (coe v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
name62 = "CS410-19.Lec.One.TREE-UNSAFE._++_"
d62 ::
  () ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) ->
  T18 -> T18 -> T18
d62 v0 v1 v2 v3 = du62 v2 v3
du62 :: T18 -> T18 -> T18
du62 v0 v1
  = case coe v0 of
      C20 -> coe v1
      C22 v2 v3 -> coe (C22 (coe v2) (coe (du62 (coe v3) (coe v1))))
      _ -> MAlonzo.RTE.mazUnreachableError
name72 = "CS410-19.Lec.One.TREE-UNSAFE.flatten"
d72 ::
  () -> (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) -> T12 -> T18
d72 v0 v1 v2 = du72 v2
du72 :: T12 -> T18
du72 v0
  = case coe v0 of
      C14 -> coe C20
      C16 v1 v2 v3
        -> coe
             (du62
                (coe (du72 (coe v1))) (coe (C22 (coe v2) (coe (du72 (coe v3))))))
      _ -> MAlonzo.RTE.mazUnreachableError
name80 = "CS410-19.Lec.One.TREE-UNSAFE.sort"
d80 ::
  () -> (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) -> T18 -> T18
d80 v0 v1 v2 = du80 v1 v2
du80 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Two.T4) -> T18 -> T18
du80 v0 v1 = coe (du72 (coe (du56 (coe v0) (coe v1))))
name86 = "CS410-19.Lec.One.SORTNAT-UNSAFE.leKey"
d86 :: Integer -> Integer -> MAlonzo.Code.Lib.Two.T4
d86 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Lib.Two.C8
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe MAlonzo.Code.Lib.Two.C6
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (d86 (coe v2) (coe v3))
name98 = "CS410-19.Lec.One.SORTNAT-UNSAFE._._++_"
d98 :: T18 -> T18 -> T18
d98 = coe du62
name102 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.List"
d102 = ()
name104 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.Tree"
d104 = ()
name108 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.flatten"
d108 :: T12 -> T18
d108 = coe du72
name110 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.insert"
d110 :: Integer -> T12 -> T12
d110 = coe (du24 (coe d86))
name114 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.makeTree"
d114 :: T18 -> T12
d114 = coe (du56 (coe d86))
name118 = "CS410-19.Lec.One.SORTNAT-UNSAFE._.sort"
d118 :: T18 -> T18
d118 = coe (du80 (coe d86))
name132 = "CS410-19.Lec.One.SORTNAT-UNSAFE.myList"
d132 :: T18
d132
  = coe
      (C22
         (coe (5 :: Integer))
         (coe
            (C22
               (coe (7 :: Integer))
               (coe
                  (C22
                     (coe (4 :: Integer))
                     (coe
                        (C22
                           (coe (9 :: Integer))
                           (coe
                              (C22
                                 (coe (10 :: Integer))
                                 (coe
                                    (C22
                                       (coe (3 :: Integer))
                                       (coe (C22 (coe (6 :: Integer)) (coe C20))))))))))))))
name134 = "CS410-19.Lec.One.SORTNAT-UNSAFE.myTree"
d134 :: T12
d134 = coe (du56 (coe d86) (coe d132))
name136 = "CS410-19.Lec.One.SORTNAT-UNSAFE.myListSorted"
d136 :: T18
d136 = coe (du80 (coe d86) (coe d132))
name138 = "CS410-19.Lec.One.LeNat"
d138 :: Integer -> Integer -> ()
d138 = erased
name152 = "CS410-19.Lec.One.owotoNat"
d152 :: Integer -> Integer -> MAlonzo.Code.Lib.Sigma.T16
d152 v0 v1
  = case coe v0 of
      0 -> coe
             (MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C6) erased)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    (MAlonzo.Code.Lib.Sigma.C30 (coe MAlonzo.Code.Lib.Two.C8) erased)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (d152 (coe v2) (coe v3))
name174 = "CS410-19.Lec.One.TREE.Bound"
d174 a0 a1 a2 = ()
data T174 = C176 | C178 AgdaAny | C180
name182 = "CS410-19.Lec.One.TREE.LeB"
d182 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) ->
  T174 -> T174 -> ()
d182 = erased
name196 = "CS410-19.Lec.One.TREE.Tree"
d196 a0 a1 a2 a3 a4 = ()
data T196 = C202 AgdaAny | C206 AgdaAny T196 T196
name214 = "CS410-19.Lec.One.TREE.insert"
d214 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) ->
  T174 -> T174 -> AgdaAny -> AgdaAny -> AgdaAny -> T196 -> T196
d214 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du214 v2 v5 v6 v7 v8
du214 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) ->
  AgdaAny -> AgdaAny -> AgdaAny -> T196 -> T196
du214 v0 v1 v2 v3 v4
  = case coe v4 of
      C202 v5
        -> coe (C206 (coe v1) (coe (C202 (coe v2))) (coe (C202 (coe v3))))
      C206 v5 v6 v7
        -> let v8 = coe v0 v1 v5 in
           case coe v8 of
             MAlonzo.Code.Lib.Sigma.C30 v9 v10
               -> case coe v9 of
                    MAlonzo.Code.Lib.Two.C6
                      -> coe
                           (C206
                              (coe v5)
                              (coe (du214 (coe v0) (coe v1) (coe v2) (coe v10) (coe v6)))
                              (coe v7))
                    MAlonzo.Code.Lib.Two.C8
                      -> coe
                           (C206
                              (coe v5) (coe v6)
                              (coe (du214 (coe v0) (coe v1) (coe v10) (coe v3) (coe v7))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name268 = "CS410-19.Lec.One.TREE.List"
d268 a0 a1 a2 = ()
data T268 = C270 | C272 AgdaAny T268
name274 = "CS410-19.Lec.One.TREE.makeTree"
d274 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) -> T268 -> T196
d274 v0 v1 v2 v3 = du274 v2 v3
du274 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) -> T268 -> T196
du274 v0 v1
  = case coe v1 of
      C270 -> coe (C202 erased)
      C272 v2 v3
        -> coe
             (du214
                (coe v0) (coe v2) erased erased (coe (du274 (coe v0) (coe v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
name284 = "CS410-19.Lec.One.TREE.OList"
d284 a0 a1 a2 a3 a4 = ()
data T284 = C290 AgdaAny | C294 AgdaAny AgdaAny T284
name302 = "CS410-19.Lec.One.TREE.flatNode"
d302 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) ->
  T174 -> T174 -> AgdaAny -> T284 -> T284 -> T284
d302 v0 v1 v2 v3 v4 v5 v6 v7 = du302 v5 v6 v7
du302 :: AgdaAny -> T284 -> T284 -> T284
du302 v0 v1 v2
  = case coe v1 of
      C290 v3 -> coe (C294 (coe v0) (coe v3) (coe v2))
      C294 v3 v4 v5
        -> coe
             (C294 (coe v3) (coe v4) (coe (du302 (coe v0) (coe v5) (coe v2))))
      _ -> MAlonzo.RTE.mazUnreachableError
name324 = "CS410-19.Lec.One.TREE.flatten"
d324 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) ->
  T174 -> T174 -> T196 -> T284
d324 v0 v1 v2 v3 v4 v5 = du324 v5
du324 :: T196 -> T284
du324 v0
  = case coe v0 of
      C202 v1 -> coe (C290 (coe v1))
      C206 v1 v2 v3
        -> coe
             (du302 (coe v1) (coe (du324 (coe v2))) (coe (du324 (coe v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
name334 = "CS410-19.Lec.One.TREE.sort"
d334 ::
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) -> T268 -> T284
d334 v0 v1 v2 = du334 v2
du334 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Sigma.T16) -> T268 -> T284
du334 v0
  = coe
      (MAlonzo.Code.Lib.Pi.du138
         (coe (du274 (coe v0))) (coe (\ v1 -> du324)))
