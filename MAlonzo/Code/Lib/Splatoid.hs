{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Lib.Splatoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Lib.Equality

name6 = "Lib.Splatoid.Splatoid"
d6 a0 = ()
data T6 = C26
name18 = "Lib.Splatoid.Splatoid.Splat"
d18 :: T6 -> ()
d18 = erased
name24 = "Lib.Splatoid.Splatoid.splat"
d24 :: T6 -> AgdaAny -> AgdaAny -> MAlonzo.Code.Lib.Equality.T14
d24 = erased
name28 = "Lib.Splatoid.SplatZero"
d28 :: T6
d28 = erased
name30 = "Lib.Splatoid.SplatOne"
d30 :: T6
d30 = erased
name46 = "Lib.Splatoid._.SplatEq"
d46 ::
  MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> AgdaAny -> T6
d46 = erased
name50 = "Lib.Splatoid._.SplatSing"
d50 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> T6
d50 = erased
