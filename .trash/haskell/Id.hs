{-# OPTIONS_GHC -ddump-simpl #-}
id'' :: t -> t
id'' x = x

-- ==================== Tidy Core ====================
-- Result size of Tidy Core
--   = {terms: 18, types: 12, coercions: 0, joins: 0/0}

-- -- RHS size: {terms: 3, types: 3, coercions: 0, joins: 0/0}
-- id'' :: forall t. t -> t
-- [GblId, Arity=1, Caf=NoCafRefs, Unf=OtherCon []]
-- id'' = \ (@ t_a7HL) (x_a7Hu :: t_a7HL) -> break<0>(x_a7Hu) x_a7Hu

-- -- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
-- $trModule1_r7Hs :: GHC.Prim.Addr#
-- [GblId, Caf=NoCafRefs, Unf=OtherCon []]
-- $trModule1_r7Hs = "main"#

-- -- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
-- $trModule2_r7HW :: GHC.Types.TrName
-- [GblId, Caf=NoCafRefs, Unf=OtherCon []]
-- $trModule2_r7HW = GHC.Types.TrNameS $trModule1_r7Hs

-- -- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
-- $trModule3_r7HX :: GHC.Prim.Addr#
-- [GblId, Caf=NoCafRefs, Unf=OtherCon []]
-- $trModule3_r7HX = "Main"#

-- -- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
-- $trModule4_r7HY :: GHC.Types.TrName
-- [GblId, Caf=NoCafRefs, Unf=OtherCon []]
-- $trModule4_r7HY = GHC.Types.TrNameS $trModule3_r7HX

-- -- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
-- Main.$trModule :: GHC.Types.Module
-- [GblId, Caf=NoCafRefs, Unf=OtherCon []]
-- Main.$trModule = GHC.Types.Module $trModule2_r7HW $trModule4_r7HY
