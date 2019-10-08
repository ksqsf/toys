{-# LANGUAGE TemplateHaskell #-}
module DeriveTopdown (deriveTopdown, derivings) where

import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Trans (lift)
import Data.List (foldl')
import qualified GHC.Generics as G

getTyVarCons :: Name -> Q ([TyVarBndr], [Con])
getTyVarCons name = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD  _ _ tvbs _ cons _ -> return (tvbs, cons)
        NewtypeD _ _ tvbs _ con _ -> return (tvbs, [con])
        TySynD _ vars type' -> undefined
        _ -> error "must be data/newtype"
    _ -> error "bad type name"

getTypeNames :: Type -> [Name]
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

third (a,b,c) = c

getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts) = concatMap getTypeNames (map snd sts)
getCompositeType (RecC n vars) = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2) = concatMap getTypeNames [snd st1, snd st2]
getCompositeType _ = undefined

getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV name) = name
getTVBName (KindedTV name _) = name

gen :: Name -> Name -> StateT [Type] Q [Dec]
gen cn tn = do
  (tvbs,cons) <- lift $ getTyVarCons tn
  let typeNames = map getTVBName tvbs
  instanceType <- lift $ foldl' appT (conT tn) $ map varT typeNames
  let context = if cn == ''G.Generic
                then []
                else (map (AppT (ConT cn)) (map VarT typeNames))
  let context_in_tuple = foldl1 AppT $ (TupleT (length context)) : context
  isMember <- if tvbs == []
              then lift $ isInstance cn [instanceType]
              else lift $ isInstance cn [ForallT tvbs [] instanceType]
  table <- get
  if isMember || elem instanceType table
    then return []
    else do let c = [StandaloneDerivD Nothing [context_in_tuple] (AppT (ConT cn) instanceType)]
            modify (instanceType:)
            let names = concatMap getCompositeType cons
            xs <- mapM (\n -> gen cn n) names
            return $ concat xs ++ c

deriveTopdown :: Name -> Name -> Q [Dec]
deriveTopdown cn tn = evalStateT (gen cn tn) []

derivings :: [Name] -> Name -> Q [Dec]
derivings cnms t = fmap concat (sequenceA $ map (\x -> deriveTopdown x t) cnms)

data C a b = A (B a) G
data B a = B a | F (D a)
data D b = D b | E b
data G = G
