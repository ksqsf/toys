{-# LANGUAGE TemplateHaskell #-}
module ZipN2 where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Data.List
import Control.Applicative

genBT :: String -> Int -> Q ([TyVarBndr], [TypeQ])
genBT name n = do
  let ns = [name ++ (show i) | i <- [1..n]]
  tvb <- sequence $ map (return.plainTV.mkName) ns
  typ <- sequence $ map (return.varT.mkName) ns
  return (tvb, typ)

genPE :: String -> Int -> Q ([PatQ], [ExpQ])
genPE name n = do
  let ns = [name ++ (show i) | i <- [1..n]]
  pat <- sequence $ map (return.varP.mkName) ns
  exp <- sequence $ map (return.varE.mkName) ns
  return (pat, exp)

applyCurryTQ :: [TypeQ] -> TypeQ
applyCurryTQ = foldr1 (\t1 -> appT (appT arrowT t1))

applyConTQ :: [TypeQ] -> TypeQ
applyConTQ xs = foldl1 appT xs

applyExpQ :: [ExpQ] -> ExpQ
applyExpQ = appsE

zipN :: Int -> DecsQ
zipN n = do
  let name = mkName ("zip" ++ show n)
  (tvb, tvar) <- genBT "a" n
  let listvar = map (appT listT) tvar
  let lstuple = appT listT (applyConTQ (tupleT n : tvar))
  let typ = applyCurryTQ (listvar ++ [lstuple])
  sig <- sigD name (forallT tvb (return []) typ)
  (py, pyv) <- genPE "y" n
  (px, pxv) <- genPE "x" n
  (pxs, pxsv) <- genPE "xs" n
  let pcons x xs = [p| $x : $xs |]
  let matchp = tupP (zipWith pcons px pxs)
  let matchb = [e| $(tupE pxv) : $(applyExpQ (varE name : pxsv))|]
  let body = normalB [e| case $(tupE pyv) of
                           $matchp -> $matchb
                           _ -> [] |]
  fun <- funD name [(clause py body [])]
  return [sig, fun]

