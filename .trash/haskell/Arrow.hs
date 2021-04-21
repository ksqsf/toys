{-# LANGUAGE Arrows #-}

import Control.Category
import Control.Arrow

dup :: Arrow arrow => arrow a (a,a)
dup = arr $ \a -> (a,a)

test' :: Integer -> (Bool, Bool)
test' = proc x -> do
  px <- (+1) -< x
  ex <- even -< px
  (a,b) <- dup -< ex
  returnA -< (not a, b)

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Category SF where
  id = SF (Prelude.id)
  (.) (SF f) (SF g) = SF ((Prelude..) f g)

instance Arrow SF where
  arr f = SF (map f)
  -- f :: [b] -> [c]
  -- bd :: [(b,d)]
  -- :: [(c,d)]
  first (SF f) = SF $ \bd -> let (bs, ds) = unzip bd
                             in zip (f bs) ds

instance ArrowChoice SF where
  left (SF f) = SF $ \xs -> combine xs (f [y | Left y <- xs])
    where combine (Left y:xs) (z:zs) = Left z: combine xs zs
          combine (Right y:xs) zs = Right y: combine xs zs
          combine [] zs = []

mapA' :: SF b c -> SF [b] [c]
mapA' (SF f) = SF (\xs -> map f xs)

listcase :: [t] -> Either () (t, [t])
listcase [] = Left ()
listcase (x:xs) = Right (x,xs)

mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = -- arr listcase >>> (arr (const []) ||| ((f *** mapA f) >>> arr (uncurry (:))))
  proc xs -> do
    case xs of
      [] -> returnA -< []
      (x:xs') -> do
        y <- f -< x
        ys <- mapA f -< xs'
        returnA -< y:ys

instance ArrowLoop SF where
  loop (SF f) = SF $ \as ->
    let (bs,cs) = unzip (f (zip as (stream cs)))
    in bs
    where stream ~(x:xs) = x:stream xs

class ArrowLoop a => ArrowCircuit a where
  delay :: b -> a b b

instance ArrowCircuit SF where
  delay x = SF (init Prelude.. (x:))

showBools :: [Bool] -> String
showBools [] = ""
showBools (False : xs) = '_' : showBools xs
showBools (True  : xs) = '^' : showBools xs
toBools :: String -> [Bool]
toBools "" = []
toBools ('_': xs) = False : toBools xs
toBools ('^': xs) = True: toBools xs
toBools _ = error "only ^ and _ can be used."

s1,s2,s3,s4 :: [Bool]
s1 = toBools "____^^^^^____^^^^^____"
s2 = toBools "_^_^_^_^_^_^_^_^_^_^_^"
s3 = toBools "^^^^^^^^^^^^^^^^^^^^^^"
s4 = toBools "______________________"

edge :: SF Bool Bool
-- edge = arr Prelude.id &&& delay False >>> arr detect
--   where detect (a,b) = a && not b
edge = proc b -> do
  c <- delay False -< b
  returnA -< b && not c

counter :: ArrowCircuit a => a Bool Int
counter = proc reset -> do
  rec output <- returnA -< if reset then 0 else next
      next   <- delay 1 -< output + 1
  returnA -< output

flipflop :: SF (Bool,Bool) (Bool,Bool)
flipflop = proc (reset,set) -> do
  rec c <- delay False -< nor' reset d
      d <- delay True  -< nor' set c
  returnA -< (c,d)
  where nor' a b = not (a || b)
