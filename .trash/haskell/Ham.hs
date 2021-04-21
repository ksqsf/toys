module Ham where

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x == y      = x:merge xs ys
  | x <  y      = x:merge xs (y:ys)
  | x >  y      = y:merge (x:xs) ys

ham = 1 : merge ham2 (merge ham3 ham5)
  where ham2 = map (*2) ham
        ham3 = map (*3) ham
        ham5 = map (*5) ham
