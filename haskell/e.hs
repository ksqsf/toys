module Main where

e = convert (2:ones)
ones = 1:ones
convert (d:x) = d:convert (normalise 2 (0:mult x))
mult x = [10*a | a<-x]
normalise c (d:e:x)
  | e+9<c     = d:normalise (c+1) (e:x)
  | otherwise = carry c (d:normalise (c+1) (e:x))
carry c (d:e:x) = d + e `quot` c : e `rem` c : x

main = print e
