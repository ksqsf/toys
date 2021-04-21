data Shape a = Circle a | Square a | Rectangle a a

instance (Eq a) => Eq (Shape a) where
  Circle r1 == Circle r2 = r1 == r2

