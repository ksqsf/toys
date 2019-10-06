{-# LANGUAGE RankNTypes #-}

ex = let f = take 3 in (f "hello", f (repeat 1))

-- foo :: (t -> b) -> (t, t) -> (b, b)
-- so xs and ys are of the same type... this is a limitation!
foo f (xs, ys) = (f xs, f ys)
-- foo (take 3) ("hello", repeat 1)

foo_stupid f1 f2 (xs, ys) = (f1 xs, f2 ys)
ex_stupid = foo_stupid (take 3) (take 3) ("hello", repeat 1)

foo' :: (forall a . [a] -> [a]) -> ([b],[c]) -> ([b],[c])
foo' f (xs, ys) = (f xs, f ys)
ex1 = foo' (take 3) ("hello", repeat 1)
