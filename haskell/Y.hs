import Unsafe.Coerce

y :: (a -> a) -> a
y f = (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

fact' fact' n = if n == 0 then 1 else n * fact' (n-1)
fact = y fact'
