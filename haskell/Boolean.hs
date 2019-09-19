import Prelude hiding ((/=), (==), not, and, or, (&&), (||))


(==) :: Bool -> Bool -> Bool
(==) True True   = True
(==) False False = True
(==) _ _         = False


not :: Bool -> Bool
not True  = False
not False = True

not' = (==False)


xor, and, or :: Bool -> Bool -> Bool
xor b1 b2 = not (b1 == b2)
and True b1 = b1
and False _ = False
or False b1 = b1
or True _ = True


condition :: Bool -> a -> a -> a
condition True  t f = t
condition False t f = f

infix 4 ==
infix 4 /=
infixl 3 &&
infixl 2 ||

(||) = or
(&&) = and
(/=) = xor

-- Half adder
hA :: Bool -> Bool -> (Bool, Bool)
hA a b = (a /= b, a && b)

-- Full adder
fA a b c =
  let (axb,   aab)   = hA a   b in
  let (axbxc, axbac) = hA axb c in (axbxc, aab || axbac)

nand, nor = Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

nor False False = True
nor _ _ = False

not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor  b b

xor1 b1 b2 = nand (nand b1 nb1b2) (nand b2 nb1b2)
  where nb1b2 = nand b1 b2

