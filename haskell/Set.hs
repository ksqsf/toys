-- http://groups.csail.mit.edu/mac/users/adams/BB/92-10.ps

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data Tree a = Empty
            | Node
              { tval :: a
              , tcnt :: Int
              , tleft :: (Tree a)
              , tright :: (Tree a)
              }
            deriving (Show, Eq, Functor, Foldable, Traversable)

tsize :: Tree a -> Int
tsize Empty = 0
tsize (Node _ cnt _ _) = cnt

tnode :: a -> (Tree a) -> (Tree a) -> (Tree a)
tnode v l r = Node v (1 + tsize l + tsize r) l r

tmember :: Ord a => (Tree a) -> a -> Bool
tmember Empty _ = False
tmember (Node x _ l r) y
  | x == y = True
  | y <  x = tmember l y
  | otherwise = tmember r y

tmin :: (Tree a) -> (Tree a)
tmin t@(Node _ _ Empty r) = t
tmin (Node _ _ l r) = tmin l
tmin Empty = error "empty"

tsingleL, tsingleR, tdoubleL, tdoubleR :: (Tree a) -> (Tree a)
tsingleL (Node av _ al (Node bv _ bl br)) = tnode bv (tnode av al bl) br
tsingleR (Node av _ (Node bv _ bl br) ar) = tnode bv bl (tnode av br ar)
tdoubleL (Node av _ x (Node cv cc (Node bv _ y z) w)) =
  tnode bv (tnode av x y) (tnode cv z w)
tdoubleR (Node av _ (Node cv cc x (Node bv _ y z)) w) =
  tnode bv (tnode cv x y) (tnode av z w)

exL, exR, ex2L, ex2R :: Tree Char
exL = Node 'a' 2 Empty (Node 'b' 1 Empty Empty)
exR = Node 'a' 2 (Node 'b' 1 Empty Empty) Empty
ex2L = Node 'a' 3 Empty (Node 'c' 2 (Node 'b' 1 Empty Empty) Empty)
ex2R = Node 'a' 3 (Node 'c' 2 Empty (Node 'b' 1 Empty Empty)) Empty

weight :: Int
weight = 5

tnode' :: a -> (Tree a) -> (Tree a) -> (Tree a)
tnode' v l r = let ln = tsize l
                   rn = tsize r
               in if ln + rn < 2 then tnode v l r
                  else if rn > weight * ln then
                         let Node _ _ rl rr = r
                             rln = tsize rl
                             rrn = tsize rr
                             p = tnode v l r
                         in if rln < rrn then tsingleL p else tdoubleL p
                  else if ln > weight * rn then
                         let Node _ _ ll lr = l
                             lln = tsize ll
                             lrn = tsize lr
                             p = tnode v l r
                         in if lrn < lln then tsingleR p else tdoubleR p
                  else tnode v l r

tadd :: Ord a => Tree a -> a -> Tree a
tadd Empty a = Node a 1 Empty Empty
tadd t@(Node b _ l r) a
  | a < b = tnode' b (tadd l a) r
  | b < a = tnode' b l (tadd r a)
  | otherwise = t

tdelete :: Ord a => Tree a -> a -> Tree a
tdelete Empty _ = Empty
tdelete t@(Node b _ l r) a
  | a < b = tnode' b (tdelete l a) r
  | b < a = tnode' b l (tdelete r a)
  | otherwise = tdeleteRoot t

tdeleteRoot :: Ord a => Tree a -> Tree a
tdeleteRoot (Node _ _ Empty r) = r
tdeleteRoot (Node _ _ l Empty) = l
tdeleteRoot (Node _ _ l r) = let Node v _ _ _ = tmin r in tnode' v l (tdelete r v)

-- A slightly faster algorithm.
-- tdeleteMin :: Ord a => Tree a -> Tree a
-- tdeleteMin (Node _ _ Empty r) = r
-- tdeleteMin (Node v _ l r) = let Node v _ _ _ = tmin r in tnode' v (tdeleteMin l) r

fromList :: Ord a => [a] -> Tree a
fromList = foldl tadd Empty

toList :: Tree a -> [a]
toList t = foldr (:) [] t

union :: Ord a => Tree a -> Tree a -> Tree a
union Empty tree2 = tree2
union tree1 Empty = tree1
union tree1 (Node v _ l r) = concat3 v (union l' l) (union r' r)
  where l' = splitLt tree1 v
        r' = splitGt tree1 v

concat3 :: Ord a => a -> Tree a -> Tree a -> Tree a
concat3 v Empty r = tadd r v
concat3 v l Empty = tadd l v
concat3 v l@(Node lv ln ll lr) r@(Node rv rn rl rr)
  | weight*ln < rn = tnode' rv (concat3 v l rl) rr
  | weight*rn < ln = tnode' lv ll (concat3 v lr r)
  | otherwise = tnode v l r

splitLt :: Ord a => Tree a -> a -> Tree a
splitLt Empty x = Empty
splitLt (Node v _ l r) x
  | x < v = splitLt l x
  | v < x = concat3 v l (splitLt r x)
  | otherwise = l

splitGt :: Ord a => Tree a -> a -> Tree a
splitGt Empty x = Empty
splitGt (Node v _ l r) x
  | x > v = splitGt r x
  | v > x = concat3 v (splitGt l x) r
  | otherwise = r

difference :: Ord a => Tree a -> Tree a -> Tree a
difference Empty s = Empty
difference s Empty = s
difference s (Node v _ l r) = concat2 (difference l' l) (difference r' r)
  where l' = splitLt s v
        r' = splitGt s v

concat2 :: Ord a => Tree a -> Tree a -> Tree a
concat2 t1 Empty = t1
concat2 t1 t2 = concat3 t2min t1 (tdelete t2 t2min)
  where t2min = tval (tmin t2)

intersection :: Ord a => Tree a -> Tree a -> Tree a
-- intersection a b = difference b (difference b a)
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection s (Node v _ l r) = if tmember s v then
                                  concat3 v (intersection l' l) (intersection r' r)
                                else
                                  concat2 (intersection l' l) (intersection r' r)
  where l' = splitLt s v
        r' = splitGt s v

s = fromList [1..10]
t = fromList [5..15]
