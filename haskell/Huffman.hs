import Data.List (insertBy, sortBy, nubBy)
import Data.Ord (comparing)

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show

htree [(_, t)] = t
htree ((w1, t1):(w2, t2):wts) =
  htree $ insertBy (comparing fst) (w1+w2, Branch t1 t2) wts

serialize (Leaf x) = [(x, "")]
serialize (Branch l r) =
  [(x, '0' : code) | (x, code) <- serialize l] ++
  [(x, '1' : code) | (x, code) <- serialize r]

huffman freq = sortBy (comparing fst) $ serialize $
  htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]

freq s = [ fromIntegral (length (filter (==c) s)) / fromIntegral (length s) | c <- s ]
encode s = huffman $ nubBy (\ x y -> snd x == snd y) $ zip s (freq s)
