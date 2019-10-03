{-# LANGUAGE RankNTypes
           , RelaxedPolyRec
           , DeriveFunctor
           , TupleSections
           , ScopedTypeVariables
           , UndecidableInstances
  #-}
import qualified Data.List as List
import qualified Data.MultiSet as Set
import qualified Data.Map as Map
import Control.Monad.Free

-- | value = Bool will suffice for this code, but 
-- more general Tables are certainly reasonable
data Table key value = Table
  { keys :: [key]
      -- ^ For iterating and looping purposes
  , rows :: Set.MultiSet (value, Row key value)
      -- ^ An unordered collection of Rows associated to labels
  } deriving(Show)

type Row key value = Map.Map key value

numKeys :: Table k v -> Int
numKeys = length . keys

numRows :: Table k v -> Int
numRows = Set.size . rows

--Assume all tables are full
getKey :: Ord k => k -> Row k v -> v
getKey k row
  = maybe undefined id (Map.lookup k row)

emptyBinTable :: Table key value
emptyBinTable = Table [] Set.empty

-- | We score the keys by how many labels the key could get correct.
-- This method returns two values: the first is if the model applied a 
-- positive correlation, the second is if it assumes a negative correlation.
scores :: Ord k => k -> Table k Bool -> (Int, Int)
scores k tab = Set.fold (indicator k) ((,0) 0) $ rows tab
  where indicator :: Ord k => k -> (Bool, Row k Bool) -> (Int,Int) -> (Int,Int)
        indicator k (label, row) (pos, neg)
          = case Map.lookup k row of
                 Just a -> (pos + fromEnum (label==a), neg + fromEnum (label/=a))
                 Nothing -> (pos, neg)

-- | Loop over all the keys and find the one that predicts with highest accuracy
bestKey :: Ord k => Table k Bool -> k
bestKey tab
  = let bestScore k
          = let (pos, neg) = scores k tab
             in (k, max pos neg)
        maxScores = fmap bestScore (keys tab)
        (bestKey, _)
          = List.maximumBy (\(_,s) (_,s') -> s `compare` s') maxScores
     in bestKey

removeKey :: (Ord k, Ord v) => k -> Table k v -> Table k v
removeKey k tab =
  emptyBinTable
    { keys = (List.\\) (keys tab) [k]
    , rows = Set.map (\(lab,row) -> (lab, Map.delete k row)) (rows tab)
    }

-- | Split a table into two tables based on the value of one key.
-- Also remove the key from the new tables.
filterOn :: Ord k => k -> Table k Bool -> (Table k Bool, Table k Bool)
filterOn k tab
  = let kTrue = getKey k . snd -- ^ predicate to test if key k is true for some row
        trueRows = Set.filter kTrue (rows tab)
        falseRows = Set.filter (not . kTrue) (rows tab)
     in (removeKey k tab{rows = trueRows}, removeKey k tab{rows = falseRows})

-- | Ignore all the rows and just guess a boolean based on class label
bestGuess :: Ord k => Table k Bool -> Bool
bestGuess tab
  = let nTrue = Set.fold (\(b,_) accum -> accum + fromEnum b) 0 (rows tab)
        -- ^ count number of true labels
     in if 2 * nTrue >= numRows tab
           then True
           else False

discriminate :: Ord k => Table k Bool -> Either (Row k Bool -> Table k Bool) Bool
discriminate tab
  | numKeys tab == 0
    = Right $ bestGuess tab
  | otherwise
    = let key = bestKey tab
          (trueTab, falseTab) = filterOn key tab
       in Left $ \row ->
          let bool = getKey key row
           in if bool then trueTab else falseTab

-- | Finally time to use this!
type TreeM r c = Free ((->) r) c

learn :: Ord k => Table k Bool -> TreeM (Row k Bool) Bool
learn = anaF discriminate

--cataF :: Functor f => (f a -> a) -> Free f a -> a
predict :: Ord k => Row k Bool -> TreeM (Row k Bool) Bool -> Bool
predict row model = cataF ($ row) model
