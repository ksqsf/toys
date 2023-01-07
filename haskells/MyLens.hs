{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLens where
import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import Data.Ord
import Data.Monoid
import Data.Tree
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Either.Validation
import Data.Tree
import Control.Monad.State
import Data.List (stripPrefix)
import Numeric.Lens

-- data Ship = Ship
--   { _name :: String
--   , _numCrew :: Int
--   } deriving (Show)



-- getNumCrew :: Ship -> Int
-- getNumCrew = _numCrew

-- setNumCrew :: Ship -> Int -> Ship
-- setNumCrew ship newNumCrew = ship { _numCrew = newNumCrew }

-- numCrew :: Lens' Ship Int
-- numCrew = lens getNumCrew setNumCrew

-- getName :: Ship -> String
-- getName = _name

-- setName :: Ship -> String -> Ship
-- setName ship newName = ship{_name = newName}

-- name :: Lens' Ship String
-- name = lens getName setName

-- ship = Ship "Hello" 123

-- makeLenses ''Ship

conditional :: Lens' (Bool, a, a) a
-- :: (a -> f a) -> (Bool, a, a) -> f (Bool, a, a)
conditional = lens getter setter
  where getter (True, a, b) = a
        getter (False, a, b) = b
        setter (True, a, b) x = (True, x, b)
        setter (False, a, b) x = (False, a, x)

data Err =
    ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
  deriving (Show)
-- unlawful lens
msg :: Lens' Err String
msg = lens getMsg setMsg
  where getMsg (ReallyBadError msg) = msg
        getMsg (ExitCode _) = "?"
        setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
        setMsg (ExitCode n) _ = ExitCode n
err = ExitCode 233

data Builder =
  Builder { _context :: [String]
          , _build :: [String] -> String
          }

builder :: Lens' Builder String
builder = lens get set
  where get builder = _build builder ( _context builder )
        set builder new = builder { _context = [new], _build = concat }


data Temperature = Temperature
  { _location :: String
  , _celsius :: Float
  } deriving (Show)

makeLenses ''Temperature
temp = Temperature "Berlin" 7.0

celsiusToFahrenheit c = (c * (9/5)) + 32
fahrenheitToCelsius f = (f - 32) * (5/9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where getter = celsiusToFahrenheit . view celsius
        setter temp f = set celsius (fahrenheitToCelsius f) temp


-- data User = User
--   { _firstName :: String
--   , _lastName :: String
--   , _email :: String
--   } deriving (Show)
-- makeLenses ''User
-- jack = User "Jack" "Fool" "jack@fool.com"

-- username :: Lens' User String
-- username = email

-- fullName :: Lens' User String
-- fullName = lens getter setter
--   where
--     getter u = _firstName u ++ " " ++ _lastName u
--     setter u name = u { _firstName = takeWhile (/= ' ') name, _lastName = drop 1 $ dropWhile (/= ' ') name }
  
data ProducePrices =
  ProducePrices { _limePrice :: Float
                , _lemonPrice :: Float
                } deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens get set
  where get = _limePrice
        set pp x = pp{_limePrice=x `max` 0.0}
lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens g s
  where g = _lemonPrice
        s pp x = pp{_lemonPrice=x `max` 0.0}


data Promotion a = Promotion
  { _item :: a
  , _discountPercentage :: Double
  } deriving (Show)
item :: Lens (Promotion a) (Promotion b) a b
item = lens g s
  where
    g :: Promotion a -> a
    g = _item
    s :: Promotion a -> b -> Promotion b
    s p x = p{_item=x}

data Prefs a = Prefs
  { _best :: a
  , _worst :: a
  } deriving (Show)
makeLenses ''Prefs


-- Some dead-simple types which represent our game
data Player  = Player  deriving Show
data Wool    = Wool    deriving Show
data Sweater = Sweater deriving Show
data Item a = Item
  { _material :: a
  , _amount :: Int
  } deriving Show
makeLenses ''Item



data Payload =
  Payload { _weightKilos :: Int
          , _cargo :: String
          } deriving (Show)
makeLenses ''Payload

data Ship = Ship { _payload :: Payload
                 } deriving (Show)
makeLenses ''Ship

serenity = Ship (Payload 50000 "Livestock")

-- Vocabulary
--  ^  view
--  .  focus
--  %  function
--  ~  update, modify
--  <  get copy of new
--  << get copy of old
data Gate =
  Gate { _open :: Bool
       , _oilTemp :: Float
       } deriving Show
makeLenses ''Gate
data Army =
  Army { _archers :: Int
       , _knights :: Int
       } deriving Show
makeLenses ''Army
-- data Kingdom =
--   Kingdom { _name :: String
--           , _army :: Army , _gate :: Gate } deriving Show
-- makeLenses ''Kingdom
-- duloc :: Kingdom
-- duloc = Kingdom { _name = "Duloc"
--                 , _army = Army { _archers = 22
--                                , _knights = 14
--                                }
--                 , _gate = Gate { _open = True
--                                , _oilTemp = 10.0 }
--                 }


data Role
    = Gunner
    | PowderMonkey
    | Navigator
    | Captain
    | FirstMate
  deriving (Show, Eq, Ord)
-- data CrewMember =
--   CrewMember { _name :: String
--              , _role :: Role
--              , _talents :: [String] }
--   deriving (Show, Eq, Ord)
-- makeLenses ''CrewMember
-- roster :: S.Set CrewMember
-- roster = S.fromList
--   [ CrewMember "Grumpy Roger" Gunner ["J", "A"]
--   , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
--   , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"]
--   , CrewMember "One-eyed Jack" Navigator []
--   ]

-- rosterRoles :: Fold (S.Set CrewMember) Role
-- rosterRoles = folded.role

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]


quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]


-- data Actor =
--   Actor { _name :: String
--         , _birthYear :: Int
--         } deriving (Show, Eq)
-- makeLenses ''Actor

-- data TVShow =
--   TVShow { _title :: String
--          , _numEpisodes :: Int
--          , _numSeasons :: Int
--          , _criticScore :: Double , _actors :: [Actor] }
--             deriving (Show, Eq)
-- makeLenses ''TVShow
-- howIMetYourMother :: TVShow
-- howIMetYourMother = TVShow
--   { _title = "How I Met Your Mother" , _numEpisodes = 208
--   , _numSeasons = 9
--   , _criticScore = 83
--   , _actors =
--     [ Actor "Josh Radnor" 1974
--     , Actor "Cobie Smulders" 1982
--     , Actor "Neil Patrick Harris" 1973 , Actor "Alyson Hannigan" 1974
--     , Actor "Jason Segel" 1980
--     ]
--   }

-- buffy :: TVShow
-- buffy = TVShow
--   { _title = "Buffy the Vampire Slayer" , _numEpisodes = 144
--   , _numSeasons = 7
--   , _criticScore = 81
--   , _actors =
--     [ Actor "Sarah Michelle Gellar" 1977 , Actor "Alyson Hannigan" 1974
--     , Actor "Nicholas Brendon" 1971
--     , Actor "David Boreanaz" 1969
--     , Actor "Anthony Head" 1954
--     ]
--   }
-- tvShows :: [TVShow]
-- tvShows = [ howIMetYourMother
--           , buffy ]

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing (view l)
-- calcAge :: Actor -> Int
-- calcAge actor = 2021 - actor^.birthYear
-- showActor :: Actor -> String
-- showActor actor = actor^.name <> ": " <> show (calcAge actor)

-- effect = traverseOf_ (folded.actors.folded.to showActor) putStrLn tvShows

-- ageSummary :: Actor -> (Sum Int, Sum Int)
-- ageSummary actor = (Sum 1, Sum (calcAge actor))

countVowels :: String -> Int
countVowels str = length $ filter (`elem` ("aeiou"::String)) str

sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

trimmingWhile :: (a->Bool) -> Fold s a -> Fold s a
trimmingWhile pred fold = backwards (droppingWhile pred (backwards (droppingWhile pred fold)))

-- data Card = Card
--   { _name :: String
--   , _aura :: Aura
--   , _holo :: Bool
--   , _moves :: [Move]
--   } deriving (Show, Eq)
-- data Aura = Wet | Hot
--   | Spark
--   | Leafy
--   deriving (Show, Eq)
-- data Move = Move { _moveName :: String
--                  , _movePower :: Int } deriving (Show, Eq)
-- makeLenses ''Card
-- makeLenses ''Move

-- deck :: [Card]
-- deck = [ Card "Skwortul" Wet False [Move "Squirt" 20]
--        , Card "Scorchander" Hot False [Move "Scorch" 20]
--        , Card "Seedasaur" Leafy False [Move "Allergize" 20]
--        , Card "Kapichu" Spark False [Move "Poke" 10 ,
--                                      Move "Zap" 30]
--        , Card "Elecdude" Spark False [Move "Asplode" 50]
--        , Card "Garydose" Wet True [Move "Gary's move" 40]
--        , Card "Moisteon" Wet False [Move "Soggy" 3]
--        , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
--        , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
--        , Card "Sparkeon" Spark True [Move "Shock" 40 ,
--                                      Move "Battery" 50]]
powerLevels = M.fromList
      [ ("Gohan", 710)
      , ("Goku", 9001)
      , ("Krillin", 5000)
      , ("Piccolo", 408)
      ]

opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]

dinos = ("T-Rex", (42, "Stegosaurus"))

-- input :: ((Bool, String), (Bool, String), (Bool, String))
-- input = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))

validateEmail :: String -> Validation [String] String
validateEmail email | elem '@' email = Success email
                    | otherwise      = Failure ["missing '@': " <> email]

goodEmail = [ ("Mike", "mike@tmnt.io")
            , ("Raph", "raph@tmnt.io")
            , ("Don", "don@tmnt.io")
            , ("Leo", "leo@tmnt.io")
            ]
badEmail = [ ("Mike", "mike@tmnt.io")
           , ("Raph", "raph.io")
           , ("Don", "don@tmnt.io")
           , ("Leo", "tmnt.io")
           ]


data User =
  User { _name :: String
       , _age :: Int
       } deriving Show
makeLenses ''User
data Account =
  Account { _actid :: String
          , _user :: User
          } deriving Show
makeLenses ''Account
validateAge :: Account -> Either String Account
validateAge = traverseOf (user.age) (checkAge)
  where checkAge x | x < 0 || x > 150 = Left ("Invalid age: " <> show x)
                   | otherwise = Right x

-- values :: Traversal [a] [b] a b
values :: Applicative f => (a -> f b) -> ([a] -> f [b])
values _ [] = pure []
values handler (a:as) = liftA2 (:) (handler a) (values handler as)

data Transaction = Withdrawal { _amt :: Int }
                 | Deposit { _amt :: Int}
                 deriving Show
makeLenses ''Transaction

newtype BankAccount = BankAccount
                      { _transactions :: [Transaction]
                      } deriving Show
makeLenses ''BankAccount

aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10, Deposit 10212]

-- deposits :: Traversal' [Transaction] Int
-- deposits :: Traversal [Transaction] [Transaction] Int Int
deposits :: Applicative f
         => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits _ [] = pure []
-- if Withdrawal are dropped, .~ won't work correctly.
deposits f (Withdrawal a:as) = liftA2 (:) (pure (Withdrawal a)) (deposits f as)
deposits f (Deposit a:as) = liftA2 (:) (Deposit <$> (f a)) (deposits f as)

-- amountT :: Traversal' Transaction Int
-- amountT :: Traversal Transaction Transaction Int Int
amountT :: Applicative f
        => (Int -> f Int) -> Transaction -> f Transaction
amountT f (Withdrawal a) = Withdrawal <$> f a
amountT f (Deposit a) = Deposit <$> f a

-- both :: Traversal (a,a) (b,b) a b
myBoth :: Applicative f
     => (a -> f b)
     -> (a,a) -> f (b,b)
myBoth f (x,y) = liftA2 (,) (f x) (f y)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
-- transactionDelta :: Applicative f
--                  => (Int -> f Int)
--                  -> Transaction -> f Transaction

myleft :: Traversal (Either a b) (Either a' b) a a'
myleft handler (Left a) = Left <$> handler a
myleft handler (Right b) = pure (Right b)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
mybeside :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
mybeside left right f (lhs,rhs) = (,) <$> (traverseOf left f lhs) <*> (traverseOf right f rhs)

-- Traversal laws
-- 1. x & myTraversal %%~ pure == pure x
-- 2. Consistent focuses: x & traversal %~ f
--                          & traversal %~ g
--                    === x & traversal %~ (g.f)
--    `filtered` breaks this law
--
-- `worded` breaks 1st law: "a   b" & worded %%~ pure != pure "a  b"

tree :: Tree Int
tree = Node 1 [ Node 2 [Node 4 []]
              , Node 3 [Node 5 [], Node 6 []]]

benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]

primes = S.fromList (filterPrimes [2..])
  where filterPrimes (x:xs) = x : filterPrimes [y | y <- xs, y `rem` x /= 0]


heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]

newtype Cycled a = Cycled [a]
  deriving Show

xs = Cycled ['a', 'b', 'c']

type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  ix :: Int -> Traversal' (Cycled a) a
  ix i handler (Cycled xs) = Cycled <$> (traverseOf (ix (i `mod` length xs)) handler xs)

data Address = Address
  { _buildingNumber :: Maybe String
  , _streetName :: Maybe String
  , _apartmentNumber :: Maybe String
  , _postalCode :: Maybe String
  } deriving Show
makeLenses ''Address

type instance Index Address = AddressPiece
type instance IxValue Address = String

data AddressPiece = BuildingNumber
                  | StreetName
                  | ApartmentNumber
                  | PostalCode
                  deriving Show

instance Ixed Address

instance At Address where
  at BuildingNumber = buildingNumber
  at StreetName = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

addr = Address Nothing Nothing Nothing Nothing

sherlockAddr = addr & at StreetName ?~ "Baker St."
                    & at ApartmentNumber ?~ "221B"


newtype MyMap a = MyMap (M.Map String a)

type instance Index (MyMap a) = String
type instance IxValue (MyMap a) = a

instance Ixed (MyMap a) where
  ix :: Applicative f => String -> (a -> f a) -> MyMap a -> f (MyMap a)
  ix k handler (MyMap m) = MyMap <$> ix (map toLower k) handler m

instance At (MyMap a) where
  at k handler (MyMap m) = MyMap <$> at (map toLower k) handler m

emptyMyMap :: MyMap Int
emptyMyMap = MyMap (M.fromList [])
map1 = emptyMyMap & at "hEllo" ?~ 1
                  & at "Hello" ?~ 2

data MyEither l r =
    MyLeft l
  | MyRight r

type Path = [String]
type Body = String
data Request = Post Path Body
             | Get Path
             | Delete Path
             deriving Show
makePrisms ''Request
post = Post ["ksqsf", "blog"] "create"

_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where embed x = Just x
        match (Just x) = Right x
        match Nothing = Left Nothing

_Nothing' :: Prism (Maybe a) (Maybe a) () ()
_Nothing' = prism' embed match
  where embed _ = Nothing
        match (Just _) = Nothing
        match Nothing = Just ()

_Prefix :: String -> Prism' String String
_Prefix prefix = prism' embed match
  where embed = (prefix <>)
        match = stripPrefix prefix

_Factor :: Int -> Prism' Int Int
_Factor n = prism' embed match
  where embed = (*n)
        match m = if r == 0
                  then Just q
                  else Nothing
          where (q,r) = m `quotRem` n

prismFizzBuzz :: Int -> String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n = "Fizz"
  | has (_Factor 5) n = "Buzz"
  | otherwise = show n

runFizzBuzz :: IO ()
runFizzBuzz = forM_ [1..20] $ \n -> putStrLn (prismFizzBuzz n)

_Cycles :: Eq a => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where embed str = take (n*length str) (concat (repeat str))
        match lst = if all (==True) (map (uncurry(==)) (window2 (groupSize (length lst `quot` 3) lst)))
                    then Just (take (length lst `quot` 3) lst)
                    else Nothing

groupSize :: Int -> [a] -> [[a]]
groupSize n xs = go [] xs
  where go res [] = res
        go res xs = go ((take n xs):res) (drop n xs)
window2 :: [a] -> [(a,a)]
window2 [x,y] = [(x,y)]
window2 (x:y:xs) = (x,y):(window2 (y:xs))

prefixes :: String -> [String]
prefixes str = [take n str | n <- [1..]]

cycles :: String -> [String]
cycles pat = [concat (take n (repeat pat)) | n <- [2..]]

possible :: String -> [String]
possible str = interleave [cycles prefix | prefix <- prefixes str]

interleave :: [[a]] -> [a]
interleave xss = map head xss <> interleave (map tail xss)

iscycling :: String -> Bool
iscycling str = str `elem` (takeWhile (\c -> length c < length str) $ possible str)


safeTail :: [a] -> [a]
safeTail = tail & outside _Empty .~ const []

packed :: Iso' String T.Text
packed = iso to' from'
  where to' = T.pack
        from' = T.unpack
