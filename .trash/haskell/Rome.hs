module Rome where

romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAmount romeNotation

subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a,_) -> a > n) pair)

convert :: Int -> String
convert 0 = ""
convert n = let (rome, m) = subtrahend n
            in m ++ convert (n-rome)


toBinary' :: Int -> String
toBinary' 0 = ""
toBinary' n | n < 0 = error "negative"
            | n `mod` 2 == 1 = toBinary (n `div` 2) ++ "1"
            | n `mod` 2 == 0 = toBinary (n `div` 2) ++ "0"

toBinary :: Int -> String
toBinary n | n == 0 = "0"
           | otherwise = toBinary' n

