fib :: [Integer]
fib = 0:1:zipWith (+) fib (drop 1 fib)

isPrime :: [Bool]
isPrime = False:False:True:
