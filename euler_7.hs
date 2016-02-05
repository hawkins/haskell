main :: IO()
main = print problem_7

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

problem_7 :: Integer
problem_7 = primes !! 10000

-- Using tools from euler 3

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
