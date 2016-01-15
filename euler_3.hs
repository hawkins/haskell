main = print(problem_3)

problem_3 = last (findFactorsOf 600851475143)

findFactorsOf :: Integer -> [Integer]
findFactorsOf a = factor a primes
    where
        factor b (p:ps)
            | p * p > b      = [b]
            | b `mod` p == 0 = p : factor (b `div` p) (p:ps)
            | otherwise      = factor b ps

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
