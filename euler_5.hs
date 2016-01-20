main :: IO()
main = print problem_5

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

problem_5 :: Int
problem_5 = foldr1 lcm [1..20]


-- The following is my previous solution, without knowledge of lcm or foldr / foldr1 functions
-- Note it is dreadfully slow and as such is named problem_5_slow
problem_5_slow :: Int
problem_5_slow = minimum [x | x <- [9699690..], isDivisibleBy x [1..20]]

isDivisibleBy :: Int -> [Int] -> Bool
isDivisibleBy _ [] = True
isDivisibleBy a (b:bs)
        | a `mod` b == 0 = isDivisibleBy a bs
        | otherwise = False
