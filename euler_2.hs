main = print problem_2
problem_2 = list

list = listsum [x | x <- takeWhile (<= 4000000) fibs, even x]

-- List of all Fibonacci numbers
fibs = (map fib [1..])

-- Fibonacci Function
fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)

-- Sum up a list
listsum :: (Num a) => [a] -> a
listsum [] = 0
listsum (x:someList) = x + listsum someList
-- listsum (a:b:[]) = a + b
-- listsum x = head x + listsum (tail x)
