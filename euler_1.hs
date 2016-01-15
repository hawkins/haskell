main = print problem_1
problem_1 = sum [x | x <- [1..999], x // 3 == 0 || x // 5 == 0] where
    (//) a b = if b > a
        then a
        else (a - b) // b
