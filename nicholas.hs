main = hello

hello :: IO()
hello = do
    putStrLn "Is your name Nicholas?"
    answer <- getLine
    if answer == "no"
    then putStrLn "Good because Josh is better."
    else areYouSure

areYouSure :: IO()
areYouSure = do
    putStrLn "Are you sure?"
    answer <- getLine
    if answer == "no"
        then putStrLn "Good because Josh is better."
        else areYouSure
