main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hi " ++ name ++ ", you seem pretty cool!"