import Data.Char
main :: IO ()
main = do
    allFortunes <- readFile "fortunes.txt"
    let fortunes = lines allFortunes
    putStr "Please enter your name: "
    name <- getLine
    putStrLn $ "Hello " ++ name ++ ", here is your fortune."
    putStrLn $ "\t" ++ (getFortune name fortunes)
    main

getFortune :: String -> [String] -> String
getFortune name list = 
    let index = sum [ord c | c <- name] `mod` length list
    in list!!index