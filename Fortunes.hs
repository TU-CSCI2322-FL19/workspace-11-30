import Data.Char
--Problems 0-2 will be done as a class.
--  0) Stop reading in the file every time.
--  1) Make an action to prompt the user and get a response.
--
--Core Exercise: Problems 2-4
--  2) Ask their name once. Remember it and print it out each time. Ask for a number for the
--  fortune.
--  3) Make an action getBool :: IO Bool to get a Yes/No response. 
--  Accept yes/y no/n in any captilization. If they input anything else, ask again.
--  4) Stop asking for a number: just give them the next fortune in the list.

--Extra Fun Problems: Problems 5-7
--  5) Feature creep! Ask the user what they want, and support the following operations. 
--  Get each one working before you move on to the next.
--  Hint: Make an "Request" data-type and a parseRequest:: String -> Request function. 
--   a) "Give me a fortune" (or any string with fortune in it)
--      Print out the next fortune
--   b) "Remember _______" 
--      Remember whatever comes after the command, and print it out on every prompt
--   c) "Remind me" 
--      Only print out the string when told to remind them
--  6) Make parseRequest more robust: return a Maybe Request. 
--  7) Add an action "What is _____" that evaluates a prefix mathematical expression and prints it
--     out. You'll need to load Calc.hs.

main :: IO ()
main = do
    allFortunes <- readFile "fortunes.txt"
    let fortunes = lines allFortunes
    putStr "Please enter your name: "
    name <- getLine
    putStrLn $ "Hello " ++ name ++ ", here is your fortune."
    putStrLn $ "\t" ++ (getFortune name fortunes)
    putStr "Would you like another fortune? "
    response <- getLine
    if map toLower response `elem` ["y","yes","sure","please","ok"]
    then main
    else return ()

getFortune :: String -> [String] -> String
getFortune name list = 
    let index = sum [ord c | c <- name] `mod` length list
    in list!!index
