import Data.Char
main :: IO ()
main = do putStr "Please enter your name: "
          name <- getLine
          if isLower (head name)
          then do putStrLn "That doesn't sound right. Try again."
                  name <- getLine
                  putStrLn $ "That's better, " ++ name ++ "."
          else putStrLn $ "Hello " ++ name ++ ", have a lovely day."

