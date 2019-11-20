import Data.Char
import System.Environment
import System.Console.GetOpt
import Text.Read

data Flag = Help | Name String | All | Count String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['n'] ["name"] (ReqArg Name "<name>") "Define the user's name."
          , Option [] ["all","everything"] (NoArg All) "Print every fortune. On your own head be it."
          , Option ['k','c'] ["count"] (ReqArg Count "<k>") "Print out <k> fortunes."
          ]

main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    putStrLn $ show (flags, inputs, errors)
    if Help `elem` flags || not (null errors) || length inputs > 1
    then do mapM putStr errors
            putStrLn $ usageInfo "Usage: fortunes [options] [file]" options
    else do let filename = if null inputs then "fortunes.txt" else  head inputs
            allFortunes <- readFile filename
            let fortunes = lines allFortunes
            if All `elem` flags
            then giveAllFortunes fortunes
            else do name <- getName flags
                    case getCount flags of 
                      Nothing -> giveFortune fortunes name
                      Just c -> giveCountFortunes fortunes name c

getName :: [Flag] -> IO String
getName (Name s:_) = return s
getName (_:flags) = getName flags
getName [] =  prompt "Please enter your name: "

getCount :: [Flag] -> Maybe Int
getCount (Count s:_) = readMaybe s
getCount (_:flags) = getCount flags
getCount [] =  Nothing

giveAllFortunes :: [String] -> IO ()
giveAllFortunes fortunes =
  do sequence $ map putStrLn fortunes
     return ()

giveFortune :: [String] -> String -> IO ()
giveFortune fortunes name = aux 0
  where aux i = do putStrLn $ "Hello " ++ name ++ ", here is your fortune."
                   putStrLn $ "\t" ++ (getFortune name fortunes i)
                   response <- prompt "Would you like another fortune? "
                   if map toLower response `elem` ["y","yes","sure","please","ok"]
                   then aux (i+1)
                   else return ()

giveCountFortunes :: [String] -> String -> Int -> IO ()
giveCountFortunes fortunes name count = 
      do putStrLn $ "Hello " ++ name ++ ", here is your fortune."
         let cFortunes = getCountFortunes name fortunes count
         sequence $ map (\s -> putStrLn $ "\t"++s) cFortunes
         return () 

prompt :: String -> IO String
prompt message = 
  do putStr message
     getLine

getFortune :: String -> [String] -> Int -> String
getFortune name list i= 
    let index = sum [ord c | c <- name] `mod` length list
    in list!!(index+i)

getCountFortunes :: String -> [String] -> Int -> [String]
getCountFortunes name list count = 
    let index = sum [ord c | c <- name] `mod` length list
    in take count $ drop (index-1) list
    
