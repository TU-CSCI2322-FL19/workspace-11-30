import Data.Char
input = "+ 7 / 232 * 3 4"
inputWords = words input

-- data Token = PlusT | MinusT | MultT | DivT | NumT Integer deriving (Eq, Show)

data Token = NumT Integer | OperT Op deriving (Eq, Show)
data Op = Plus | Minus | Mult | Div deriving (Eq, Show)
tokenGoal = [OperT Plus, NumT 7, OperT Div, NumT 232, OperT Mult, NumT 3, NumT 4]

-- step 1
dangerLex :: String -> Token
dangerLex "+" = OperT Plus
dangerLex "-" = OperT Minus
dangerLex "/" = OperT Div
dangerLex "*" = OperT Mult
dangerLex str = NumT $ read str

--step 2
lexer :: String -> Maybe Token
lexer "+" = Just $ OperT Plus
lexer "-" = Just $ OperT Minus
lexer "/" = Just $ OperT Div
lexer "*" = Just $ OperT Mult
lexer ('-':str) = if all isDigit str 
                  then Just $ NumT $ read ('-':str)
                  else Nothing
lexer str = if all isDigit str 
            then Just $ NumT $ read str
            else Nothing

(Just tokens) = sequence $ map lexer inputWords
--do not use this refutable pattern or fromJust outside of a class activity. Ever.

data AST = Leaf Integer | Node Op AST AST deriving (Show, Eq)

goalAST = Node Plus (Leaf 7) (Node Div (Leaf 232) (Node Mult (Leaf 3) (Leaf 4)))

--Step 2
dangerParse :: [Token] -> AST
dangerParse = undefined

--test on input and input2
input2 = "+ * 2 3 * 4 2"


--Step 1
eval :: AST -> Integer
eval = undefined

--step 0
isOp :: Token -> Bool
isOp = undefined
countOps :: AST -> Integer
countOps = undefined
