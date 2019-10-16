input = "+ 7 - 232 * 3 4"
inputWords = words input

-- data Token = PlusT | MinusT | MultT | DivT | NumT Integer deriving (Eq, Show)

data Token = NumT Integer | OperT Op deriving (Eq, Show)
data Op = Plus | Minus | Mult | Div deriving (Eq, Show)
tokenGoal = [OperT Plus, NumT 7, OperT Minus, NumT 232, OperT Mult, NumT 3, NumT 4]

-- step 1
dangerLex :: String -> Token

--step 2
lexer :: String -> Maybe Token
