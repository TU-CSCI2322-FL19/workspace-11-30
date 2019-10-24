import Data.Char
import Debug.Trace

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

--do not use this refutable pattern or fromJust outside of a class activity. Ever.

data AST = Leaf Integer | Node Op AST AST deriving (Show, Eq)

input = "+ 7 / 232 * 3 4"
input2 = "+ * 2 3 * 4 2"
inputWords = words input
(Just tokens) = sequence $ map lexer inputWords
goalAST = Node Plus (Leaf 7) (Node Div (Leaf 232) (Node Mult (Leaf 3) (Leaf 4)))

--Step 2
dangerParse :: [Token] -> AST
dangerParse tokens = 
  let (tree, afterTree) = aux tokens
  in if null afterTree
     then tree
     else error "Invalid Parse: Too Many Tokens"

aux :: [Token] -> (AST, [Token])
aux [] = error "Invalid Parse: Too Few Tokens"
aux (NumT x:ts) = (Leaf x, ts)
aux (OperT o:ts) = 
      let (lftTree, afterLft) =  aux ts
          (rgtTree, afterRgt) =  aux afterLft
      in (Node o lftTree rgtTree, afterRgt)

--test on input and input2


--Step 1
eval :: AST -> Integer
eval (Leaf x) = x
eval (Node Plus lft rgt) = eval lft + eval rgt
eval (Node Mult lft rgt) = eval lft * eval rgt
eval (Node Minus lft rgt) = eval lft - eval rgt
eval (Node Div lft rgt) = eval lft `div` eval rgt

eval2 :: AST -> Integer
eval2 (Leaf x) = x
eval2 (Node op lft rgt) = (evalOp op) (eval2 lft) (eval2 rgt)

evalOp :: Op -> Integer -> Integer -> Integer
evalOp Plus = (+)
evalOp Mult = (*)
evalOp Div = div 
evalOp Minus = (-)

--step 0
isOp :: Token -> Bool
isOp (NumT x) = False
isOp (OperT op) = True

countOps :: AST -> Integer
countOps (Leaf x) = 0
countOps (Node op lft rgt) = 1 + countOps lft + countOps rgt

