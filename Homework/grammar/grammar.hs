==type Program = [Statement] -- a program is a list of statements

data Identifier = String

data Statement
  = Assignment Identifier Expression
  | If Condition Statement Statement
  | While Condition Statement
  | Msg String
  | Print Expression
  | Println Expression
  | Read Identifier
  | Skip
  | Block Program


-- This is not correct
data Condition
  = BoolConst Bool
  | Not Condition
  | BoolBinOp Condition Condition
  | Test Comparison Expression Expression

data BoolBinOp
  = And
  | Or

data Comparison
  = Greater
  | GreaterEq
  | Less
  | LessEqual
  | Equal
  | NEqual

data Expression
  = Term
  | AddOp Expression Term

data Term
  = Factor
  | MultOp Term Factor

data Factor
  = Var Identifier
  | IntConst Integer
  | Expression

data UnOp = Neg

data AddOp
  = Plus
  | Minus

data MultOp
  = Times
  | Divide
  | Mod

{--

x = 1;
while x > 0 {
  println x;
  x = x - 1
}

--}

-- example :: Program
-- example = [ Assignment "x" (IntConst 10),
--             While (Test (Greater (Var "x") (IntConst 0))
--               (Block
--                 [ Println (Var "x"),
--                   Assignment "x" (BinExpression Minus (Var "x") (IntConst 1))
--                 ]
--               )
--           ]