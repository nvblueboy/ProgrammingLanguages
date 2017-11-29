module MoBettaAST where

type Program = [Statement]

data Statement
  = Assign String AExpr
  | Skip
  | Print AExpr
  | Msg String
  | Read String
  | If BExpr Statement Statement
  | While BExpr Statement
  | Block [Statement]
  deriving (Show)

data BExpr
  = Reln Comp AExpr AExpr
  | BoolConst Bool
  | BBin BinBOp BExpr BExpr
  | BUn UnBOp BExpr
  deriving (Show)

data BinBOp = And | Or deriving (Show, Eq)
data UnBOp = Not deriving (Show, Eq)

data Comp
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NEqual
  deriving (Show, Eq)

data AExpr
  = Var String
  | IntConst Integer
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Mod AExpr AExpr
  | Neg AExpr
  deriving (Show)

