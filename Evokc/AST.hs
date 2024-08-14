module AST where

import Shunting ( Shuntable(..) )
import Text.Parsec ( SourcePos )
import qualified Data.Map.Strict as Map (Map)

newtype EnumIdent = EnumIdent String deriving (Eq, Show)
newtype FieldIdent = FieldIdent String deriving (Eq, Show)
newtype VarIdent = VarIdent String deriving (Eq, Show)

data Value
  = Bool Bool
  | Int Int
  | Enum FieldIdent EnumIdent
  deriving (Eq, Show)

-- A comparison operation. One that takes two like values and outputs a boolean
data CmpOp = EqOp | NeOp | GtOp | GeOp | LtOp | LeOp deriving (Eq, Show)
-- A conditional flow operation. One that takes in a boolean/value and a value
-- and conditionally returns one of the values
data CndOp = IfOp | ElseOp deriving (Eq, Show)
-- A logical operation. One that takes two booleans and outputs a boolean
data LogOp = AndOp | OrOp deriving (Eq, Show)
-- A 'numerical' operation. One that takes an integer and outputs an integer
data NumOp = AddOp | SubOp | MulOp | DivOp deriving (Eq, Show)

-- A binary operation. A union of the listed operation types
data BinOp = CmpOp CmpOp | CndOp CndOp | LogOp LogOp | NumOp NumOp
  deriving (Eq, Show)

data BodyExpr
  = ReturnExpr Expr
  | NodeExpr VarIdent Expr BodyExpr
  deriving (Eq, Show)

data Expr
  = BinExpr Expr BinOp Expr
  | BodyExpr BodyExpr
  | FieldExpr FieldIdent
  | UndefExpr -- placeholder during parsing
  | ParenExpr Expr
  | ValueExpr Value
  | VarExpr VarIdent
  deriving (Eq, Show)

instance Shuntable Expr where
  isOp (BinExpr _ _ _) = True
  isOp _ = False

  prec (BinExpr _ (CndOp op) _)
    = case op of
        ElseOp -> 1
        IfOp -> 2
  prec (BinExpr _ (LogOp _) _) = 3
  prec (BinExpr _ (CmpOp _) _) = 4
  prec (BinExpr _ (NumOp op) _)
    = case op of
      AddOp -> 5
      SubOp -> 5
      MulOp -> 6
      DivOp -> 6
  prec _ = 0
