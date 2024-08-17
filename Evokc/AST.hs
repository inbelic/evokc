module AST where

import ShuntYard ( Shunt(..) )
import Text.Parsec ( SourcePos )
import qualified Data.Map.Strict as Map (Map)

newtype EnumIdent = EnumIdent String deriving (Eq, Show)
newtype FieldIdent = FieldIdent String deriving (Eq, Ord, Show)
newtype NameIdent = NameIdent String deriving (Eq, Ord, Show)
newtype VarIdent = VarIdent String deriving (Eq, Ord, Show)

-- // Define types //

data Param
  = FieldParam FieldIdent
  | VarParam VarIdent
  deriving (Eq, Ord, Show)

data IntType = I8 | U8
    deriving (Eq, Show)

data FieldType
  = BoolType
  | IntType IntType
  | EnumType [EnumIdent]
  deriving (Show)

instance Eq FieldType where
  BoolType == BoolType = True
  (IntType at) == (IntType bt) = at == bt
  (EnumType []) == (EnumType _) = True
  (EnumType _) == (EnumType []) = True
  (EnumType at) == (EnumType bt) = at == bt
  _ == _ = False

anyType :: [FieldType]
anyType = [BoolType, IntType U8, IntType I8, EnumType []]

-- // Define values //

data Value
  = Bool Bool
  | Int Int
  | Enum FieldIdent EnumIdent
  deriving (Eq, Show)

-- // Define binary operations //

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

-- // Define an expression //

data BodyExpr
  = ReturnExpr Expr
  | NodeExpr VarIdent Expr BodyExpr
  deriving (Eq, Show)

data Expr
  = BinExpr Expr BinOp Expr
  | CallExpr Param [Param]
  | ClosureExpr [VarIdent] BodyExpr
  | FieldExpr FieldIdent
  | ParenExpr Expr
  | UndefExpr -- placeholder during parsing
  | ValueExpr Value
  | VarExpr VarIdent
  deriving (Eq, Show)

instance Shunt Expr where
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

-- // Define a statement //

data Statement a = Statement SourcePos NameIdent a
  deriving (Eq, Show, Functor)

data FieldDef = FieldDef FieldType (Maybe Expr)
  deriving (Eq, Show)
