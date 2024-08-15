module TyperError where

import AST

-- // Define a TyperError //

data TyperError
  = GuardErr
  | IntOverflowErr Int
  | EnumUndefErr EnumIdent [EnumIdent]
  | FieldUndefErr FieldIdent
  | VarUndefErr VarIdent
  | FieldTypeMismatch FieldIdent FieldType [FieldType]
  | OpTypeMismatch BinOp [FieldType] [FieldType]
  deriving (Eq)

instance Show TyperError where
  show GuardErr = "guard err"
  show (IntOverflowErr x)
    = "int value overflowed the range [-128, 256): " ++ show x
  show (EnumUndefErr enum enums)
    = "enum value '" ++ show enum ++ "' was not defined in range " ++ show enums
  show (FieldUndefErr field)
    = "the field '" ++ show field ++ "' has not been defined"
  show (VarUndefErr var)
    = "the var '" ++ show var ++ "' has not been defined in local scope"
  show (FieldTypeMismatch field fType fTypes)
    = "the field '" ++ show field ++ "' expects a type of " ++ show fType
    ++ " but got " ++ show fTypes
  show (OpTypeMismatch op lhsTypes rhsTypes)
    = show op ++ " expected " ++ opExpected op ++ " but got lhs types of "
    ++ show lhsTypes ++ " and rhs types of " ++ show rhsTypes
      where
        opExpected :: BinOp -> String
        opExpected (NumOp _) = "the same integer operand types"
        opExpected (CmpOp EqOp) = "the same operand types"
        opExpected (CmpOp NeOp) = "the same operand types"
        opExpected (CmpOp _) = "the same integer operand types"
        opExpected (CndOp IfOp) = "the lhs operand of type bool"
        opExpected (CndOp ElseOp) = "the same operand types"
        opExpected (LogOp _) = "both operands to be boolean typed"

instance Semigroup TyperError where
  ta <> tb
    | typePrec ta < typePrec tb = tb
    | otherwise = ta

typePrec :: TyperError -> Int
typePrec GuardErr = 0
typePrec (IntOverflowErr _) = 1
typePrec (EnumUndefErr _ _) = 2
typePrec (FieldUndefErr _) = 3
typePrec (VarUndefErr _) = 4
typePrec (FieldTypeMismatch _ _ _) = 5
typePrec (OpTypeMismatch _ _ _) = 6
