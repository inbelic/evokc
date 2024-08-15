module Typer where

import AST

import Control.Applicative
import Data.List (intersect, union)
import qualified Data.Map.Strict as Map (Map, empty, insert, insertWith, lookup)

data TypeState = TypeState
  { fTypes :: Map.Map FieldIdent FieldType
  , vTypes   :: Map.Map VarIdent [FieldType]
  } deriving (Eq, Show)

insertFType :: FieldIdent -> FieldType -> TypeState -> TypeState
insertFType field fType ts = ts { fTypes = Map.insert field fType $ fTypes ts }

insertVTypes :: VarIdent -> [FieldType] -> TypeState -> TypeState
insertVTypes var vTypes' ts
  = ts { vTypes = Map.insertWith union var vTypes' $ vTypes ts }

-- VarIdents are strictly within their ExprBody so allow for the scope to be
-- reset when entering a new body
resetScope :: TypeState -> TypeState
resetScope ts = ts { vTypes = Map.empty }

newtype Typer a = Typer
  { runTyper :: TypeState -> a -> [FieldType]
  }

instance Semigroup (Typer a) where
  (<>) :: Typer a -> Typer a -> Typer a
  (Typer t1) <> (Typer t2) = Typer $ \ts x -> union (t1 ts x) (t2 ts x)

-- // General helpers //

-- We will follow the general pattern of using `imap` to get the values of a
-- particular data constructor (eg. Value -> Enum or Expr -> BinExpr) without
-- including the fall-through to allow for easier typing.
imap :: (a -> b) -> Typer b -> Typer a
imap f (Typer t) = Typer $ \ts x -> t ts $ f x

-- Using the above pattern is clearly unsafe, so we can have a guard that will
-- not try to run the Typer if the de-construction would fail. Then we can have
-- the pattern: guard g . imap (\Constructor x -> x) to allow for partial
-- construction of a Typer that can be combined with <>.
guard :: (a -> Bool) -> Typer a -> Typer a
guard f (Typer t) =
  Typer $ \ts x ->
    case f x of
      True -> t ts x
      False -> []

-- // Value typer //

valueT :: Typer Expr
valueT = guard g . imap (\(ValueExpr val) -> val) $ boolT <> intT <> enumT
  where
    g (ValueExpr _) = True
    g _ = False

    -- Will always be a BoolType if Value is a Bool
    boolT :: Typer Value
    boolT = guard g . Typer $ \ts (Bool _) -> [BoolType]
      where
        g (Bool _) = True
        g _ = False

    -- Return the possible types of integer based on the Int value
    intT :: Typer Value
    intT = guard g . Typer $ \ts (Int x) -> f x
      where
        f x
          | x < -128 = []
          | x < 0 = [IntType I8]
          | x < 128 = [IntType I8, IntType U8]
          | x < 256 = [IntType U8]
          | otherwise = []
        g (Int _) = True
        g _ = False

    -- Ensure that the enum has been defined in a field and retreive the type
    enumT :: Typer Value
    enumT = guard g . Typer $ \ts (Enum field enum) ->
      case Map.lookup field $ fTypes ts of
        Just (EnumType enums) ->
          case elem enum enums of
            True -> [EnumType enums]
            False -> []
        Nothing -> []
      where
        g (Enum _ _) = True
        g _ = False

-- // Various Ident typers //

fieldT :: Typer Expr
fieldT = guard g . imap (\(FieldExpr field) -> field) $ fieldIdentT
  where
    g (FieldExpr _) = True
    g _ = False

    -- Ensure that the Field has been defined and retreive the type
    fieldIdentT :: Typer FieldIdent
    fieldIdentT = Typer $ \ts field ->
      case Map.lookup field $ fTypes ts of
        Just fType -> [fType]
        Nothing -> []

varT :: Typer Expr
varT = guard g . imap (\(VarExpr var) -> var) $ varIdentT
  where
    g (VarExpr _) = True
    g _ = False

    -- Ensure that the Var has been defined and retreive the type
    varIdentT :: Typer VarIdent
    varIdentT = Typer $ \ts var ->
      case Map.lookup var $ vTypes ts of
        Just fTypes -> fTypes
        Nothing -> []

-- // BinExpr typer //

opT :: Typer Expr
opT = guard g . imap (\(BinExpr lhs op rhs) -> (lhs, op, rhs)) $ binOpT
  where
    g (BinExpr _ _ _) = True
    g _ = False

    -- First we will get the types of the sub-expressions. Then we will verify
    -- these types based on the operation type
    binOpT :: Typer (Expr, BinOp, Expr)
    binOpT = Typer $ \ts (lhs, op, rhs) ->
      checkType op (runTyper exprT ts lhs) (runTyper exprT ts rhs)

    checkType :: BinOp -> [FieldType] -> [FieldType] -> [FieldType]
    -- NumOp expects the operands to both be an overlapping IntType and
    -- then will output the possible overlap that they could both be
    checkType (NumOp _) fTypes1 fTypes2
      = case (filter filt inter, filter (not . filt) inter) of
          (fTypes, []) -> fTypes
          (_, _) -> []
      where
        inter = intersect fTypes1 fTypes2
        filt (IntType _) = True
        filt _ = False

    -- LogOp expects both operands to only be a BoolType
    checkType (LogOp _) [BoolType] [BoolType] = [BoolType]
    checkType (LogOp _) _ _ = []

    -- IfOp requires the first operand to be a boolean and will return the
    -- second operand's type
    checkType (CndOp IfOp) [BoolType] fTypes = fTypes
    checkType (CndOp IfOp) _ _ = []
    -- ElseOp requires both operands to have the same type
    checkType (CndOp ElseOp) fTypes1 fTypes2 = intersect fTypes1 fTypes2

    -- EqOp/NeOp requires both operands to have the same type and will output
    -- a BoolType
    checkType (CmpOp EqOp) fTypes1 fTypes2
      = case intersect fTypes1 fTypes2 of
          [] -> []
          _ -> [BoolType]
    checkType (CmpOp NeOp) fTypes1 fTypes2
      = case intersect fTypes1 fTypes2 of
          [] -> []
          _ -> [BoolType]
    -- All other CmpOps requires both operands to have the same IntType, as
    -- implemented in checkType NumOp ...
    checkType (CmpOp _) fTypes1 fTypes2
      = checkType (NumOp AddOp) fTypes1 fTypes2


-- // Expr body typer //

bodyExprT :: Typer Expr
bodyExprT = guard g . imap (\(BodyExpr body) -> body) $ bodyT
  where
    g (BodyExpr _) = True
    g _ = False

    -- We will traverse down the body and accumulate the VarIdent types until we
    -- reach the final ReturnExpr and ensure that all the types align.
    --
    --  Calls resetScope at each new Typer check of a BodyExpr so that we do not
    --  get false positives of VarIdents from outside the scope
    bodyT :: Typer BodyExpr
    bodyT = Typer $ \ts expr -> t (resetScope ts) expr
      where
        t :: TypeState -> BodyExpr -> [FieldType]
        t ts (ReturnExpr expr) = runTyper exprT ts expr
        t ts (NodeExpr var expr next)
          = case runTyper exprT ts expr of
              [] -> []
              fTypes -> t (insertVTypes var fTypes ts) next

-- // Expr typer //

exprT :: Typer Expr
exprT = opT <> valueT <> varT <> fieldT <> bodyExprT
