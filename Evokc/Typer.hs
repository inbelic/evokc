module Typer where

import AST
import TyperError

import Control.Applicative
import Data.List (intersect, singleton, union)
import qualified Data.Map.Strict as Map
  ( Map, empty, insert, insertWith, intersection, fromList, lookup
  )

-- // Define a TypeState //

data TypeState = TypeState
  { cExprs :: Map.Map Param Expr
  , fTypes :: Map.Map FieldIdent FieldType
  , vTypes :: Map.Map VarIdent [FieldType]
  } deriving (Eq, Show)

insertCExpr :: Param -> Expr -> TypeState -> TypeState
insertCExpr ident expr@(ClosureExpr _ _) ts
  = ts { cExprs = Map.insert ident expr $ cExprs ts }
insertCExpr _ _ ts = ts

insertFType :: FieldIdent -> FieldType -> TypeState -> TypeState
insertFType field fType ts = ts { fTypes = Map.insert field fType $ fTypes ts }

insertVTypes :: VarIdent -> [FieldType] -> TypeState -> TypeState
insertVTypes var vTypes' ts
  = ts { vTypes = Map.insertWith union var vTypes' $ vTypes ts }

-- Non-parameter VarIdents are strictly within their ExprBody, so allow for
-- the scope to be reset when entering a new body with parameters set to be
-- any type
closureScope :: [VarIdent] -> TypeState -> TypeState
closureScope params ts = ts
  { vTypes = Map.fromList . map (, anyType) $ params
  }

-- When we look at a Call we want to ensure the Call parameters will still pass
-- the type check
setScope :: [(VarIdent, [FieldType])] -> TypeState -> TypeState
setScope typedIdents ts = ts { vTypes = Map.fromList typedIdents }

-- // Define a Typer //

newtype Typer a = Typer
  { runTyper :: TypeState -> a -> Either TyperError [FieldType]
  }

(<+>) :: Typer a -> Typer a -> Typer a
(Typer ta) <+> (Typer tb)
  = Typer $ \ts x ->
    let aRes = ta ts x
        bRes = tb ts x
     in case (aRes, bRes) of
          (Right aTypes, Right bTypes) -> Right $ union aTypes bTypes
          (Right aTypes, _) -> Right aTypes
          (_, Right bTypes) -> Right bTypes
          (Left aErr, Left bErr) -> Left $ aErr <> bErr

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
      False -> Left GuardErr

-- // Value typer //

valueT :: Typer Expr
valueT = guard g . imap (\(ValueExpr val) -> val) $ boolT <+> intT <+> enumT
  where
    g (ValueExpr _) = True
    g _ = False

    -- Will always be a BoolType if Value is a Bool
    boolT :: Typer Value
    boolT = guard g . Typer $ \ts (Bool _) -> Right [BoolType]
      where
        g (Bool _) = True
        g _ = False

    -- Return the possible types of integer based on the Int value
    intT :: Typer Value
    intT = guard g . Typer $ \ts (Int x) -> f x
      where
        f x
          | x < -128 = Left $ IntOverflowErr x
          | x < 0 = Right [IntType I8]
          | x < 128 = Right [IntType I8, IntType U8]
          | x < 256 = Right [IntType U8]
          | otherwise = Left $ IntOverflowErr x
        g (Int _) = True
        g _ = False

    -- Ensure that the enum has been defined in a field and retreive the type
    enumT :: Typer Value
    enumT = guard g . Typer $ \ts (Enum field enum) ->
      case Map.lookup field $ fTypes ts of
        Just (EnumType enums) ->
          case elem enum enums of
            True -> Right [EnumType enums]
            False -> Left $ EnumUndefErr enum enums
        Just fType -> Left $ FieldTypeMismatch field fType [EnumType [enum]]
        Nothing -> Left $ FieldUndefErr field
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
        Just fType -> Right [fType]
        Nothing -> Left $ FieldUndefErr field

varT :: Typer Expr
varT = guard g . imap (\(VarExpr var) -> var) $ varIdentT
  where
    g (VarExpr _) = True
    g _ = False

    -- Ensure that the Var has been defined and is not a CallExpr then
    -- retreive the type
    varIdentT :: Typer VarIdent
    varIdentT = Typer $ \ts var ->
      case Map.lookup var $ vTypes ts of
        Nothing -> Left $ VarUndefErr var
        Just fTypes ->
          case Map.lookup (VarParam var) $ cExprs ts of
            Just (ClosureExpr idents _) -> Left $ CallParamErr idents []
            Nothing -> Right fTypes

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
      let lhsRes = runTyper exprT ts lhs
          rhsRes = runTyper exprT ts rhs
       in case (lhsRes, rhsRes) of
            (Right lhsTypes, Right rhsTypes) ->
              case checkType op lhsTypes rhsTypes of
                [] -> Left $ OpTypeMismatch op lhsTypes rhsTypes
                fTypes -> Right fTypes
            (Left err, Right _) -> Left err
            (Right _, Left err) -> Left err
            (Left lhsErr, Left rhsErr) -> Left $ lhsErr <> rhsErr

    checkType :: BinOp -> [FieldType] -> [FieldType] -> [FieldType]
    -- NumOp expects the operands to both be an overlapping IntType and
    -- then will output the possible overlap that they could both be
    checkType (NumOp _) fTypes1 fTypes2
      = filter filt . intersect fTypes1 $ fTypes2
      where
        inter = intersect fTypes1 fTypes2
        filt (IntType _) = True
        filt _ = False

    -- LogOp expects both operands to be a BoolType
    checkType (LogOp _) lhsTypes rhsTypes
      = case elem BoolType lhsTypes && elem BoolType rhsTypes of
          True -> [BoolType]
          False -> []

    -- ItrOp expects the first operand to be a boolean and will return the
    -- second operand's type
    checkType (ItrOp _) lhsTypes rhsTypes
      = case elem BoolType lhsTypes of
          True -> rhsTypes
          False -> []

    -- IfOp requires the first operand to be a boolean and will return the
    -- second operand's type
    checkType (CndOp IfOp) lhsTypes rhsTypes
      = case elem BoolType lhsTypes of
          True -> rhsTypes
          False -> []
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
      = case checkType (NumOp AddOp) fTypes1 fTypes2 of
          [] -> []
          _ -> [BoolType]

-- // Call body typer //
callExprT :: Typer Expr
callExprT
  = guard g . imap (\(CallExpr param params) -> (param, params)) $ callT
  where
    g (CallExpr _ _) = True
    g _ = False

    callT :: Typer (Param, [Param])
    callT = Typer $ \ts (ident, params) ->
      case (runTyper paramT ts ident, Map.lookup ident $ cExprs ts) of
        (Left err, _) -> Left err
        (_, Nothing) -> Left $ ParamUndefErr [ident]
        (Right _, Just (ClosureExpr idents bodyExpr)) ->
          case ( length idents == length params
               , traverse (paramLookup ts) $ params
               ) of
            (False, _) -> Left $ CallParamErr idents params
            (_, Nothing) -> Left $ ParamUndefErr params
            (_, Just types) ->
              let typedIdents = zip idents types
               in bodyExprTFun (setScope typedIdents ts) bodyExpr

    paramLookup :: TypeState -> Param -> Maybe [FieldType]
    paramLookup ts (FieldParam field) =
      fmap singleton . Map.lookup field $ fTypes ts
    paramLookup ts (ValParam val)
      = case runTyper valueT ts (ValueExpr val) of
          Left _ -> Nothing
          Right fTypes -> Just fTypes
    paramLookup ts (VarParam var) = Map.lookup var $ vTypes ts

    paramT :: Typer Param
    paramT = Typer $ \ts param ->
       case (paramLookup ts param) of
         Nothing -> Left $ ParamUndefErr [param]
         Just fTypes -> Right fTypes


-- // Expr body typer //

closureExprT :: Typer Expr
closureExprT
  = guard g . imap (\(ClosureExpr params body) -> (params, body)) $ closureT
  where
    g (ClosureExpr _ _) = True
    g _ = False

    -- We will traverse down the body and accumulate the VarIdent types until we
    -- reach the final ReturnExpr and ensure that all the types align.
    --
    --  Calls resetScope at each new Typer check of a BodyExpr so that we do not
    --  get false positives of VarIdents from outside the scope
    closureT :: Typer ([VarIdent], BodyExpr)
    closureT = Typer $ \ts (params, expr) ->
      case bodyExprTFun (closureScope params ts) expr of
        Left err -> Left err
        Right fTypes -> Right $ fTypes

bodyExprTFun :: TypeState -> BodyExpr -> Either TyperError [FieldType]
bodyExprTFun ts (ReturnExpr expr) = runTyper exprT ts expr
bodyExprTFun ts (NodeExpr var expr next)
  = case runTyper exprT ts expr of
      Left err -> Left err
      Right fTypes ->
        bodyExprTFun ( insertCExpr (VarParam var) expr
          . insertVTypes var fTypes
          $ ts) next

-- // Expr typer //

exprT :: Typer Expr
exprT = opT <+> valueT <+> varT <+> fieldT <+> closureExprT <+> callExprT

-- // Statement typer //

statementT :: Typer a -> Typer (Statement a)
statementT (Typer t)
  = Typer $ \ts (Statement _ _ x) -> t ts x

fieldDefT :: Typer (Statement FieldDef)
fieldDefT
  = Typer $ \ts (Statement _ (NameIdent name) (FieldDef fType maybeExpr)) ->
      case maybeExpr of
        Nothing -> Right [fType]
        Just expr ->
          case runTyper exprT ts expr of
            (Left err) -> Left err
            (Right fTypes) ->
              case intersect [fType] fTypes of
                [] -> Left $ FieldTypeMismatch (FieldIdent name) fType fTypes
                _ -> Right [fType]
