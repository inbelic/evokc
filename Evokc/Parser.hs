module Parser where

import AST
import Shunting ( shuntingYard )

import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map.Strict as Map (fromList)
import Text.Parsec

-- // General helpers //

-- attempts to transform the parsed data in place and allows for a specific
-- error message from construction
build :: (Stream s m Char)
      => String
      -> (a -> Maybe b)
      -> ParsecT s u m a
      -> ParsecT s u m b
build s f p = do
  x <- p
  case f x of
    Nothing -> unexpected s
    (Just x') -> return x'

-- only allows space characters instead of all whitespace
spacesP :: (Stream s m Char) => ParsecT s u m String
spacesP = many (char ' ')

-- Parse an identifier of the form [A-Z][a-zA-Z]*
enumIdentP :: (Stream s m Char) => ParsecT s u m EnumIdent
enumIdentP = EnumIdent <$> (lookAhead upper *> many letter)

-- Parse an identifier of the form [A-Z][a-zA-Z]*
fieldIdentP :: (Stream s m Char) => ParsecT s u m FieldIdent
fieldIdentP = FieldIdent <$> (lookAhead upper *> many letter)

-- Parse an identifier of the form [a-z][a-zA-Z]*
varIdentP :: (Stream s m Char) => ParsecT s u m VarIdent
varIdentP = VarIdent <$> (lookAhead lower *> many letter)

-- Parse a value: [0-9]+ | -[0-9]+ | true | false | EnumIdent
valueP :: (Stream s m Char) => ParsecT s u m Value
valueP = try (intP <|> boolP <|> enumP)
  where
    -- // Integer parser components //
    intP :: (Stream s m Char) => ParsecT s u m Value
    intP = Int <$> (multP <*> positiveNatural) <?> "signed/unsigned int"

    multP :: (Stream s m Char) => ParsecT s u m (Int -> Int)
    multP = (pure negate <* char '-') <|> pure id

    positiveNatural :: (Stream s m Char) => ParsecT s u m Int
    positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

    -- // Boolean parser components //
    boolP = Bool <$> (trueP <|> falseP)
    trueP = const True <$> string "true"
    falseP = const False <$> string "false"

    -- // Enum parser //
    enumP = Enum <$> fieldIdentP <*> (string "::" *> enumIdentP)

-- Parse a single base expression corresponding to one of the adts of Expr and
-- do not recurse unto any child-exprs
singleExprP :: (Stream s m Char) => ParsecT s u m Expr
singleExprP = valueExprP <|> fieldExprP <|> varExprP <|> binOpP <|> parenP
  where
    -- // Basic element parsers //
    fieldExprP :: (Stream s m Char) => ParsecT s u m Expr
    fieldExprP = FieldExpr <$> fieldIdentP

    -- valueExprP must be tried before fieldExprP as they both can consume a
    -- fieldIdentP, use try to ensure that the fieldIdent is not consumed
    valueExprP :: (Stream s m Char) => ParsecT s u m Expr
    valueExprP = ValueExpr <$> try valueP

    varExprP :: (Stream s m Char) => ParsecT s u m Expr
    varExprP = VarExpr <$> varIdentP

    -- // Operation parsers //
    opP :: (Stream s m Char) => (String, BinOp) -> ParsecT s u m Expr
    opP (str, binOp)
      = BinExpr UndefExpr
      <$> (const binOp <$> try (string str))
      <*> pure UndefExpr

    binOpP :: (Stream s m Char) => ParsecT s u m Expr
    binOpP = choice . map opP $
      [ ("?", CndOp IfOp)
      , (":", CndOp ElseOp)
      -- Conditional ops
      , ("==", CmpOp EqOp)
      , ("!=", CmpOp NeOp)
      , ("<", CmpOp LtOp)
      , ("<=", CmpOp LeOp)
      , (">", CmpOp GtOp)
      , (">=", CmpOp GeOp)
      -- Comparison ops
      , ("|", LogOp OrOp)
      , ("&", LogOp AndOp)
      -- Logical ops
      , ("+", NumOp AddOp)
      , ("-", NumOp SubOp)
      , ("*", NumOp MulOp)
      , ("/", NumOp DivOp)
      -- Numerical ops
      ]

    -- // Parenthesis parser //
    parenP :: (Stream s m Char) => ParsecT s u m Expr
    parenP = ParenExpr <$> (char '(' *> spacesP *> exprP True)

-- Parse a list of single base expressions
--  If we are within a parenthesis then we can ignore newlines
--  Othewise, we will stop at a newline
type InParens = Bool
exprsP :: (Stream s m Char) => InParens -> ParsecT s u m [Expr]
exprsP True = sepEndBy1 singleExprP spaces <* char ')'
exprsP False = sepEndBy1 singleExprP spacesP <* (void newline <|> eof)

-- Parse an expression tree
--  First parse the individual expr statements into a list
--  Then sort these using the Shunting Yard algorithm into postfix notation
--  Finally, construct the expr tree by filling the BinExpr with their operands
--  and removing parenthesis from the expr tree
exprP :: (Stream s m Char) => InParens -> ParsecT s u m Expr
exprP inParens
  = build "error when constructing expr tree"
  (constructExpr [] . shuntingYard) $ exprsP inParens
    where
      constructExpr :: [Expr] -> [Expr] -> Maybe Expr
      constructExpr [expr] []
        = Just expr
      constructExpr acc []
        = Nothing -- not all expr's where used
      constructExpr (y:x:acc) ((BinExpr UndefExpr op UndefExpr):xs)
        = constructExpr (BinExpr x op y : acc) xs
      constructExpr acc ((ParenExpr x) : xs)
        = constructExpr (x : acc) xs -- extract paren expr
      constructExpr acc (UndefExpr : xs)
        = Nothing -- incorrect parse
      constructExpr acc (x:xs)
        = constructExpr (x : acc) xs -- otherwise put on the stack to consume
