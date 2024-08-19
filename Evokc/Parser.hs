module Parser where

import AST
import ShuntYard ( shuntingYard )

import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map.Strict as Map (fromList)
import Text.Parsec

-- // General helpers //

-- attempts to transform the parsed data in place and allows for a specific
-- error message from buildion
-- TODO: improve error msg output
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

maybeP :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m (Maybe a)
maybeP p = Just <$> try p <|> pure Nothing

-- only allows space characters instead of all whitespace
spacesP :: (Stream s m Char) => ParsecT s u m String
spacesP = many (char ' ')

-- // Identifier parsers //

-- Parse an identifier of the form [A-Z][a-zA-Z]*
enumIdentP :: (Stream s m Char) => ParsecT s u m EnumIdent
enumIdentP = EnumIdent <$> (lookAhead upper *> many letter)

-- Parse an identifier of the form [A-Z][a-zA-Z]*
fieldIdentP :: (Stream s m Char) => ParsecT s u m FieldIdent
fieldIdentP = FieldIdent <$> (lookAhead upper *> many letter)

-- Parse an identifier of the form [A-Z][a-zA-Z]*
nameIdentP :: (Stream s m Char) => ParsecT s u m NameIdent
nameIdentP = NameIdent <$> (lookAhead upper *> many letter)

-- Parse an identifier of the form [a-z][a-zA-Z]*
varIdentP :: (Stream s m Char) => ParsecT s u m VarIdent
varIdentP = VarIdent <$> (lookAhead lower *> many letter)

-- // Value parser //

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

-- // Expr related parsers //

-- Parse a single base expression corresponding to one of the adts of Expr and
-- do not recurse unto any child-exprs
singleExprP :: (Stream s m Char) => ParsecT s u m Expr
singleExprP
  = callP        -- callP must be tried before field/var as it may consume one
  <|> valueExprP -- valueExprP must be tried before field as it may consume one
  <|> fieldExprP <|> varExprP
  <|> binOpP <|> parenP <|> closureP
  where
    -- // Basic element parsers //
    fieldExprP :: (Stream s m Char) => ParsecT s u m Expr
    fieldExprP = FieldExpr <$> fieldIdentP

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
      , ("@", ItrOp IterateOp)
      -- Iterator ops
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

    -- // Closure call parser //
    callP :: (Stream s m Char) => ParsecT s u m Expr
    callP = try $ CallExpr <$> paramP <*> paramsP

    paramP :: (Stream s m Char) => ParsecT s u m Param
    paramP = (FieldParam <$> fieldIdentP) <|> (VarParam <$> varIdentP)

    paramsP :: (Stream s m Char) => ParsecT s u m [Param]
    paramsP
      = between (char '[') (char ']') (many1 $ paramP <* spacesP)

-- Parse a list of single base expressions
--  If we are within a parenthesis then we can ignore newlines
--  Othewise, we will stop at a newline
type InParens = Bool
exprsP :: (Stream s m Char) => InParens -> ParsecT s u m [Expr]
exprsP True = sepEndBy1 singleExprP spaces <* char ')'
exprsP False = sepEndBy1 singleExprP spacesP <* (void (many1 newline) <|> eof)

-- Parse an expression tree
--  First parse the individual expr statements into a list
--  Then sort these using the Shunting Yard algorithm into postfix notation
--  Finally, build the expr tree by filling the BinExpr with their operands
--  and removing parenthesis from the expr tree
exprP :: (Stream s m Char) => InParens -> ParsecT s u m Expr
exprP inParens
  = build "error when building expr tree"
  (buildExpr [] . shuntingYard) $ exprsP inParens
    where
      buildExpr :: [Expr] -> [Expr] -> Maybe Expr
      buildExpr [expr] []
        = Just expr
      buildExpr acc []
        = Nothing -- not all expr's where used
      buildExpr (y:x:acc) ((BinExpr UndefExpr op UndefExpr):xs)
        = buildExpr (BinExpr x op y : acc) xs
      buildExpr acc ((ParenExpr x) : xs)
        = buildExpr (x : acc) xs -- extract paren expr
      buildExpr acc (UndefExpr : xs)
        = Nothing -- incorrect parse
      buildExpr acc (x:xs)
        = buildExpr (x : acc) xs -- otherwise put on the stack to consume

-- Parse an expr closure
closureP :: (Stream s m Char) => ParsecT s u m Expr
closureP = ClosureExpr <$> try paramsP <*> bodyP
  where
    paramsP :: (Stream s m Char) => ParsecT s u m [VarIdent]
    paramsP
      = between (char '[') (char ']') (many1 $ varIdentP <* spacesP)
      <|> pure []

    bodyP :: (Stream s m Char) => ParsecT s u m BodyExpr
    bodyP = build "error when building epxr body"
      (foldr buildBody Nothing)
      $ between (char '{' <* spaces) (char '}') bodyNodesP

    bodyNodesP :: (Stream s m Char) => ParsecT s u m [(Maybe VarIdent, Expr)]
    bodyNodesP = many1 ((,) <$> maybeP (varIdentP <* string " = ") <*> exprP False <* spacesP)

    buildBody :: (Maybe VarIdent, Expr) -> Maybe BodyExpr -> Maybe BodyExpr
    buildBody (Nothing, expr) Nothing
      = Just $ ReturnExpr expr
    buildBody (Just ident, expr) (Just next)
      = Just $ NodeExpr ident expr next
    buildBody _ _ = Nothing

-- // Statement related parsers //

type Keyword = String
statementP :: (Stream s m Char)
          => Keyword
          -> ParsecT s u m a
          -> ParsecT s u m (Statement a)
statementP keyword p
  = Statement
  <$> getPosition
  <*> (string keyword *> spacesP *> nameIdentP <* spacesP)
  <*> p

-- Parse a field def of the form: `field : ` FieldType (` = ` Expr)?
fieldDefP :: (Stream s m Char) => ParsecT s u m (Statement FieldDef)
fieldDefP = statementP "field" $
  FieldDef <$> (string ": " *> fieldTypeP) <*> maybeValueP
  where
    maybeValueP = Just <$> (string " = " *> exprP False) <|> pure Nothing

-- Parse a field type: u8 | i8 | bool | enum optional(:: EnumIdent+)
fieldTypeP :: (Stream s m Char) => ParsecT s u m FieldType
fieldTypeP = boolTypeP <|> u8TypeP <|> i8TypeP <|> enumTypeP
  where
    boolTypeP = const BoolType <$> string "bool"
    i8TypeP = const (IntType I8) <$> string "i8"
    u8TypeP = const (IntType U8) <$> string "u8"
    enumTypeP = EnumType <$>
      between (string "enum {" *> spaces) (char '}') (endBy1 enumIdentP spaces)
