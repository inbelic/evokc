module Main where

import AST
import Typer
import Parser

import qualified Data.Map.Strict as Map (fromList)
import Text.Parsec (eof, spaces, runParser)

main :: IO ()
main = do
    let p = fieldDefP <* spaces <* eof
        t = fieldDefT
        ts = TypeState
            (Map.fromList [])
            (Map.fromList [(FieldIdent "Evok", EnumType $ map EnumIdent ["One", "Two"])])
            (Map.fromList [])
    input <- readFile "tests/syntax.ev"
    case runParser p () "" $ input of
      Left err -> print err
      Right x -> print $ runTyper t ts x
