module Main where

import Parser
import Text.Parsec (eof, spaces, runParser)

main :: IO ()
main = do
    let p = exprP False <* eof
    input <- readFile "tests/syntax.ev"
    case runParser p () "" $ input of
      Left err -> print err
      Right mod -> print $ mod
