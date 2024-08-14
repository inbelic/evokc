module Shunting where

class Shuntable a where
  isOp :: a -> Bool -- differentiate between operations and other exprs
  prec :: a -> Int  -- precedence mapping


-- standard implementation of the Shunting Yard Algorithm of a Shuntable type
-- which defines a precedence of the type and whether it is an "op"
type OpStack a = [a]
type Input a = [a]
type Output a = [a]
shuntingYard :: (Shuntable a) => [a] -> [a]
shuntingYard = shuntingYard' [] []
  where
    shuntingYard' :: (Shuntable a) => OpStack a -> Output a -> Input a -> [a]
    shuntingYard' ops acc []
      = reverse  -- we have been prepending instead of appending so reverse
      $ reverse ops ++ acc -- move the remaining ops to the accumulator
    shuntingYard' ops acc (x : xs)
      | not $ isOp x -- not an op so we can just push it onto the accumulator
        = shuntingYard' ops (x : acc) xs
      | otherwise -- pop ops that have a higher precedence onto the accumulator
        = case span (\y -> prec x <= prec y) ops of
            ([], _) -> shuntingYard' (x : ops) acc xs
            (popOps, remOps) ->
              shuntingYard' (x : remOps) (reverse popOps ++ acc) xs
