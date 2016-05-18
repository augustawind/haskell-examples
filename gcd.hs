import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      let remainder = a `mod` b
      tell [show a ++ " mod " ++ show b ++ " = " ++ show remainder]
      gcd' b remainder
