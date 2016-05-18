import Control.Monad (liftM, foldM)

solveRPN :: String -> Maybe Double
solveRPN str = do
    [result] <- foldM f [] (words str)
    return result

f :: [Double] -> String -> Maybe [Double]
f (x:y:xs) "+" = return $ (x + y):xs
f (x:y:xs) "-" = return $ (x - y):xs
f (x:y:xs) "*" = return $ (x * y):xs
f (x:y:xs) "/" = return $ (x / y):xs
f xs number    = liftM (:xs) (readMaybe number)

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
                  [(x, "")] -> Just x
                  _         -> Nothing
