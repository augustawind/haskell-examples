import Control.Monad
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

multWithLog' = (*) `liftM` logNumber 3 `ap` logNumber 5

multWithLog'' = do 
    result <- liftM2 (*) (logNumber 3) (logNumber 5)
    tell ["Gonna multiply these two"]
    return result
