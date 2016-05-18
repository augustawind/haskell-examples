import Control.Monad.State
import System.Random

-- Note the standard library type definition of 'random' from System.Random.
-- It follows the format of a Stateful computation.
-- random :: (RandomGen g, Random a) => g -> (a, g)

-- Thus, this works:
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- This function throws three coins.
threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
