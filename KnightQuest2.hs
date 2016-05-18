import Control.Monad (guard)
import Control.Monad.Trans.Writer

type KnightPos = (Int, Int)

-- Using do notation.
moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) = do
    (x', y') <- [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1)
                ,(x+1, y-2), (x+1, y+2), (x-1, y-2), (x-1, y+2)]
    guard (x' `elem` [1..8] && y' `elem` [1..8])
    return (x', y')

-- Using bind (>>=).
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (x, y) = moves >>= \(x', y') ->
                     guard (x' `elem` [1..8] && y' `elem` [1..8]) >>
                     return (x', y')
    where moves = [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1)
                  ,(x+1, y-2), (x+1, y+2), (x-1, y-2), (x-1, y+2)]

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
