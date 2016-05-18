type Birds = Int
type Pole = (Int, Int)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise                    = Left (leftMsg ++ rightMsg)
  where leftMsg = "Pierre fell with " ++ show (left + n) ++ " birds on the left"
        rightMsg = " and " ++ show right ++ " birds on the right."

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Right (left, right + n)
  | otherwise                    = Left (leftMsg ++ rightMsg)
  where leftMsg = "Pierre fell with " ++ show left ++ " birds on the left"
        rightMsg = " and " ++ show (right + n) ++ " birds on the right."
