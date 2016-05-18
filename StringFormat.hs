module StringFormat (fChr, (-%-), (-%%-)) where

import Data.List (findIndices)

-- | Formatting escape character.
fChr :: Char
fChr = '%'

-- | String formatting operator.
-- | Each occurence of `fChr` in the given String (`str`) will be replaced with a
-- | String from the given list of replacements (`vars`), in the order they
-- | appear in the list.. Once each item in `vars` has been consumed, further
-- | items will be ignored and further occurences of `fChr` will be left as-is.
infixl 4 -%-
(-%-) :: String -> [String] -> String
""  -%- _    = ""
str -%- []   = str
str -%- vars = before ++ head vars ++ (after -%- tail vars)
    where (before,  rest) = break (==fChr) str
          after = drop 1 rest

-- | String formatting by name. Items in the given String to be replaced are 
-- | denoted in the form '%name%', where name matches a String key in the Map-like
-- | list of pairs `vars`, replaced by the corresponding value. If a name doesn't
-- | match, it is simply ignored.
infixl 4 -%%-
(-%%-) :: String -> [(String, String)] -> String
""     -%%- _    = ""
(x:"") -%%- _    = [x]
str    -%%- vars = case splitBetween fChr str of 
    Nothing                    -> str
    Just (before, name, after) -> let value = case lookup name vars of
                                                Nothing  -> (fChr:name) ++ [fChr]
                                                Just val -> val
                                   in before ++ value ++ (after -%%- vars)

-- Helper function for (-%%-). Given a char and a String, splits the string
-- into (x, Maybe y, z) where y is the first occurence of some text between
-- two of the given char, x is the text before it, and z is the text after.
--
-- If no such sandwich is found, the result will be (x, Nothing, ""), where
-- x is the given String in its entirety.
--
-- For example, given the separator % and the String "I %like% turtles", the
-- result will be ("I ", "like", " turtles"), and given the String
-- "I %like turtles", the result will be ("I %like turtles", Nothing, "").
splitBetween :: Char -> String -> Maybe (String, String, String)
splitBetween c str
  | null indices || ((<2) . length) indices = Nothing
  | otherwise = Just (before, middle, after)
    where (before, (_:rest)) = break (==c) str
          (middle, (_:after)) = break (==c) rest
          indices = findIndices (==c) str

-- Example usage of (-%-).
exampleA = "I once was a % named %, who % on his % who was %." -%- vars
    where vars = ["kid", "Larry", "sat", "cat", "harry"]

-- Example usage of (-%%-).
exampleB = "My favorite movie is %movie%, and I'm %age% years old." -%%- vars
    where vars = [("age", "27"), ("movie", "Karate Kid")]
