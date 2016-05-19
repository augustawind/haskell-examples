module TextAdventure where

import Control.Monad (mapM_)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import StringFormat

data Adventure = End
               | Do Action Adventure
               | Prompt Input Switch

type Switch = Map.Map String Adventure
type Action = IO ()
type Input = [String] -> IO String

-- Example usage.
-- ---------------------------------------------------------------------------

promptStr' = ">> "
lineChar' = '-'
textWidth' = 78

-- Customized functions:

hr' = hr lineChar' textWidth'
printWrap' = printWrap textWidth'
prompt' = prompt promptStr' textWidth'
cmdPrompt' = cmdPrompt promptStr' textWidth'

-- The adventure:

main = run myAdventure

myAdventure =
    Do intro $
        Prompt firstCrossroads $
            Map.fromList [("left", Do goLeft End), ("right", Do goRight End)]

intro = do
    printLines ["You've decided to set out on an adventure."
                ,"You've left your house and taken the path to a crossroads."]

    name <- prompt' "What is your name?"
    
    printWrap' ("Hello, %! Your adventure begins..." -%- [name])
    pause

    hr'

firstCrossroads opts = do
    cmdPrompt' opts "Which direction will you take?"

goLeft = printWrap' "You went left!"
goRight = printWrap' "You went right!"

-- Control flow.
-- ---------------------------------------------------------------------------

-- Run an Adventure.
run :: Adventure -> IO ()
run End = printWrap' "Game over!"
run (Do action adventure) = action >> run adventure
run this@(Prompt input switch) = do
    let switch' = Map.mapKeys normalize switch
    choice <- input (Map.keys switch')
    case Map.lookup choice switch' of
      Nothing        -> retry (run this)
      Just adventure -> run adventure

-- Same as prompt, but also takes a list of possible choices (`opts`) and
-- prints them, normalizing the input (see `normalize`).
cmdPrompt :: String -> Int -> [String] -> String -> IO String
cmdPrompt promptStr width opts msg = do putStr str
                                        choice <- getLine
                                        blankLine
                                        return $ normalize choice

   where str = wordWrap width msg ++ ('\n':optStr) ++ (' ':promptStr)
         optStr = "(" ++ intercalate ", " opts ++ ")" 

-- Print something then prompt for input. 
prompt :: String -> Int -> String -> IO String
prompt promptString width message = do putStr str
                                       answer <- fmap strip getLine
                                       blankLine
                                       return answer
    where str = wordWrap width message ++ ('\n':promptString)

-- Print a "try again" message and execute a given IO action.
retry :: Action -> Action
retry action = putStrLn "Invalid input. Please try again." >> blankLine >> action

-- Pause execution and wait for a keypress to continue.
pause :: IO ()
pause = putStr "<Press any key to continue...>" >> getChar >> return ()

-- Output.
-- ---------------------------------------------------------------------------

-- Print a blank line.
blankLine :: IO ()
blankLine = putChar '\n'

-- Print a horizontal rule.
hr :: Char -> Int -> IO ()
hr char width = putStrLn (replicate width char) >> blankLine

-- Print a list of Strings line by line.
printLines :: [String] -> IO ()
printLines xs = mapM_ putStrLn xs >> blankLine

-- Print a String, wrapping its text to the given width.
printWrap :: Int -> String -> IO ()
printWrap width str = putStrLn (wordWrap width str) >> blankLine

-- String manipulation helpers.
-- ---------------------------------------------------------------------------

-- Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` [' ', '\t', '\n', '\r'])

-- Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = take width str' ++ "\n" ++ drop width str'
  where str' = filter notLineBreak str
        notLineBreak = not . (`elem` "\n\r")
