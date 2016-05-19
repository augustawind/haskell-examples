module TextAdventure where

import Control.Monad (mapM_)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import StringFormat

type Dispatcher = Map.Map String (IO ())

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
dispatch' = dispatch promptStr' textWidth'

-- Main loop:
main = do
    printLines ["You've decided to set out on an adventure."
                ,"You've left your house and taken the path to a crossroads."]

    name <- prompt' "What is your name?"
    
    printWrap' ("Hello, %! Your adventure begins..." -%- [name])
    pause

    hr'

    firstCrossroads

firstCrossroads = do
    let switch = Map.fromList [("left", goLeft), ("right", goRight)]

    dispatch' firstCrossroads switch "Which direction will you take?"

goLeft = printWrap' "You went left!"
goRight = printWrap' "You went right!"

-- Control flow.
-- ---------------------------------------------------------------------------

-- Print something then prompt for input. 
prompt :: String -> Int -> String -> IO String
prompt promptString width message = do putStr str
                                       answer <- fmap strip getLine
                                       blankLine
                                       return answer
    where str = wordWrap width message ++ ('\n':promptString)

-- Like prompt, but takes a list of possible choices and returns the user's
-- choice with the context of possible failure. Options are case-insensitive.
cmdPrompt :: String -> Int -> [String] -> String -> IO (Maybe String)
cmdPrompt promptStr width opts msg = do putStr str
                                        choice <- getLine
                                        blankLine

                                        let choice' = normalize choice
                                            opts' = map normalize opts
                                        return $
                                            if choice' `elem` opts'
                                               then Just choice'
                                               else Nothing

   where str = wordWrap width msg ++ ('\n':optStr) ++ (' ':promptStr)
         optStr = "(" ++ intercalate ", " opts ++ ")" 

-- Like prompt, but takes a Map mapping possible choices to IO actions and
-- executes the action corresponding with the user's choice, or calls retry
-- on the caller if an invalid option was given. Options are case-insensitive.
dispatch :: String -> Int -> IO () -> Dispatcher -> String -> IO ()
dispatch promptStr width caller disp msg = do putStr str
                                              choice <- getLine
                                              blankLine

                                              let choice' = normalize choice

                                              case choice' `Map.lookup` disp' of
                                                Nothing -> retry caller
                                                Just action -> action

   where str = wordWrap width msg ++ ('\n':optStr) ++ (' ':promptStr)
         disp' = Map.mapKeys normalize disp
         optStr = "(" ++ intercalate ", " (Map.keys disp') ++ ")" 

-- Print a "try again" message and execute a given IO action.
retry :: IO () -> IO ()
retry action = putStrLn "Invalid input. Please try again." >> blankLine >> action

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

-- Pause execution and wait for a keypress to continue.
pause :: IO ()
pause = putStr "<Press any key to continue...>" >> getChar >> return ()

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
