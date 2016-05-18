module ArgParse where

import Data.List (isPrefixOf, partition)
import qualified Data.Map as Map
import System.Environment

type PositionalArgs = [String]
type OptionalArgs = Map.Map Option (Maybe Value)

type Option = String
type Value = String

-- Parse and print the command line args.
main :: IO ()
main = getArgs >>= print . parseArgs

-- | Parse a list of arguments (such as the one returned by
-- | `System.Environment.getArgs`) into the pair (positionals, options).
parseArgs :: [String] -> (PositionalArgs, OptionalArgs)
parseArgs xs = let (opts, args) = partition ("-" `isPrefixOf`) xs
                in (args, parseOptions opts)

-- | Parse a list of options into a Map mapping option names to values.
-- | Later occurences of an option override earlier occurences.
parseOptions :: [String] -> OptionalArgs
parseOptions = foldl addOpt Map.empty
    where addOpt opts arg = let trimmed = dropWhile (=='-') arg
                                (option, rest) = break (=='=') trimmed
                                value = tailMaybe rest
                             in Map.insert option value opts

-- Like `tail`, but returns `Nothing` if the list has less than 2 items.
tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (x:[]) = Nothing
tailMaybe (x:xs) = Just xs
