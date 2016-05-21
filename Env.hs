import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- The global namespace ("environment"), like in imperative languages.
type Env = State Vars

-- Mapping of variable names to values.
type Vars = Map.Map String String
type Name = String
type Value = String

-- "Assignment" operator. Returns ().
(==>) :: Name -> Value -> Env ()
name ==> value = state $ \vars -> ((), Map.insert name value vars)

-- Get a value in an Env by its name, returning the value.
valueOf :: Name -> Env Value
valueOf name = state $ \vars -> (fromMaybe "null" (Map.lookup name vars), vars)

exampleA = do
    "x" ==> "5"
    "y" ==> "12"
    x <- valueOf "x"
    y <- valueOf "y"
    z <- valueOf "z"
    return $ x ++ ", " ++ y ++ ", " ++ z

exampleB =
    "x" ==> "5" >>
    "y" ==> "12" >>
    valueOf "x" >>= \x ->
    valueOf "y" >>= \y ->
    valueOf "z" >>= \z ->
    return $ x ++ ", " ++ y ++ ", " ++ z
