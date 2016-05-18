infixl 4 %%
(%%) :: String -> [String] -> String
"" %% _     = ""
str %% []   = str
str %% vars = before ++ head vars ++ (after %% tail vars)
    where (before,  rest) = break (=='%') str
          after = drop 1 rest

main = do
    print "What is your name?"
    name <- getLine
    print ("Hello, %!" %% [name])
