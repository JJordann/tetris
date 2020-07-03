data State = State {
    score :: Integer,
    test :: Bool }
     deriving (Show)

mtest = State {score = 1, test = True}

mtest' = mtest { test = False }
