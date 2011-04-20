-- Monad Wars: a version of Dope Wars in Haskell
--
-- Usage:
--    *Main> playTurn startState

data GameState = GameState {
        turn     :: Integer,
        score    :: Integer,
        location :: Location
    } deriving Show

type Location = Integer

startState = GameState {
    turn     = 0,
    score    = 0,
    location = 0
  }

-- some constants
maxTurns = 5
prompt   = "> "

nextTurn :: GameState -> GameState
nextTurn gs = gs { turn = succ $ turn gs }

modScore :: Integer -> GameState -> GameState
modScore d gs = gs { score = score gs + d }

isEnd gs = turn gs > maxTurns

playTurn gs = do showStatus gs
                 putStr prompt
                 s <- getLine
                 let f = parseLine s
                 let gs' = f gs
                 if isEnd gs' 
                    then endGame  gs'
                    else playTurn gs'

-- stub
parseLine s =   nextTurn 
              . modScore 10 

showStatus gs = putStrLn $ show gs

endGame gs = do putStrLn   "Game over!"
                putStrLn $ "Your score was " ++ (show $ score gs)
                return ()
