-- Monad Wars: a version of Dope Wars in Haskell
--
-- Usage:
--    *Main> playTurn startState
-- 
--    *Main> parseMerchandise "pey"
--    *Main> test startState "sell 3 la"
-- 
--    *Main> buy  2 a startState
--    *Main> sell 5 a startState

import Data.Char
import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Map as M
import Debug.Trace

data GameState = GameState {
        turn     :: Integer,
        cash     :: Money,
     -- owed     :: Money,
     -- bank     :: Money,
        location :: Location,
        stock    :: [Stock], -- carried
        avail    :: [Stock], -- here
        debug    :: String
    } deriving Show

type Location = Integer
type Money    = Integer

commandMap = getPrefixMap [
            ( "buy",  cmdBuy  ),
            ( "sell", cmdSell ),
            ( "jet",  cmdJet ),
            ( "quit", cmdQuit )
          ]

op `on` p = (\a b -> p a `op` p b)

tokenizeLine :: [Char] -> [[Char]]
tokenizeLine = concatMap (groupBy ((==) `on` isLetter)) . words

startState = GameState {
    turn     = 1,
    cash     = 2000,
    location = 0,
    debug    = "",
    stock    = myStock,
    avail    = availableStock
 }

-- some constants
maxTurns = 5
prompt   = "> "

nextTurn :: GameState -> GameState
nextTurn gs = gs { turn = succ $ turn gs }

modScore :: Integer -> GameState -> GameState
modScore d gs = gs { cash = cash gs + d }

isEnd gs = turn gs > maxTurns

{-
-- broken - f is in Maybe, this is an IO monad
playTurn gs = do showStatus gs
                 putStr prompt
                 s <- getLine
                 f <- parseLine s
                 let gs' = f gs
                 if isEnd gs' 
                    then endGame  gs'
                    else playTurn gs'
-}

parseLine :: [Char] -> Maybe (GameState -> Maybe GameState)
parseLine s = do let (cmd:pars) = tokenizeLine s
                 c  <- parseCommand cmd
                 c' <- c pars
                 return c'

parseInt s | all isDigit s = return $ read s
           | otherwise     = fail "Not an integer"

parseCommand     = parseMap commandMap

parseMerchandise = parseMap merchandiseMap

data Merchandise = Merchandise {
      name :: String,
      priceRange :: (Money, Money)
    } deriving (Show, Eq)

a = merchandise !! 0

toLowerS = map toLower
merchandiseMap = getPrefixMap $ 
                 map (toLowerS . name &&& id)
                 merchandise

-- using a list of [Stock] seemed like a good idea at the time
-- perhaps would be better as Map?
--
lookupStock :: Merchandise -> [Stock] -> Maybe Stock
lookupStock k s = lookup k $ map (item &&& id) s

adjustStock k = adjustList (\i -> item i == k)

adjustList p s f = let (l,r) = break p s
                   in case r of
                        (i:rs)    -> l ++ (f i : rs)
                        otherwise -> s

data Stock = Stock {
        item    :: Merchandise,
        qty     :: Integer,
        price   :: Money
    } deriving Show

setPrice p i = i { price = p }
modQty   d i = i { qty   = qty i + d }


cmdMerchandise f (n:m:_) = do n' <- parseInt n
                              m' <- parseMerchandise m
                              Just $ f n' m'
cmdMerchandise _ _       = Nothing

cmdBuy  = cmdMerchandise  doBuy
cmdSell = cmdMerchandise  doSell

-- Just a stub: we'll probably want to set an "endflag" or similar
cmdQuit _ = Just doQuit
doQuit gs = return $ modScore (-10) gs { 
               debug = "Quitter!" }

cmdJet (n:_) = do n' <- parseInt n
                  Just $ doJet n'
cmdJet _     = Nothing

doJet n gs | n == location gs
              = return $ gs { 
                    debug = "You are already in location " ++ show n }
            | otherwise 
              = return $ nextTurn gs { 
                    location = n,
                    debug    = "You have moved to " ++ show n }

doBuy :: (Monad m) => Integer -> Merchandise -> GameState -> m GameState
doBuy  n m gs = return gs {
    debug = "You bought " ++ 
            (show n) ++ " " ++
            (name m)
    }

doSell :: (Monad m) => Integer -> Merchandise -> GameState -> m GameState
doSell  n m gs = return gs {  
    debug = "You sold " ++
            (show n) ++ " " ++
            (name m)
    }

-- if we had:  my <- lookupStock ... here, then we wouldn't be able to buy
-- unless we already possessed some of this stuff
buy n m gs = do here <- lookupStock m $ avail gs
                let cost = n * (price here)
                guard $ cost <= cash gs
                let my = lookupStock m $ stock gs >>= return

                -- todo refactor!
                -- right now we have 2 cases: where we have an
                -- existing quantity of goods, and where we have
                -- none.  Perhaps it would be better to just take
                -- a list of [Just new, Maybe existing] and fold
                -- the average/add operation over them!
                -- (I think this will be quite a useful refactoring
                -- to go through)
                let new_state = gs { cash = (cash gs) - cost }
                return $ case my of
                    Just my' ->
                        let unit_cost = avg_price (price my', qty my') 
                                                  (price here,      n)
                        in new_state {
                             stock = adjustStock m (stock gs) 
                                     (modQty n . setPrice unit_cost)
                         }
                    otherwise -> 
                         let new_item = Stock {
                            price = price here,
                            qty   = n,
                            item  = m
                         }
                         in new_state {
                             stock = new_item : (stock gs)
                         }

sell n m gs = do my   <- lookupStock m $ stock gs
                 here <- lookupStock m $ avail gs
                 let cost = n * (price here)
                 guard $ n <= qty my
                 -- the reference version doesn't affect price when you sell
                 -- which in some cases can be a bug (e.g. if you then buy
                 -- back)
                 -- TODO: this can leave an empty record
                 -- (Eg a stock with qty 0)
                 return gs {
                     cash  = (cash gs) + cost,
                     stock = adjustStock m (stock gs) 
                              $ modQty (-n)
                 }

avg_price (p1, q1) (p2, q2) = let tot1 = p1 * q1
                                  tot2 = p2 * q2
                                  tot  = tot1 + tot2
                                  q    = q1 + q2
                              in tot `div` q

-- getR is just a function to generate a slightly
-- interesting example list to play with.
-- (we'll worry about random later)
--
myStock        = getR False $ take 2 merchandise
availableStock = getR True  $ take 4 merchandise

getR b m = map getR' $ zip m $ iterate not b
           where getR' (i,b) = Stock {
                    item  = i,
                    qty   = 10,
                    price = (if b then fst else snd) $ priceRange i
                        }

merchandise :: [Merchandise]
merchandise = [
    Merchandise { name = "Arrows"  ,priceRange = ( 1000,  4400 ) },
    Merchandise { name = "Curry"   ,priceRange = (15000, 29000 ) },
    Merchandise { name = "Kleisli" ,priceRange = (  480,  1280 ) },
    Merchandise { name = "Haskell" ,priceRange = ( 5500, 13000 ) },
    Merchandise { name = "Lambdas" ,priceRange = (   11,    60 ) },
    Merchandise { name = "STM"     ,priceRange = ( 1500,  4400 ) },
    Merchandise { name = "Monads"  ,priceRange = (  540,  1250 ) },
    Merchandise { name = "GHC"     ,priceRange = ( 1000,  2500 ) },
    Merchandise { name = "Peyton"  ,priceRange = (  220,   700 ) },
    Merchandise { name = "Fundeps" ,priceRange = (  630,  1300 ) },
    Merchandise { name = "Zipper"  ,priceRange = (   90,   250 ) },
    Merchandise { name = "Endo"    ,priceRange = (  315,   890 ) }
    ]


-- can contain duplicates!
getPrefixes :: [([a], t)] -> [([a], t)]
getPrefixes = concatMap $ uncurry zip . (tail . inits . fst &&& repeat . snd)

getPrefixMap :: (Ord a) => [([a], t)] -> M.Map [a] t
getPrefixMap = M.fromList . getPrefixes

-- this maps to Maybe if necessary, but can be used in
-- different monads too
-- parseMap :: (Ord k, Monad m) => M.Map k a -> k -> m a
parseMap = flip M.lookup


test gs s = do c <- parseLine s
               c gs


showStatus gs = putStrLn $ show gs

endGame gs = do putStrLn   "Game over!"
                putStrLn $ "Your score was " ++ (show $ cash gs)
                return ()
