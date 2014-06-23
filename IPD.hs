
import Control.Arrow
import Data.List
import Data.Function
import Control.Monad
import qualified Data.Map as M
import Data.Monoid ((<>))

data Move = Defect
          | Cooper
          deriving (Show, Eq)

type GameState = ([Move], [Move])
type Strategy = [Move] -> Move

--------------------------------------------------------------------------------
-- | strategies
--------------------------------------------------------------------------------
titForTat :: [Move] -> Move
titForTat []       = Cooper
titForTat (prev:_) = prev

alternate :: [Move] -> Move
alternate xs = if even (length xs) then Cooper else Defect

alternateOdd :: [Move] -> Move
alternateOdd xs = if odd (length xs) then Cooper else Defect

alwaysDefect :: [Move] -> Move
alwaysDefect _ = Defect

alwaysCooperate :: [Move] -> Move
alwaysCooperate _ = Cooper

--------------------------------------------------------------------------------
-- | Scoring
--------------------------------------------------------------------------------
score :: (Move, Move) -> (Int, Int)
score ms =
    case ms of
      (Cooper, Cooper) -> (2, 2) --(R, R)
      (Defect, Defect) -> (1, 1) --(P, P)
      (Cooper, Defect) -> (0, 3) --(S, T)
      (Defect, Cooper) -> (3, 0) --(T, S)

--------------------------------------------------------------------------------
-- | Simulation
--------------------------------------------------------------------------------
progress :: (Strategy, Strategy) -> GameState -> GameState
progress s@(s1, s2) m@(m1, m2) = (s1 m2 : m2, s2 m1 : m1)

simulate :: (GameState -> GameState) -> [(Move, Move)]
simulate sim = drop 1 . map (head *** head) . iterate sim $ ([], [])

compete s = simulate (progress s)

tally :: [(Move, Move)] -> (Int, Int)
tally = foldr tal (0, 0) . map score
  where tal (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

strategies :: [(String, Strategy)]
strategies = [
    ("titForTat", titForTat)
--, ("alwaysCooperate", alwaysCooperate)
  , ("alwaysDefect", alwaysDefect)
  , ("alternate", alternate)
  ]

tournament = do
  (n1, s1) <- strategies
  (n2, s2) <- strategies
  return $ (n1, n2, tally $ take 10 $ compete (s1, s2))

tournamentResults = foldr summarize M.empty
  where
    summarize (n1, n2, (t1, t2)) m = M.alter (upd t1) n1 
                                    (M.alter (upd t2) n2 m)
    upd t1 Nothing  = Just t1
    upd t1 (Just x) = Just (t1 + x)

main = do
  putStrLn "overall"
  putStrLn "-------"
  mapM print $ sortBy (compare `on` negate.snd) (M.toList $ tournamentResults tournament)
  putStrLn "\nversus"
  putStrLn "-------"
  mapM print tournament
