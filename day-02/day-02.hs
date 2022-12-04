import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Char
import System.IO (isEOF)

main :: IO ()
main = execStateT loop 0 >>= print

type Computation = StateT Int IO

loop :: Computation ()
loop = do
  done <- liftIO isEOF
  unless done (play' >> loop)

-- Part 1
play :: Computation ()
play = do
  line <- liftIO BS.getLine
  let opponent = charToMove $ BS.index line 0
      player = charToMove $ BS.index line 2
      res = calculateScore player opponent
  modify' (+ res)

-- Part 2
play' :: Computation ()
play' = do
  line <- liftIO BS.getLine
  let opponent = charToMove $ BS.index line 0
      expectedResult = charToExpected $ BS.index line 2
      player = requiredMove opponent expectedResult
      res = calculateScore player opponent
  modify' (+ res)

data Move = Rock | Paper | Scissors
  deriving (Eq, Enum, Show)

-- Never ever pass anything except characters in "ABCXYZ"!
charToMove :: Char -> Move
charToMove 'A' = Rock
charToMove 'B' = Paper
charToMove 'C' = Scissors
charToMove other = charToMove (toEnum $ ord 'A' + (ord other - ord 'X'))

charToExpected :: Char -> Ordering
charToExpected c = toEnum (ord c - ord 'X')

requiredMove :: Move -> Ordering -> Move
requiredMove m EQ = m
requiredMove Rock LT = Scissors
requiredMove Paper LT = Rock
requiredMove Scissors LT = Paper
requiredMove Rock GT = Paper
requiredMove Paper GT = Scissors
requiredMove Scissors GT = Rock

calculateScore :: Move -> Move -> Int
calculateScore player opponent = moveToScore player + scoreComparedMoves player opponent
  where
    moveToScore = succ . fromEnum
    scoreComparedMoves = (* 3) `dot` fromEnum `dot` compareMoves
    compareMoves Rock Paper = LT
    compareMoves Rock Scissors = GT
    compareMoves Scissors Paper = GT
    compareMoves x y
      | x == y = EQ
      | otherwise = compare EQ (compareMoves y x)

infixr 9 `dot`

-- compose a function with arity 1 after a function of arity 2.
dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
dot = (.) . (.)
