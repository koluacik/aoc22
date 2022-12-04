import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import System.IO (isEOF)

type Computation = StateT Int IO

type Range = (Int, Int)

main :: IO ()
main = execStateT loop 0 >>= print

loop :: Computation ()
loop = do
  done <- liftIO isEOF
  unless done (compute >> loop)

compute :: Computation ()
compute = do
  line <- liftIO BS.getLine
  let (r1, r2) = evalState parseLine line -- Ah yes, monads within monads.
  when (r1 `contains` r2 || r2 `contains` r1) (modify' succ)

-- Parsing logic --
dropLetter :: State BS.ByteString ()
dropLetter = modify' BS.tail

parsePair :: State BS.ByteString Range
parsePair = do
  int1 <- state $ fromJust . BS.readInt
  dropLetter
  int2 <- state $ fromJust . BS.readInt
  return (int1, int2)

parseLine :: State BS.ByteString (Range, Range)
parseLine = do
  pair1 <- parsePair
  dropLetter
  pair2 <- parsePair
  return (pair1, pair2)

contains :: Range -> Range -> Bool
r1 `contains` r2 = fst r1 <= fst r2 && snd r1 >= snd r2