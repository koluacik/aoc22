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
  let (r1, r2) = evalState parseLine line
  when (r1 `overlaps` r2) (modify' succ)

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

overlaps :: Range -> Range -> Bool
(l1, r1) `overlaps` (l2, r2) = not $ (l1 > r2) || (r1 < l2)
