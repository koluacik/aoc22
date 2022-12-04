import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List (insertBy)
import Data.Maybe
import Data.Ord
import System.IO (isEOF)

type Computation = StateT ([Int], [Int]) IO

main :: IO ()
main = print . sum . fst =<< execStateT computeLine ([], [])

computeLine :: Computation ()
computeLine = do
  stop <- liftIO isEOF
  unless stop $ do
    line <- liftIO BS.getLine
    if BS.null line
      then computeSingleElf
      else pushToStack . parseInt $ line
    computeLine

parseInt :: BS.ByteString -> Int
parseInt = fst . fromJust . BS.readInt

pushToStack :: Int -> Computation ()
pushToStack x = modify' ((x :) <$>)

computeSingleElf :: Computation ()
computeSingleElf = do
  (maximums, stack) <- get
  let newMaximums = insertBy (compare `on` Down) (sum stack) maximums
  put (take 3 newMaximums, [])
