import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Set as S
import System.IO (isEOF)

type Computation = StateT Int IO

main :: IO ()
main = execStateT loop 0 >>= print

loop :: Computation ()
loop = do
  done <- liftIO isEOF
  unless done (compute3Lines >> loop)

compute3Lines :: Computation ()
compute3Lines = do
  lines <- replicateM 3 (liftIO getLine)
  let badge = S.elemAt 0 . foldr1 S.intersection . map S.fromList $ lines
  modify' (+ charToPri badge) 

-- Too lazy, sorry.
charToPri :: Char -> Int
charToPri c
  | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
  | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 27
  | otherwise = error "not a letter"