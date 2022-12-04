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
  unless done (compute >> loop)

compute :: Computation ()
compute = do
  line <- liftIO getLine
  let len = length line
      (leftHalf, rightHalf) = splitAt (len `div` 2) line
      intersection = S.fromList leftHalf `S.intersection` S.fromList rightHalf
      cost = S.foldl' (\acc r -> acc + charToPri r) 0 intersection
  modify' (+ cost)

-- Too lazy, sorry.
charToPri :: Char -> Int
charToPri c
  | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
  | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 27
  | otherwise = error "not a letter"