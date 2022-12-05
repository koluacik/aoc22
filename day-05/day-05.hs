{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import System.Environment (getArgs)

-- Mom, can we have arr[i].push_back(x);???
type Computation = StateT (ProgramState (IOArray Int)) IO

type Stack = Seq Char

data ProgramState f = ProgramState
  { stacks :: f Stack,
    operations :: [Operation]
  }

deriving instance (Show (f Stack)) => Show (ProgramState f)

deriving instance (Eq (f Stack)) => Eq (ProgramState f)

data Operation = Operation
  { moveCount :: Int,
    source :: Int,
    destination :: Int
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  input <- BS.getContents
  let firstLine = BS.takeWhile (/= '\n') input
      nonNumericPrefix = BS.takeWhile (\c -> c > '9' || c < '1') input
      stackCount = BS.length firstLine `div` 4
      stackDepth = BS.length $ BS.filter (== '\n') nonNumericPrefix
      (Right program) = parseOnly (parseAll stackCount stackDepth) input

  ioarr <- stacks <$> (execStateT loop =<< createMutableProgramState program)
  arr <- freeze ioarr :: IO (Array Int Stack)
  let go S.Empty xs = xs
      go (c S.:<| cs) xs = c : xs
      msg = foldr go "" arr

  putStrLn msg

-- Parsing logic
eol' :: Parser ()
eol' = void (word8 10)

space' :: Parser ()
space' = void (word8 32)

parseFullCell :: Parser Char
parseFullCell = char '[' *> anyChar <* char ']'

parseEmptyCell :: Parser ()
parseEmptyCell = void (count 3 space')

parseCell :: Parser (Maybe Char)
parseCell = Just <$> parseFullCell <|> Nothing <$ parseEmptyCell

parseStackLayer :: Int -> Parser [Maybe Char]
parseStackLayer stackCount = count stackCount (parseCell <* (space' <|> endOfLine))

parseStacks :: Int -> Int -> Parser [Seq Char]
parseStacks stackCount stackDepth = do
  let stacks = replicate stackCount S.empty
  layers <- count stackDepth (parseStackLayer stackCount)
  pure $ foldl' addLayerBelow stacks layers
  where
    addLayerBelow = zipWith maybePrepend
    maybePrepend sqnc Nothing = sqnc
    maybePrepend sqnc (Just c) = sqnc |> c

parseOperation :: Parser Operation
parseOperation =
  Operation <$> (string "move " *> decimal <* string " from ") <*> (decimal <* string " to ") <*> decimal

skipLine :: Parser ()
skipLine = P.takeTill isEndOfLine >> endOfLine >> pure mempty

parseAll :: Int -> Int -> Parser (ProgramState [])
parseAll stackCount stackDepth = do
  stacks <- parseStacks stackCount stackDepth
  count 2 skipLine
  operations <- many' (parseOperation <* skipSpace)
  pure $ ProgramState stacks operations

-- Business logic
loop :: Computation ()
loop = do
  done <- gets (null . operations)
  unless done (computeOperation >> loop)

computeOperation :: Computation ()
computeOperation = do
  ((Operation cnt src dst) : ops) <- gets operations
  pop src cnt >>= push1 dst {- push2 dst -}
  modify' (\s -> s {operations = ops})

-- We can, but it is disgusting.
createMutableProgramState :: ProgramState [] -> IO (ProgramState (IOArray Int))
createMutableProgramState state =
  ProgramState
    <$> newListArray (1, length (stacks state)) (stacks state)
    <*> pure (operations state)

pop :: Int -> Int -> Computation Stack
pop ix cnt = do
  ss <- gets stacks
  sqnc <- liftIO $ readArray ss ix
  let (popped, remaining) = S.splitAt cnt sqnc
  liftIO $ writeArray ss ix remaining
  pure popped

-- Part 1
push1 :: Int -> Stack -> Computation ()
push1 = push' S.reverse

-- Part 2
push2 :: Int -> Stack -> Computation ()
push2 = push' id

push' :: (Stack -> Stack) -> Int -> Stack -> Computation ()
push' f ix stack = do
  ss <- gets stacks
  sqnc <- liftIO $ readArray ss ix
  liftIO $ writeArray ss ix (f stack <> sqnc)