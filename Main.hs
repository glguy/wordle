{-# Language BlockArguments, ImportQualifiedPost #-}
{- |
Module      : Main
Description : The whole program
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main (main) where

import Control.Applicative
import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (foldl', delete, groupBy, sortBy, mapAccumL)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import System.Console.ANSI (hideCursor, showCursor, clearLine)
import System.Console.ANSI.Codes
import System.Random (randomRIO)
import Text.Printf (printf)
import System.IO

import Options

topHint :: String
topHint = "RAISE"

main :: IO ()
main =
 do opts <- getOptions
    case optMode opts of
      Solve xs -> withoutCursor (solver opts xs)
      Give     -> withoutCursor (give opts)
      Play     -> withoutCursor (play opts)

withoutCursor :: IO c -> IO c
withoutCursor m = bracket_ hideCursor showCursor
 do hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn (setSGRCode [SetColor Foreground Dull Magenta] ++ "[ W O R D L E ]")
    m

-- | Play wordle with a randomly-selected word
play :: Options [String] -> IO ()
play opts =
 do w <- randomFromList (optWordlist opts)
    playLoop (optStrategy opts) (optDictionary opts) (optDictionary opts) w Map.empty

-- | Play wordle with a manually-selected word
give :: Options [String] -> IO ()
give opts =
 do w <- getSecret (optDictionary opts)
    playLoop (optStrategy opts) (optDictionary opts) (optDictionary opts) w Map.empty

playLoop :: Strategy -> [String] -> [String] -> String -> Map.Map Char Clue -> IO ()
playLoop strat dict remain answer letters =
 do w <- getWord strat letters dict remain
    let clue = computeClues answer w
    let remain' = filter (\x -> computeClues x w == clue) remain
    let letters' = Map.unionWith max letters (Map.fromListWith max (zip w clue))
    putStrLn ('\r' : prettyWord (zip (Just <$> clue) w))
    unless (clue == replicate 5 Hit)
      (playLoop strat dict remain' answer letters')

-- | Use the metric computation to have the computer make guesses.
-- The user must provide the clue responses. The solver will use
-- the given guess-list to start and then switch to metrics when
-- the list is exhausted.
solver ::
  Options [String] ->
  [String] {- ^ initial guesses -} ->
  IO ()
solver opts start =
  solverLoop
    (optStrategy opts)
    (map (map toUpper) start <++ [topHint])
    (optDictionary opts) (optDictionary opts)

solverLoop ::
  Strategy ->
  [String] ->
  [String] ->
  [String] ->
  IO ()

solverLoop _ _ _ [] =
 do putStrLn (colorWord [(Red,x) | x <- "ERROR"])

solverLoop _ _ _ [answer] =
 do putStrLn (prettyWord [(Just Hit, x) | x <- answer])

solverLoop strat nexts dict remain =
 do (next, nexts') <- case nexts of
                        x:xs -> pure (x,xs)
                        []   -> do x <- randomFromList (pickWord strat dict remain)
                                   pure (x,[])
    answer <- getClue (length remain) next
    putStrLn ""
    unless (answer == replicate 5 Hit)
      (solverLoop strat nexts' dict (filter (\w -> computeClues w next == answer) remain))

-- | Render a word with colors indicating clue status
prettyWord :: [(Maybe Clue, Char)] -> String
prettyWord xs = colorWord [(maybe Blue clueColor c, x) | (c,x) <- xs]

clueColor :: Clue -> Color
clueColor Hit  = Green
clueColor Miss = Black
clueColor Near = Yellow

colorWord :: [(Color, Char)] -> String
colorWord w = setSGRCode [SetConsoleIntensity BoldIntensity] ++
              foldr f (setSGRCode [Reset]) w
  where
    f (x,y) z =
      setSGRCode [SetColor Background Dull x, SetColor Foreground Dull White] ++
      [' ',y,' '] ++ z

-- * Word choice heuristic

-- | Find the worst case number of words remaining given a guessed word.
metric ::
  Strategy ->
  [String] {- ^ remaining words -} ->
  String   {- ^ guessed word -} ->
  Double {- ^ worst case words remaining after guess -}
metric strat dict word = f (Map.fromListWith (+) [(computeClues w word, 1) | w <- dict])
  where
    f = case strat of
          WorstCase -> fromIntegral . maximum
          MaxEntropy -> negEntropy
          SumOfSquares -> fromIntegral . sum . fmap (\x -> x*x)
          MostChoices  -> negate . fromIntegral . length

negEntropy :: (Foldable f, Functor f) => f Int -> Double
negEntropy ns = sum (h <$> ns) where
    h n = let p = fromIntegral n / denom in p * log p
    denom = fromIntegral (sum ns) :: Double

-- | Given a dictionary and a list of remaining possibilities,
-- find the words with the minimimum metric. Words from the
-- remaining list are preferred to those not from the list
-- when the metric is otherwise equal.
pickWord ::
  Strategy ->
  [String] {- ^ dictionary -} ->
  [String] {- ^ remaining -} ->
  [String] {- ^ selections -}
pickWord strat dict remain = [x | x <- xs, x `elem` remain] <++ xs
  where
    xs = map snd
       $ head
       $ groupBy ((==) `on` fst)
       $ sortBy (comparing fst)
       $ [(metric strat remain w, w) | w <- dict]

-- * Input modes

getClue :: Int -> String -> IO [Clue]
getClue n w = go []
  where
    go acc =
     do putStr ('\r' : prettyWord (zip (map Just acc ++ replicate (5 - length acc) Nothing) w)
              ++ setSGRCode [Reset, SetColor Foreground Dull Black]
              ++ printf "  %5d" n)
        hFlush stdout
        input <- getChar
        case input of
          'g'    | length acc < 5  -> go (acc ++ [Hit])
          'b'    | length acc < 5  -> go (acc ++ [Miss])
          'y'    | length acc < 5  -> go (acc ++ [Near])
          '\DEL' | not (null acc)  -> go (init acc)
          '\n'   | length acc == 5 -> pure acc
          _                        -> go acc

getSecret :: [String] -> IO [Char]
getSecret dict = go []
  where
    go acc =
     do putStr ('\r' : prettyWord [(Just Hit, x) | x <- take 5 ('◆' <$ acc <|> repeat '·')])
        hFlush stdout
        c <- toUpper <$> getChar
        case c of
          '\n'   | acc `elem` dict                    -> pure acc
          '\DEL' | not (null acc)                     -> go (init acc)
          _      | 'A' <= c, c <= 'Z', length acc < 5 -> go (acc ++ [c])
                 | otherwise                          -> go acc

getWord :: Strategy -> Map Char Clue -> [String] -> [String] -> IO [Char]
getWord strat letters dict remain = go []
  where
    go acc =
     do putStr ('\r' : colorWord [(Blue, x) | x <- take 5 (acc ++ repeat '·')]
                  ++ "    " ++
                  prettyWord [(Map.lookup x letters, x) | x <- ['A' .. 'Z']])
        hFlush stdout
        c <- toUpper <$> getChar
        case c of
          '\n'   | acc `elem` dict                    -> acc <$ clearLine
          '\DEL' | not (null acc)                     -> go (init acc)
          '?' | length remain > 1000                  -> go topHint
              | otherwise -> go =<< randomFromList (pickWord strat dict remain)
          _      | 'A' <= c, c <= 'Z', length acc < 5 -> go (acc ++ [c])
                 | otherwise                          -> go acc

-- * Game logic

-- | Per-letter clues.
--
-- Clues are intentionally ordered such that the best hint is the /maximum/.
data Clue
  = Miss -- ^ Letter is not in the word, or is covered by an earlier duplicate
  | Near -- ^ Letter is in the word but not at this location
  | Hit  -- ^ Letter is in the word at this location
  deriving (Eq, Ord, Read, Show)

-- | Compute the letter clues for a guessed word.
computeClues ::
  String {- ^ target word      -} ->
  String {- ^ guessed word     -} ->
  [Clue] {- ^ per-letter clues -}
computeClues answer input = snd (mapAccumL clue1 nears (zip answer input))
  where
    nears = foldl' addLetter [] (zip answer input)

    addLetter m (a,i)
      | a == i    = m
      | otherwise = a:m

    clue1 m (x,y)
      | x == y     = (         m, Hit )
      | y `elem` m = (delete y m, Near)
      | otherwise  = (         m, Miss)

-- * List utilities

-- | Uniform selection of an element from a list.
randomFromList :: [a] -> IO a
randomFromList xs
  | null xs = fail "randomFromList: empty list"
  | otherwise =
     do i <- randomRIO (0, length xs - 1)
        pure $! xs !! i

-- | Biased choice; return the first list unless it's empty.
(<++) :: [a] -> [a] -> [a]
[] <++ xs = xs
xs <++ _  = xs
