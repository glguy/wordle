{-# Language BlockArguments, ViewPatterns, ImportQualifiedPost #-}
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
import Data.List (foldl', groupBy, sortBy, sortOn, mapAccumL)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import System.Console.ANSI (hideCursor, showCursor, clearLine)
import System.Console.ANSI.Codes
import System.Environment (getArgs)
import System.IO (hFlush, hSetBuffering, hSetEcho, stdin, stdout, BufferMode(NoBuffering))
import System.Random (randomRIO)
import Text.Printf (printf)

topHint :: String
topHint = "SERAI"

getDictionary :: IO [String]
getDictionary = lines <$> readFile "all.txt"

main :: IO ()
main =
 do args <- getArgs
    case args of
      ("solve":start) -> withoutCursor (solver start)
      ["play"]  -> withoutCursor play
      ["give"]  -> withoutCursor give
      _         ->
        putStr "Usage: wordle <solve|play|give>\n\
               \\n\
               \Modes:\n\
               \    solve - Program guesses a secret word, reply with 'b' 'y' 'g'\n\
               \    play  - Program picks a random word, type in your guess words\n\
               \    give  - User types in the secret words, then types in guess words\n"

withoutCursor :: IO c -> IO c
withoutCursor m = bracket_ hideCursor showCursor
 do hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn (setSGRCode [SetColor Foreground  Dull Magenta] ++ "[ W O R D L E ]")
    m

-- | Play wordle with a randomly-selected word
play :: IO ()
play =
 do d <- getDictionary
    p <- lines <$> readFile "play.txt"
    w <- randomFromList p
    playLoop d d w Map.empty

-- | Play worded with a manually-selected word
give :: IO ()
give =
 do d <- getDictionary
    w <- getSecret d
    playLoop d d w Map.empty

playLoop :: [String] -> [String] -> String -> Map.Map Char Clue -> IO ()
playLoop dict remain answer letters =
 do w <- getWord letters dict remain
    let clue = computeClues answer w
    let remain' = filter (\x -> computeClues x w == clue) remain
    let letters' = Map.unionWith max letters (Map.fromListWith max (zip w clue))
    putStrLn ('\r' : prettyWord (zip (Just <$> clue) w))
    unless (clue == replicate 5 Hit)
      (playLoop dict remain' answer letters')

-- | Use the metric computation to have the computer make guesses.
-- The user must provide the clue responses. The solver will use
-- the given guess-list to start and then switch to metrics when
-- the list is exhausted.
solver ::
  [String] {- ^ initial guesses -} ->
  IO ()
solver start =
 do allWords <- getDictionary
    solverLoop (map (map toUpper) start <++ [topHint]) allWords allWords

solverLoop ::
  [String] ->
  [String] ->
  [String] ->
  IO ()

solverLoop _ _ [] =
 do putStrLn (colorWord [(Red,x) | x <- "ERROR"])

solverLoop _ _ [answer] =
 do putStrLn (prettyWord [(Just Hit, x) | x <- answer])

solverLoop nexts dict remain =
 do (next, nexts') <- case nexts of
                        x:xs -> pure (x,xs)
                        []   -> do x <- randomFromList (pickWord dict remain)
                                   pure (x,[])
    answer <- getClue (length remain) next
    putStrLn ""
    unless (answer == replicate 5 Hit)
      (solverLoop nexts' dict (filter (\w -> computeClues w next == answer) remain))

-- | Render a word with colors indicating clue status
prettyWord :: [(Maybe Clue, Char)] -> String
prettyWord xs = colorWord [(maybe Blue clueColor c, x) | (c,x) <- xs]

clueColor :: Clue -> Color
clueColor Hit  = Green
clueColor Miss = Black
clueColor Near = Yellow

colorWord :: [(Color, Char)] -> String
colorWord ((x,y):xs) =
  setSGRCode [SetColor Background Dull x, SetColor Foreground Dull White] ++
  [' ',y,' '] ++
  colorWord xs
colorWord _ = setSGRCode [Reset]

-- * Word choice heuristic

-- | Find the worst case number of words remaining given a guessed word.
metric ::
  [String] {- ^ remaining words -} ->
  String   {- ^ guessed word -} ->
  Int {- ^ worst case words remaining after guess -}
metric dict word = maximum (Map.fromListWith (+) [(computeClues w word, 1) | w <- dict])

difficulty :: [String] -> String -> Int
difficulty dict answer = go 1 (learn topHint dict)
  where
    learn v = filter (\w -> computeClues answer v == computeClues w v)
    go acc [_] = acc
    go acc xs = go (acc+1) (learn next xs)
      where
        next = head (sortOn (metric xs) dict)

-- | Given a dictionary and a list of remaining possibilities,
-- find the words with the minimimum metric. Words from the
-- remaining list are preferred to those not from the list
-- when the metric is otherwise equal.
pickWord ::
  [String] {- ^ dictionary -} ->
  [String] {- ^ remaining -} ->
  [String] {- ^ selections -}
pickWord dict remain = [x | x <- xs, x `elem` remain] <++ xs
  where
    xs = map snd
       $ head
       $ groupBy ((==) `on` fst)
       $ sortBy (comparing fst)
       $ [(metric remain w, w) | w <- dict]

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
     do putStr ('\r' : prettyWord [(Just Hit, x) | x <- take 5 ('*' <$ acc <|> repeat ' ')])
        hFlush stdout
        c <- toUpper <$> getChar
        case c of
          '\n'   | acc `elem` dict                    -> pure acc
          '\DEL' | not (null acc)                     -> go (init acc)
          _      | 'A' <= c, c <= 'Z', length acc < 5 -> go (acc ++ [c])
                 | otherwise                          -> go acc    

getWord :: Map Char Clue -> [String] -> [String] -> IO [Char]
getWord letters dict remain = go []
  where
    go acc =
     do putStr ('\r' : colorWord [(Blue, x) | x <- take 5 (acc ++ repeat ' ')]
                  ++ "    " ++
                  prettyWord [(Map.lookup x letters, x) | x <- ['A' .. 'Z']])
        hFlush stdout
        c <- toUpper <$> getChar
        case c of
          '\n'   | acc `elem` dict                    -> acc <$ clearLine
          '\DEL' | not (null acc)                     -> go (init acc)
          '?' | length remain > 1000 -> pure topHint
              | otherwise -> go =<< randomFromList (pickWord dict remain)
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
    nears = foldl' addLetter Map.empty (zip answer input)

    addLetter m (a,i)
      | a == i    = m 
      | otherwise = Map.insertWith (+) a (1::Int) m

    clue1 m (x,y)
      | x == y                          = (                   m, Hit )
      | Just n <- Map.lookup y m, n > 0 = (Map.insert y (n-1) m, Near)
      | otherwise                       = (                   m, Miss)

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
