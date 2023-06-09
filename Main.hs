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
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Ord (comparing)
import System.Console.ANSI
import System.Exit
import System.IO
import System.Random (randomRIO)
import Text.Printf (printf)

import Options

topHint :: String
topHint = "RAISE"

data UI = UI Keyboard [[(Color,Char)]] (Map Char Clue)

printUI :: UI -> IO ()
printUI (UI keyboard history letters) =
 do clearScreen
    setCursorPosition 0 0
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Magenta]
    printLetters keyboard letters
    putStrLn "[ W O R D L E ]"
    for_ history (putStrLn . colorWord)

main :: IO ()
main =
 do opts <- getOptions
    let ui = UI (optKeyboard opts) [] Map.empty
    case optMode opts of
      Solve -> withoutCursor ui (solver ui opts)
      Give  -> withoutCursor ui (give   ui opts)
      Play  -> withoutCursor ui (play   ui opts)

withoutCursor :: UI -> IO c -> IO c
withoutCursor ui m =
  bracket_ hideCursor showCursor
 do hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    printUI ui
    m

-- | Play wordle with a randomly-selected word
play :: UI -> Options [String] -> IO ()
play ui opts =
 do answers <-
     if optAdversarial opts
       then pure (optWordlist opts)
       else pure <$> randomFromList (optWordlist opts)
    case prepStart opts of
      Right start -> playLoop ui opts (prepRemain opts) answers start Nothing
      Left bad -> hPutStrLn stderr ("Invalid word: " ++ bad) >> exitFailure

prepRemain :: Options [String] -> [String]
prepRemain opts
  | optCheat opts = optWordlist opts
  | otherwise     = optDictionary opts

prepStart :: Options [String] -> Either String [String]
prepStart opts = traverse check (optStart opts)
  where
    check x
      | x' `elem` optDictionary opts = Right x'
      | otherwise                    = Left x
      where
        x' = map toUpper x

-- | Play wordle with a manually-selected word
give :: UI -> Options [String] -> IO ()
give ui opts =
  case prepStart opts of
    Left bad -> hPutStrLn stderr ("Invalid word: " ++ bad) >> exitFailure
    Right start ->
     do (w,ws) <-
          case start of
            w:ws -> pure (w,ws)
            []   -> do w <- getSecret ui (optDictionary opts)
                       pure (w,[])
        playLoop ui opts (prepRemain opts) [w] ws Nothing

playLoop :: UI -> Options [String] -> [String] -> [String] -> [String] -> Maybe [(Clue, Char)] -> IO ()
playLoop ui opts remain answers start prev =
 do printUI ui
    (w,ws) <-
      case start of
        w:ws -> pure (w,ws)
        []   -> do w <- getWord ui opts prev remain
                   pure (w,[])
    clue <- chooseClue answers w
    let answers' = [a  | a <- answers, computeClues a w == clue]
    let remain' = filter (\x -> computeClues x w == clue) remain
    let ui' = updateUI clue w ui
    putStrLn ""
    if clue == replicate 5 Hit then
      printUI ui'
    else
      playLoop ui' opts remain' answers' ws (Just (zip clue w))

-- | Use the metric computation to have the computer make guesses.
-- The user must provide the clue responses. The solver will use
-- the given guess-list to start and then switch to metrics when
-- the list is exhausted.
solver ::
  UI ->
  Options [String] ->
  IO ()
solver ui opts =
  solverLoop
    ui
    opts
    (map (map toUpper) (optStart opts) <++ [topHint])
    Nothing
    (prepRemain opts)

solverLoop ::
  UI ->
  Options [String] ->
  [String] ->
  Maybe [(Clue, Char)] ->
  [String] ->
  IO ()

solverLoop _ _ _ _ [] =
 do putStrLn (colorWord [(Red,x) | x <- "ERROR"])

solverLoop _ _ _ _ [answer] =
 do putStrLn (prettyWord [(Just Hit, x) | x <- answer])

solverLoop ui opts nexts prev remain =
 do printUI ui
    (next, nexts') <- case nexts of
                        x:xs -> pure (x,xs)
                        []   -> do let d | optHard opts, Just c <- prev = filter (hardCheck [] [] c) (optDictionary opts)
                                         | otherwise = optDictionary opts
                                   x <- randomFromList (pickWord (optStrategy opts) d remain)
                                   pure (x,[])
    clue <- getClue ui remain next
    let ui' = updateUI clue next ui
    putStrLn ""
    unless (clue == replicate 5 Hit)
      (solverLoop ui' opts nexts' (Just (zip clue next))
         (filter (\w -> computeClues w next == clue) remain))

updateUI :: [Clue] -> String -> UI -> UI
updateUI clues word (UI k h l) =
  UI k
    (h ++ [[(clueColor c, x) | (c,x) <- zip clues word]])
    (Map.unionWith max l (Map.fromListWith max (zip word clues)))

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

negEntropy :: Foldable f => f Int -> Double
negEntropy ns = w / denom - log denom
  where
    M denom w = foldl' (\(M a b) x -> let x' = fromIntegral x in M (a+x') (b+ x'*log x')) (M 0 0) ns

data M = M !Double !Double

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

getClue :: UI -> [String] -> String -> IO [Clue]
getClue ui remain w = go []
  where
    go acc =
     do putStr ('\r' : prettyWord (zip (map Just acc ++ replicate (5 - length acc) Nothing) w)
              ++ setSGRCode [Reset, SetColor Foreground Dull Black]
              ++ printf "  %5d" (length remain))
        hFlush stdout
        input <- getChar
        case input of
          '\^L' -> printUI ui >> go acc
          '$'   -> print remain >> go acc
          'g'    | length acc < 5  -> go (acc ++ [Hit])
          'b'    | length acc < 5  -> go (acc ++ [Miss])
          'y'    | length acc < 5  -> go (acc ++ [Near])
          '\DEL' | not (null acc)  -> go (init acc)
          '\n'   | length acc == 5 -> pure acc
          _                        -> go acc

getSecret :: UI -> [String] -> IO [Char]
getSecret ui dict = go []
  where
    go acc =
     do putStr ('\r' : prettyWord [(Just Hit, x) | x <- take 5 ('◆' <$ acc <|> repeat '·')])
        hFlush stdout
        c <- toUpper <$> getChar
        case c of
          '\^L' -> printUI ui >> go acc
          '\n'   | acc `elem` dict                    -> pure acc
          '\DEL' | not (null acc)                     -> go (init acc)
          _      | 'A' <= c, c <= 'Z', length acc < 5 -> go (acc ++ [c])
                 | otherwise                          -> go acc

keyboardLayout :: Keyboard -> (String, String, String)
keyboardLayout Qwerty  = ("QWERTYUIOP", "ASDFGHJKL", "ZXCVBNM")
keyboardLayout Dvorak  = ("   PYFGCRL", "AOEUIDHTNS", " QJKXBMWVZ")
keyboardLayout Colemak = ("QWFPGJLUY", "ARSTDHNEIO", "ZXCVBKM")
keyboardLayout Alphabet= ("ABCDEFGHI", "JKLMNOPQR", "RSTUVWXY")
keyboardLayout Frequencies= ("SEAORILTN", "UDYCPMHGB", "KFWVZJXQ")

printLetters :: Keyboard -> Map Char Clue -> IO ()
printLetters layout letters =
 do saveCursor
    setCursorPosition 0 20
    row r1
    setCursorPosition 1 22
    row r2
    setCursorPosition 2 24
    row r3
    restoreCursor
  where
    (r1,r2,r3) = keyboardLayout layout
    row = putStr . unwords . map draw
    draw ' ' = "   "
    draw x = prettyWord [(Map.lookup x letters, x)]

getWord ::
  UI ->
  Options [String] ->
  Maybe [(Clue, Char)] {- ^ previous response -} ->
  [String] {- ^ remaining possible words -} ->
  IO [Char]
getWord ui opts prev remain = go []
  where
    dict = optDictionary opts
    dict'
      | optHard opts, Just c <- prev = filter (hardCheck [] [] c) dict
      | otherwise = dict
    check w
      | optHard opts, Just c <- prev = hardCheck [] [] c w
      | otherwise = True
    go acc =
     do draw (colorWord [(Blue, x) | x <- take 5 (acc ++ repeat '·')])
        getLoop acc

    draw str =
     do putStr ('\r' : str)
        hFlush stdout

    getLoop acc =
     do c <- toUpper <$> getChar
        case c of
          '$' -> print remain >> go acc
          '\^L' -> printUI ui >> go acc
          '\n'   | acc `elem` dict, check acc         -> pure acc
                 | otherwise -> do draw (colorWord [(Red, x) | x <- take 5 (acc ++ repeat '·')])
                                   getLoop acc
          '\DEL' | not (null acc)                     -> go (init acc)

          '?' | length remain > 1000                  -> go topHint
              | otherwise ->
                 do let clues = pickWord (optStrategy opts) dict' remain
                    case [y | length acc == 5, (x,y) <- zip clues (tail clues ++ [head clues]), x == acc] of
                      c:_ -> go c
                      []  -> go =<< randomFromList clues

          _      | 'A' <= c, c <= 'Z', length acc < 5 -> go (acc ++ [c])
                 | otherwise                          -> go acc

hardCheck :: String -> String -> [(Clue, Char)] -> String -> Bool
hardCheck n u ((Hit, x):xs) (y:ys) = x == y && hardCheck n u xs ys
hardCheck n u ((Near,x):xs) (y:ys) = hardCheck (x:n) (y:u) xs ys
hardCheck n u ((Miss,_):xs) (y:ys) = hardCheck n (y:u) xs ys
hardCheck n u _ _ = null (n \\ u)

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

chooseClue :: [String] -> String -> IO [Clue]
chooseClue answers w
  | null clueOptions = pure win
  | otherwise = randomFromList clueOptions
  where
    win = replicate 5 Hit
    clueOptions = [computeClues a w | a <- answers, let c = computeClues a w, c /= win]

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
