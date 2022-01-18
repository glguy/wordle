{-# Language BlockArguments, ViewPatterns, ImportQualifiedPost #-}
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.List
import System.IO
import System.Console.ANSI (hideCursor, showCursor, clearLine)
import System.Console.ANSI.Codes
import Control.Exception
import System.Random (randomRIO)
import Data.Ord
import Data.Map (Map)
import Data.Function
import Control.Monad
import Text.Printf
import System.Environment
import Data.Char

data Clue = Miss | Near | Hit deriving (Eq, Ord, Show)

guess :: String -> String -> [Clue]
guess answer input = snd (mapAccumL f nears (zip answer input))
  where
    nears = foldl' (\m (a,i) -> if a == i then m else Map.insertWith (+) a (1::Int) m) Map.empty (zip answer input)
    f m (x,y)
      | x == y = (m, Hit)
      | Just n <- Map.lookup y m = if n == 1 then (Map.delete y m, Near)
                                             else (Map.insert y (n-1) m, Near)
      | otherwise = (m, Miss)

metric :: [String] -> String -> Int
metric dict word = maximum classifications
  where
    classifications = Map.fromListWith (+) [(guess w word, 1) | w <- dict]

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

play :: IO ()
play =
 do d <- getDictionary
    p <- lines <$> readFile "play.txt"
    w <- randomFromList p
    playLoop d w Map.empty

give :: IO ()
give =
 do d <- getDictionary
    w <- getSecret d []
    playLoop d w Map.empty

playLoop :: [String] -> String -> Map.Map Char Clue -> IO ()
playLoop dict answer letters =
 do w <- getWord letters dict []
    let clue = guess answer w
    let letters' = Map.unionWith addHint letters (Map.fromListWith addHint (zip w clue))
    putStrLn ('\r' : prettyWord (zip (Just <$> clue) w))
    unless (clue == replicate 5 Hit)
      (playLoop dict answer letters')

addHint :: Clue -> Clue -> Clue
addHint Hit _ = Hit
addHint _ Hit = Hit
addHint Near _ = Near
addHint _ Near = Near
addHint _ _    = Miss

solver :: [String] -> IO ()
solver start =
 do allWords <- getDictionary
    driver (map (map toUpper) start <++ ["AESIR"]) allWords allWords

driver ::
  [String] ->
  [String] ->
  [String] ->
  IO ()

driver _ _ [] =
 do putStrLn (setSGRCode [SetColor Background Dull Red, SetColor Foreground Dull White] ++
              " E  R  R  O  R " ++
              setSGRCode [Reset])

driver _ _ [answer] =
 do putStrLn (prettyWord [(Just Hit, x) | x <- answer])

driver nexts dict remain =
 do (next, nexts') <- case nexts of
                        x:xs -> pure (x,xs)
                        []   -> do x <- randomFromList (pickWord dict remain)
                                   pure (x,[])
    answer <- getClue (length remain) next []
    putStrLn ""
    unless (answer == replicate 5 Hit)
      (driver nexts' dict (filter (\w -> guess w next == answer) remain))

pickWord :: [String] -> [String] -> [String]
pickWord dict remain = [x | x <- xs, x `elem` remain] <++ xs
  where
    xs = map snd
       $ head
       $ groupBy ((==) `on` fst)
       $ sortBy (comparing fst)
       $ [(metric remain w, w) | w <- dict]

randomFromList :: [a] -> IO a
randomFromList xs
  | null xs = fail "randomFromList: empty list"
  | otherwise =
     do i <- randomRIO (0, length xs - 1)
        pure $! xs !! i

(<++) :: [a] -> [a] -> [a]
[] <++ xs = xs
xs <++ _  = xs


prettyWord :: [(Maybe Clue, Char)] -> String
prettyWord ((x,y):xs) =
  setSGRCode [SetColor Background Dull color, SetColor Foreground Dull White] ++
  [' ',y,' '] ++
  prettyWord xs
  where
    color =
      case x of
        Just Hit  -> Green
        Just Miss -> Black
        Just Near -> Yellow
        Nothing   -> Blue
prettyWord _ = setSGRCode [Reset]

difficulty :: [String] -> String -> Int
difficulty dict answer = go 1 (learn "AESIR" dict)
  where
    learn v = filter (\w -> guess answer v == guess w v)
    go acc [_] = acc
    go acc xs = go (acc+1) (learn next xs)
      where
        next = head (sortOn (metric xs) dict)

-- * Input modes

getClue :: Int -> String -> [Clue] -> IO [Clue]
getClue n w acc =
 do putStr ('\r' : prettyWord (zip (map Just acc ++ replicate (5 - length acc) Nothing) w)
           ++ setSGRCode [Reset, SetColor Foreground Dull Black]
           ++ printf "  %5d" n)
    hFlush stdout
    input <- getChar
    case input of
      'g'    | length acc < 5  -> getClue n w (acc ++ [Hit])
      'b'    | length acc < 5  -> getClue n w (acc ++ [Miss])
      'y'    | length acc < 5  -> getClue n w (acc ++ [Near])
      '\DEL' | not (null acc)  -> getClue n w (init acc)
      '\n'   | length acc == 5 -> pure acc
      _                        -> getClue n w acc

getSecret :: [String] -> [Char] -> IO [Char]
getSecret dict acc =
 do putStr ('\r' : prettyWord [(Just Hit, x) | x <- take 5 (('*' <$ acc) ++ repeat ' ')])
    hFlush stdout
    c <- getChar
    case c of
      '\n' | acc `elem` dict -> pure acc
      '\DEL' | not (null acc) -> getSecret dict (init acc)
      _ | 'a' <= toLower c, toLower c <= 'z', length acc < 5 -> getSecret dict (acc ++ [toUpper c])
        | otherwise -> getSecret dict acc    

getWord :: Map Char Clue -> [String] -> [Char] -> IO [Char]
getWord letters dict acc =
 do putStr ('\r' : prettyWord [(Nothing, x) | x <- take 5 (acc ++ repeat ' ')]
            ++ "    " ++
            prettyWord [(Map.lookup x letters, x) | x <- ['A' .. 'Z']])
    hFlush stdout
    c <- getChar
    case c of
      '\n' | acc `elem` dict -> clearLine >> pure acc
      '\DEL' | not (null acc) -> getWord letters dict (init acc)
      _ | 'a' <= toLower c, toLower c <= 'z', length acc < 5 -> getWord letters dict (acc ++ [toUpper c])
        | otherwise -> getWord letters dict acc
