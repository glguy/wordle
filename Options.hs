{-# Language DeriveTraversable, BlockArguments #-}
{- |
Module      : Options
Description : The whole program
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Options where

import Data.List (foldl')
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import System.Exit

data Options a = Options
  { optDictionary :: a
  , optWordlist   :: a
  , optStrategy   :: Strategy
  , optMode       :: Mode
  , optHard       :: Bool
  , optKeyboard   :: Keyboard
  }
  deriving (Read, Show, Eq, Ord, Foldable, Functor, Traversable)

data Keyboard = Qwerty | Dvorak | Colemak
  deriving (Read, Show, Eq, Ord)

defaultOpts :: Options FilePath
defaultOpts = Options
  { optDictionary = "all.txt"
  , optWordlist = "play.txt"
  , optStrategy = MostChoices
  , optMode = error "defaultOpts: mode not set"
  , optHard = False
  , optKeyboard = Qwerty
  }

data Strategy = WorstCase | MaxEntropy | SumOfSquares | MostChoices
  deriving (Read, Show, Eq, Ord)

data Mode = Play | Give | Solve [String]
  deriving (Read, Show, Eq, Ord)

optDescrs :: [OptDescr (Options FilePath -> Options FilePath)]
optDescrs =
  [ Option [] ["dict"] (ReqArg (\x o -> o { optDictionary = x }) "FILE") "Dictionary"
  , Option [] ["words"] (ReqArg (\x o -> o { optWordlist = x }) "FILE") "Word list"
  , Option [] ["worstcase"]    (NoArg \o -> o { optStrategy = WorstCase}) "Strategy: worst case"
  , Option [] ["maxentropy"]   (NoArg \o -> o { optStrategy = MaxEntropy}) "Strategy: maximum entropy"
  , Option [] ["sumofsquares"] (NoArg \o -> o { optStrategy = SumOfSquares}) "Strategy: sum of squares"
  , Option [] ["mostchoices"] (NoArg \o -> o { optStrategy = MostChoices}) "Strategy: most choices (default)"
  , Option [] ["easy"] (NoArg \o -> o { optHard = False}) "Disable hard mode (default)"
  , Option [] ["hard"] (NoArg \o -> o { optHard = True}) "Enable hard mode"
  , Option [] ["qwerty"]  (NoArg \o -> o { optKeyboard = Qwerty})  "Keyboard layout: qwerty (default)"
  , Option [] ["dvorak"]  (NoArg \o -> o { optKeyboard = Dvorak})  "Keyboard layout: dvorak"
  , Option [] ["colemak"] (NoArg \o -> o { optKeyboard = Colemak}) "Keyboard layout: colemak"
  ]

getOptions :: IO (Options [String])
getOptions =
 do args <- getArgs
    case getOpt Permute optDescrs args of
      (_, _, errs) | not (null errs) ->
        do mapM_ (hPutStrLn stderr) errs
           usage
      (fs, ms, _) ->
        do let opts = foldl' (\x f -> f x) defaultOpts fs
           opts' <- case ms of
             "solve":start -> pure opts { optMode = Solve start }
             ["play"]      -> pure opts { optMode = Play }
             ["give"]      -> pure opts { optMode = Give }
             _             -> usage
           traverse (fmap lines . readFile) opts'

usage :: IO a
usage =
 do hPutStr stderr (usageInfo header optDescrs)
    exitFailure
  where
    header =
      "Usage: wordle [FLAGS] MODE\n\
      \\n\
      \Modes:\n\
      \    solve - Program guesses a secret word, reply with 'b' 'y' 'g'\n\
      \    play  - Program picks a random word, type in your guess words\n\
      \    give  - User types in the secret words, then types in guess words\n"
