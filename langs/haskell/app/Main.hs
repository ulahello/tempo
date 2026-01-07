-- TODO: performance considerations for lazy execution?
-- are we needlessly accruing thunks?

module Main (main) where

import Control.Exception
import Control.Monad (guard, when)
import Data.Char
import Data.Foldable
import Data.Maybe
import System.IO
import System.IO.Error (isEOFError)
import Tapper
import Text.Printf
import Text.Read

tempoName :: String
tempoName = "tempo"

tempoVersion :: String
tempoVersion = "0.1.0"

tempoDescription :: String
tempoDescription = "terminal tempo tapper"

defaultCapacity :: Int
defaultCapacity = 10

defaultBounded :: Bool
defaultBounded = True

data Command
  = Help
  | Tap
  | Clear
  | Size
  | Bound
  | Print
  | Quit

commands :: [Command]
commands =
  [ Help,
    Tap,
    Clear,
    Size,
    Bound,
    Print,
    Quit
  ]

commandLiteral :: Command -> String
commandLiteral c = case c of
  Help -> "h"
  Tap -> ""
  Clear -> "c"
  Size -> "s"
  Bound -> "b"
  Print -> "p"
  Quit -> "q"

commandShortName :: Command -> String
commandShortName Tap = "<enter>"
commandShortName c = commandLiteral c

commandLongName :: Command -> String
commandLongName c = case c of
  Help -> "help"
  Tap -> "tap"
  Clear -> "clear"
  Size -> "size"
  Bound -> "bound"
  Print -> "print"
  Quit -> "quit"

commandDescription :: Command -> String
commandDescription c = case c of
  Help -> "describe commands"
  Tap -> "register a beat"
  Clear -> "clear buffer contents"
  Size -> "adjust buffer size"
  Bound -> "toggle whether buffer is bounded to size"
  Print -> "print buffer contents in order from newest to oldest"
  Quit -> "quit"

parseCommand :: String -> Maybe Command
parseCommand s =
  find
    (\c -> eqCase s (commandLiteral c) || eqCase s (commandLongName c))
    commands
  where
    eqCase a b = map toLower a == map toLower b

printSplash :: IO ()
printSplash = do
  printf "%s %s: %s\n" tempoName tempoVersion tempoDescription
  putStrLn "type \"h\" for help"

printPrompt :: Tapper -> IO ()
printPrompt t = do
  printf
    "%d/%d%s samples in buffer\n"
    (Tapper.count t)
    (Tapper.capacity t)
    (if Tapper.isBounded t then "" else "+")
  printf "%s BPM\n" (show (Tapper.bpm t))

printHelp :: [Command] -> IO ()
printHelp = mapM_ describeCommand
  where
    describeCommand c = do
      printf " %s or %s. %s.\n" (commandLongName c) (commandShortName c) (commandDescription c)

getLine' :: String -> IO (Maybe String)
getLine' p = do
  putStr p
  hFlush stdout
  line <- tryJust (guard . isEOFError) getLine
  case line of
    Left _ ->
      return Nothing
    Right s -> do
      let s' = strip s
      return (Just s')
  where
    lstrip = dropWhile isSpace
    rstrip s = reverse (lstrip (reverse s))
    strip = lstrip . rstrip

handleResize :: Tapper -> Int -> IO Tapper
handleResize t cap = do
  let t' = Tapper.resize t cap
  let reported = Tapper.capacity t'
  when (reported /= cap) $
    printf
      " size too %s, clamped to %d\n"
      (if reported < cap then "large" else "small")
      reported
  return t'

interactiveResize :: Tapper -> IO Tapper
interactiveResize t = do
  putStrLn ""
  input <- getLine' " new buffer size? "
  let input' = fromMaybe "" input
  parsedCap <- case (input', readMaybe input' :: Maybe Int) of
    ("", _) ->
      return Nothing
    (_, Just n) ->
      return (Just n)
    (_, Nothing) -> do
      -- TODO: not sure if it overflowed??
      putStrLn " invalid integer"
      return Nothing
  maybe (return t) (handleResize t) parsedCap

handleCommand :: Tapper -> Command -> IO (Maybe Tapper)
handleCommand t c = case c of
  Help -> do
    putStrLn ""
    printHelp commands
    return (Just t)
  Tap -> do
    t' <- Tapper.tap t
    return (Just t')
  Clear -> do
    let t' = Tapper.clear t
    return (Just t')
  Size -> do
    t' <- interactiveResize t
    return (Just t')
  Bound -> do
    let t' = Tapper.toggleBounded t
    return (Just t')
  Print -> do
    putStrLn ""
    printf " %s\n" (show t)
    return (Just t)
  Quit -> do
    putStrLn ""
    putStrLn " goodbye"
    putStrLn ""
    return Nothing

repl :: Tapper -> IO ()
repl t = do
  putStrLn ""
  printPrompt t
  cmdStr <- getLine' (if Tapper.isRecording t then " * " else " ; ")
  let cmd = maybe (Just Quit) parseCommand cmdStr
  t' <- case cmd of
    Just c -> handleCommand t c
    Nothing -> do
      putStrLn ""
      putStrLn " unrecognized command. try \"h\" for help."
      return (Just t)
  maybe (return ()) repl t'

main :: IO ()
main = do
  let t = Tapper.create defaultCapacity defaultBounded
  printSplash
  repl t
