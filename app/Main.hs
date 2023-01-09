{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.IO
import Options.Applicative
import Control.Monad ( unless, forM_ )
import qualified Data.ByteString.Char8 as B
import Data.Char ( toLower )
import Data.List ( intercalate )
import PandocSR ( SRAlgs(..), parseSR, Output(..), sralgsHelp, outHelp )
import Data.SRTree ( SRTree )
import qualified Data.SRTree.Print as P

left :: a -> String -> Either a b
left = pure . Left
right :: a1 -> String -> Either a2 a1
right = pure . Right

sralgsReader :: ReadM SRAlgs
sralgsReader = do
  sr <- str
  eitherReader $
      case map toLower sr of
        "tir" -> right TIR
        "hl"  -> right HL
        "bingo" -> right Bingo
        "operon" -> right Operon
        _     -> left $ "unknown algorithm. Available options are " <> intercalate "," sralgsHelp

outputsReader :: ReadM Output
outputsReader = do
  sr <- str
  eitherReader $
      case map toLower sr of
        "python" -> right Python
        "math"  -> right Math
        "tikz" -> right Tikz
        "latex" -> right Latex
        _     -> left $ "unknown output. Available options are " <> intercalate "," outHelp

data Args = Args
    {   from        :: SRAlgs
      , to          :: Output
      , infile      :: String
      , outfile     :: String
      , varnames    :: String
    } deriving Show

opt :: Parser Args
opt = Args
   <$> option sralgsReader
       ( long "from"
       <> short 'f'
       <> metavar ("[" <> intercalate "|" sralgsHelp <> "]")
       <> help "Input expression format" )
   <*> option outputsReader
       ( long "to"
       <> short 't'
       <> metavar ("[" <> intercalate "|" outHelp <> "]")
       <> help "Output expression format" )
   <*> strOption
       ( long "input"
       <> short 'i'
       <> metavar "INPUT"
       <> showDefault
       <> value ""
       <> help "Input file containing expressions. Empty string gets expression from stdin." )
   <*> strOption
       ( long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> showDefault
       <> value ""
       <> help "Output file to store expressions. Empty string prints expressions to stdout." )
   <*> strOption
       ( long "varnames"
       <> short 'v'
       <> metavar "VARNAMES"
       <> showDefault
       <> value ""
       <> help "Comma separated list of variables names. Empty list assumes the default of each algorithm (e.g, \"x,y,epsilon\")." )


withInput :: String -> SRAlgs -> String -> IO [Either String (SRTree Int Double)]
withInput fname sr hd = do
  h <- if null fname then pure stdin else openFile fname ReadMode
  contents <- hGetLines h 
  let myParser = parseSR sr (B.pack hd) . B.pack
      es = map myParser contents
  unless (null fname) $ hClose h
  pure es

withOutput :: String -> Output -> [Either String (SRTree Int Double)] -> IO ()
withOutput fname output exprs = do
  h <- if null fname then pure stdout else openFile fname WriteMode
  forM_ exprs $ \case 
                   Left  err -> hPutStrLn h $ "invalid expression: " <> err
                   Right ex  -> hPutStrLn h (showOutput output ex)
  unless (null fname) $ hClose h

showOutput :: Output -> SRTree Int Double -> String
showOutput Python = P.showPython
showOutput Math   = P.showDefault
showOutput Tikz   = P.showTikz
showOutput Latex   = P.showLatex

hGetLines :: Handle -> IO [String]
hGetLines h = do
  done <- hIsEOF h
  if done
    then return []
    else do
      line <- hGetLine h
      (line :) <$> hGetLines h

main :: IO ()
main = do
  args <- execParser opts
  content <- withInput (infile args) (from args) (varnames args)
  withOutput (outfile args) (to args) content
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Convert different symbolic expressions format to common formats."
            <> header "pandoc-symreg - a pandoc-like CLI for symbolic regression expressions"
            )
