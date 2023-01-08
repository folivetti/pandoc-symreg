module Main (main) where

import Options.Applicative
import Data.Char ( toLower )
import Data.List ( intercalate )
import PandocSR ( SRAlgs(..), parseSR, Output(..), sralgsHelp, outHelp )

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
        _     -> left $ "unknown algorithm. Available options are " <> intercalate "," sralgsHelp

data Args = Args
    {   from        :: SRAlgs
      , to          :: String
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
   <*> strOption
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
       <> help "Comma separated list of variables names. Empty list assumes the default of each algorithm (e.g, x0, x1)." )

main :: IO ()
main = do
  args <- execParser opts
  content <- lines <$> readFile (infile args)
  print args
  print $ parseSR (from args) (content !! 0)
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Convert different symbolic expressions format to common formats."
            <> header "pandoc-symreg - a pandoc-like CLI for symbolic regression expressions"
            )
