module Main (main) where

import Options.Applicative

import Data.Char ( toLower, toUpper )
import Text.Read ( readMaybe )
import Data.List ( intercalate )
import Text.ParseSR ( SRAlgs(..), Output(..) )
import Text.ParseSR.IO ( withInput, withOutput )

envelope :: a -> [a] -> [a]
envelope c xs = c : xs <> [c]

sralgsHelp :: [String]
sralgsHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: SRAlgs ..]

outHelp :: [String]
outHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: Output ..]

sralgsReader :: ReadM SRAlgs
sralgsReader = do
  sr <- map toUpper <$> str
  eitherReader $ case readMaybe sr of
    Nothing -> pure . Left $ "unknown algorithm. Available options are " <> intercalate "," sralgsHelp
    Just x  -> pure . Right $ x

outputsReader :: ReadM Output
outputsReader = do
  sr <- map toUpper <$> str
  eitherReader $ case readMaybe sr of
    Nothing -> pure . Left $ "unknown output. Available options are " <> intercalate "," outHelp
    Just x  -> pure . Right $ x

data Args = Args
    {   from        :: SRAlgs
      , to          :: Output
      , infile      :: String
      , outfile     :: String
      , varnames    :: String
      , parameters  :: Bool
      , simpl       :: Bool
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
    <*> switch
        ( long "parameters"
        <> short 'p'
        <> help "Convert floating point numbers to free parameters." )
    <*> switch
        ( long "simplify"
        <> help "Apply basic simplification." )

main :: IO ()
main = do
  args <- execParser opts
  withInput (infile args) (from args) (varnames args) (parameters args) (simpl args)
    >>= withOutput (outfile args) (to args) 
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Convert different symbolic expressions format to common formats."
            <> header "pandoc-symreg - a pandoc-like CLI for symbolic regression expressions"
            )
