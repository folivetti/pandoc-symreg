{-# language LambdaCase #-}
module Text.ParseSR.IO ( withInput, withOutput )
    where

import Control.Monad ( unless, forM_ )
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.SRTree
import Text.ParseSR ( SRAlgs, Output, parseSR, showOutput )

withInput :: String -> SRAlgs -> String -> Bool -> Bool -> IO [Either String (SRTree Int Double)]
withInput fname sr hd param simpl = do
  h <- if null fname then pure stdin else openFile fname ReadMode
  contents <- hGetLines h 
  let myParserFun = parseSR sr (B.pack hd) param . B.pack
      myParser = if simpl then fmap simplify . myParserFun else myParserFun
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

hGetLines :: Handle -> IO [String]
hGetLines h = do
  done <- hIsEOF h
  if done
    then return []
    else do
      line <- hGetLine h
      (line :) <$> hGetLines h
