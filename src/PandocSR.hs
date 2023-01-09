{-# language OverloadedStrings #-}
module PandocSR ( parseSR, SRAlgs(..), Output(..), sralgsHelp, outHelp, intOrDouble ) 
    where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Data.Char ( toLower )
import Control.Monad.State ( State )
import qualified Control.Monad.State as ST

import Data.SRTree

type ParseTree = Parser (SRTree Int Double)

-- * Argument parsing helpers
data SRAlgs = TIR | HL | Operon | Bingo deriving (Show, Enum, Bounded)
data Output = Python | Math | Tikz | Latex deriving (Show, Enum, Bounded)

envelope :: a -> [a] -> [a]
envelope c xs = c : xs <> [c]

sralgsHelp :: [String]
sralgsHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: SRAlgs ..]
outHelp :: [String]
outHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: Output ..]

-- * Expression parser helpers

binary :: B.ByteString -> (a -> a -> a) -> Assoc -> Operator B.ByteString a
binary name fun  = Infix (do{ string name; pure fun })

prefix :: B.ByteString -> (a -> a) -> Operator B.ByteString a
prefix  name fun = Prefix (do{ string name; pure fun })

parens :: Parser a -> Parser a
parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

parseExpr :: [[Operator B.ByteString (SRTree Int Double)]] -> [ParseTree -> ParseTree] -> ParseTree -> Bool -> [(B.ByteString, Int)] -> ParseTree
parseExpr table binFuns var param header = relabelParams <$> expr
  where
    term  = parens expr <|> enclosedAbs expr <|> choice (map ($ expr) binFuns) <|> coef <|> varC <?> "term"
    expr  = buildExpressionParser table term
    coef = if param 
             then do eNumber <- intOrDouble
                     case eNumber of
                       Left x  -> pure $ Const (fromIntegral x)
                       Right _ -> pure $ Param 0
             else Const <$> signed double <?> "const"
    varC = if null header
             then var
             else var <|> varH
    varH = choice $ map (uncurry getParserVar) header
    getParserVar k v = do string k <|> enveloped k
                          pure $ Var v
    enveloped s = (char ' ' <|> char '(') >> string s >> (char ' ' <|> char ')') >> pure ""

relabelParams :: SRTree Int Double -> SRTree Int Double
relabelParams t = (toState t) `ST.evalState` 0
  where
    toState :: SRTree Int Double -> State Int (SRTree Int Double)
    toState (Param x) = do n <- ST.get; ST.put (n+1); pure (Param n)
    toState (Add l r) = do l' <- toState l; r' <- toState r; pure (Add l' r')
    toState (Sub l r) = do l' <- toState l; r' <- toState r; pure (Sub l' r')
    toState (Mul l r) = do l' <- toState l; r' <- toState r; pure (Mul l' r')
    toState (Div l r) = do l' <- toState l; r' <- toState r; pure (Div l' r')
    toState (Power l r) = do l' <- toState l; r' <- toState r; pure (Power l' r')
    toState (LogBase l r) = do l' <- toState l; r' <- toState r; pure (LogBase l' r')
    toState (Fun f n) = do n' <- toState n; pure (Fun f n')
    toState (Pow n i) = do n' <- toState n; pure (Pow n' i)
    toState n = pure n

enumerate :: [a] -> [(a, Int)]
enumerate = (`zip` [0..])

splitHeader :: B.ByteString -> [(B.ByteString, Int)]
splitHeader = enumerate . B.split ','

parseSR :: SRAlgs -> B.ByteString -> Bool -> B.ByteString -> Either String (SRTree Int Double)
parseSR HL header param = eitherResult . parse (parseHL param $ splitHeader header) . putEOL
parseSR Bingo header param = eitherResult . parse (parseBingo param $ splitHeader header) . putEOL
parseSR TIR header param = eitherResult . parse (parseTIR param $ splitHeader header) . putEOL
parseSR Operon header param = eitherResult . parse (parseOperon param $ splitHeader header) . putEOL

intOrDouble :: Parser (Either Int Double)
intOrDouble = eitherP parseInt (signed double)
  where
      parseInt :: Parser Int
      parseInt = do x <- signed decimal
                    c <- peekChar
                    case c of                      
                      Just '.' -> string "bogus" >> pure 0
                      Just 'e' -> string "bogus" >> pure 0
                      Just 'E' -> string "bogus" >> pure 0
                      _   -> pure x

putEOL :: B.ByteString -> B.ByteString
putEOL bs | B.last bs == '\n' = bs
          | otherwise         = B.snoc bs '\n'

-- * Special cases

-- analytic quotient
aq :: Floating a => a -> a -> a
aq x y = x / sqrt (1 + y ^ (2 :: Int))

-- | x | abs parser
enclosedAbs :: Num a => Parser a -> Parser a
enclosedAbs expr = do char '|'
                      e <- expr
                      char '|'
                      pure $ abs e

-- binary functions
binFun :: B.ByteString -> (a -> a -> a) -> Parser a -> Parser a
binFun name f expr = do string name
                        many' space >> char '(' >> many' space
                        e1 <- expr
                        many' space >> char ',' >> many' space -- many' space >> char ',' >> many' space
                        e2 <- expr
                        many' space >> char ')'
                        pure $ f e1 e2 
-- * Parsers
parseTIR :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseTIR = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ ]
    prefixOps = map (uncurry prefix)
                [   ("Id", id), ("Abs", abs)
                  , ("Sinh", sinh), ("Cosh", cosh), ("Tanh", tanh)
                  , ("Sin", sin), ("Cos", cos), ("Tan", tan)
                  , ("ASinh", asinh), ("ACosh", acosh), ("ATanh", atanh)
                  , ("ASin", asin), ("ACos", acos), ("ATan", atan)
                  , ("Sqrt", sqrt), ("Cbrt", (Fun Cbrt)), ("Square", (**2))
                  , ("Log", log), ("Exp", exp)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft]
            ]
    var = do char 'x'
             ix <- decimal
             pure $ Var ix
          <?> "var"

parseOperon :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseOperon = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ binFun "pow" Power ]
    prefixOps = map (uncurry prefix)
                [ ("abs", abs), ("cbrt", (Fun Cbrt))
                , ("acos", acos), ("cosh", cosh), ("cos", cos)
                , ("asin", asin), ("sinh", sinh), ("sin", sin)
                , ("exp", exp), ("log", log)
                , ("sqrt", sqrt), ("square", (**2))
                , ("atan", atan), ("tanh", tanh), ("tan", tan)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = do char 'X'
             ix <- decimal
             pure $ Var (ix - 1) -- Operon is not 0-based
          <?> "var"

parseHL :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseHL = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ binFun "aq" aq ]
    prefixOps = map (uncurry prefix)
                [ ("logabs", (log.abs)), ("sqrtabs", (sqrt.abs)) -- the longer versions should come first
                , ("abs", abs), ("exp", exp), ("log", log)
                , ("sqrt", sqrt), ("sqr", (**2)), ("cube", (**3))
                , ("cbrt", (Fun Cbrt)), ("sin", sin), ("cos", cos)
                , ("tan", tan), ("tanh", tanh) -- , ("aq", aq)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = do char 'x'
             ix <- decimal
             pure $ Var ix
          <?> "var"

parseBingo :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseBingo = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns = []
    prefixOps = map (uncurry prefix)
                [ ("abs", abs), ("exp", exp), ("log", (log.abs))
                , ("sqrt", (sqrt.abs))
                , ("sinh", sinh), ("cosh", cosh)
                , ("sin", sin), ("cos", cos)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary "/" (/) AssocLeft, binary "" (*) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = do string "X_"
             ix <- decimal
             pure $ Var ix
          <?> "var"
