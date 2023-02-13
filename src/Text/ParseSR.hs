{-# language OverloadedStrings #-}
module Text.ParseSR ( parseSR, showOutput, SRAlgs(..), Output(..) ) 
    where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import qualified Data.SRTree.Print as P
import Data.List ( sortOn )

import Debug.Trace ( trace )
import Data.SRTree

-- * Data types

-- | Parser of a symbolic regression tree with `Int` variable index and
-- numerical values represented as `Double`. The numerical values type
-- can be changed with `fmap`.
type ParseTree = Parser (SRTree Int Double)

-- * Data types and caller functions

-- | Supported algorithms.
data SRAlgs = TIR | HL | OPERON | BINGO | GOMEA | PYSR deriving (Show, Read, Enum, Bounded)

-- | Supported outputs.
data Output = PYTHON | MATH | TIKZ | LATEX deriving (Show, Read, Enum, Bounded)

-- | Returns the corresponding function from Data.SRTree.Print for a given `Output`.
showOutput :: Output -> SRTree Int Double -> String
showOutput PYTHON = P.showPython
showOutput MATH   = P.showDefault
showOutput TIKZ   = P.showTikz
showOutput LATEX  = P.showLatex

-- | Calls the corresponding parser for a given `SRAlgs`
parseSR :: SRAlgs -> B.ByteString -> Bool -> B.ByteString -> Either String (SRTree Int Double)
parseSR HL     header param = eitherResult . (`feed` "") . parse (parseHL param $ splitHeader header) . putEOL
parseSR BINGO  header param = eitherResult . (`feed` "") . parse (parseBingo param $ splitHeader header) . putEOL
parseSR TIR    header param = eitherResult . (`feed` "") . parse (parseTIR param $ splitHeader header) . putEOL
parseSR OPERON header param = eitherResult . (`feed` "") . parse (parseOperon param $ splitHeader header) . putEOL
parseSR GOMEA  header param = eitherResult . (`feed` "") . parse (parseGOMEA param $ splitHeader header) . putEOL
parseSR PYSR   header param = eitherResult . (`feed` "") . parse (parsePySR param $ splitHeader header) . putEOL

eitherResult' :: Show r => Result r -> Either String r
eitherResult' res = trace (show res) $ eitherResult res

-- * Parsers

-- | Creates a parser for a binary operator
binary :: B.ByteString -> (a -> a -> a) -> Assoc -> Operator B.ByteString a
binary name fun  = Infix (do{ string name; pure fun })

-- | Creates a parser for a unary function
prefix :: B.ByteString -> (a -> a) -> Operator B.ByteString a
prefix  name fun = Prefix (do{ string name; pure fun })

-- | Envelopes the parser in parens
parens :: Parser a -> Parser a
parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

-- | Parse an expression using a user-defined parser given by the `Operator` lists containing
-- the name of the functions and operators of that SR algorithm, a list of parsers `binFuns` for binary functions
-- a parser `var` for variables, a boolean indicating whether to change floating point values to free
-- parameters variables, and a list of variable names with their corresponding indexes.
parseExpr :: [[Operator B.ByteString (SRTree Int Double)]] -> [ParseTree -> ParseTree] -> ParseTree -> Bool -> [(B.ByteString, Int)] -> ParseTree
parseExpr table binFuns var param header = do e <- relabelParams <$> expr
                                              many1' space
                                              pure e
  where
    term  = parens expr <|> enclosedAbs expr <|> choice (map ($ expr) binFuns) <|> coef <|> varC <?> "term"
    expr  = buildExpressionParser table term
    coef  = if param 
              then do eNumber <- intOrDouble
                      case eNumber of
                        Left x  -> pure $ Const (fromIntegral x)
                        Right _ -> pure $ Param 0
              else Const <$> signed double <?> "const"
    varC = if null header
             then var
             else var <|> varHeader

    varHeader        = choice $ map (uncurry getParserVar) $ sortOn (B.length . fst) header
    getParserVar k v = (string k <|> enveloped k) >> pure (Var v)
    enveloped s      = (char ' ' <|> char '(') >> string s >> (char ' ' <|> char ')') >> pure ""

enumerate :: [a] -> [(a, Int)]
enumerate = (`zip` [0..])

splitHeader :: B.ByteString -> [(B.ByteString, Int)]
splitHeader = enumerate . B.split ','

-- | Tries to parse as an `Int`, if it fails, 
-- parse as a Double.
intOrDouble :: Parser (Either Int Double)
intOrDouble = eitherP parseInt (signed double)
  where
      parseInt :: Parser Int
      parseInt = do x <- signed decimal
                    c <- peekChar
                    case c of                      
                      Just '.' -> digit >> pure 0
                      Just 'e' -> digit >> pure 0
                      Just 'E' -> digit >> pure 0
                      _   -> pure x

putEOL :: B.ByteString -> B.ByteString
putEOL bs | B.last bs == '\n' = bs
          | otherwise         = B.snoc bs '\n'

-- * Special case functions

-- | analytic quotient
aq :: SRTree Int Double -> SRTree Int Double -> SRTree Int Double
aq x y = x / sqrt (1 + y ** 2)

log1p :: SRTree Int Double -> SRTree Int Double
log1p x = log (1 + x)

-- Parse `abs` functions as | x |
enclosedAbs :: Num a => Parser a -> Parser a
enclosedAbs expr = do char '|'
                      e <- expr
                      char '|'
                      pure $ abs e

-- | Parser for binary functions
binFun :: B.ByteString -> (a -> a -> a) -> Parser a -> Parser a
binFun name f expr = do string name
                        many' space >> char '(' >> many' space
                        e1 <- expr
                        many' space >> char ',' >> many' space -- many' space >> char ',' >> many' space
                        e2 <- expr
                        many' space >> char ')'
                        pure $ f e1 e2 

-- * Custom parsers for SR algorithms

-- | parser for Transformation-Interaction-Rational.
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
                  , ("Sqrt", sqrt), ("Cbrt", Fun Cbrt), ("Square", (**2))
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

-- | parser for Operon.
parseOperon :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseOperon = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ binFun "pow" Power ]
    prefixOps = map (uncurry prefix)
                [ ("abs", abs), ("cbrt", Fun Cbrt)
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

-- | parser for HeuristicLab.
parseHL :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseHL = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ binFun "aq" aq ]
    prefixOps = map (uncurry prefix)
                [ ("logabs", log.abs), ("sqrtabs", sqrt.abs) -- the longer versions should come first
                , ("abs", abs), ("exp", exp), ("log", log)
                , ("sqrt", sqrt), ("sqr", (**2)), ("cube", (**3))
                , ("cbrt", Fun Cbrt), ("sin", sin), ("cos", cos)
                , ("tan", tan), ("tanh", tanh)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = do char 'x'
             ix <- decimal
             pure $ Var ix
          <?> "var"

-- | parser for Bingo
parseBingo :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseBingo = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns = []
    prefixOps = map (uncurry prefix)
                [ ("abs", abs), ("exp", exp), ("log", log.abs)
                , ("sqrt", sqrt.abs)
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

-- | parser for GOMEA
parseGOMEA :: Bool -> [(B.ByteString, Int)] -> ParseTree
parseGOMEA = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns = []
    prefixOps = map (uncurry prefix)
                [ ("exp", exp), ("plog", log.abs)
                , ("sqrt", sqrt.abs)
                , ("sin", sin), ("cos", cos)
                ]
    binOps = [[binary "^" Power AssocLeft]
            , [binary "/" (/) AssocLeft, binary "*" (*) AssocLeft, binary "aq" aq AssocLeft]
            , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
            ]
    var = do string "x"
             ix <- decimal
             pure $ Var ix
          <?> "var"

-- | parser for PySR
parsePySR :: Bool -> [(B.ByteString, Int)] -> ParseTree
parsePySR = parseExpr (prefixOps : binOps) binFuns var
  where
    binFuns   = [ binFun "pow" Power ]
    prefixOps = map (uncurry prefix)
                [ ("abs", abs), ("exp", exp)
                , ("square", (**2)), ("cube", (**3)), ("neg", negate)
                , ("acosh_abs", acosh . (+1) . abs), ("acosh", acosh), ("asinh", asinh)
                , ("acos", acos), ("asin", asin), ("atan", atan)
                , ("sqrt_abs", sqrt.abs), ("sqrt", sqrt)
                , ("sinh", sinh), ("cosh", cosh), ("tanh", tanh)
                , ("sin", sin), ("cos", cos), ("tan", tan)
                , ("log10", (`LogBase` 10)), ("log2", (`LogBase` 2)), ("log1p", log1p) 
                , ("log_abs", log.abs), ("log10_abs", (`LogBase` 10) . abs)
                , ("log", log)
                ]
    binOps = [[binary " ^ " Power AssocLeft]
            , [binary " / " (/) AssocLeft, binary " * " (*) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = do string "x"
             ix <- decimal
             pure $ Var ix
          <?> "var"