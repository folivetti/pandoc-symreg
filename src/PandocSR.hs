{-# language OverloadedStrings #-}
module PandocSR ( parseSR, SRAlgs(..), Output(..), sralgsHelp, outHelp ) 
    where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Data.Char ( toLower )

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

parseExpr :: [[Operator B.ByteString (SRTree Int Double)]] -> [ParseTree -> ParseTree] -> ParseTree -> [(B.ByteString, Int)] -> ParseTree
parseExpr table binFuns var header = expr
  where
    term  = parens expr <|> enclosedAbs expr <|> choice (map ($ expr) binFuns) <|> coef <|> varC <?> "term"
    expr  = buildExpressionParser table term
    coef = Const <$> (constF <|> signed double) <?> "const"
    constF = do { x <- signed double; char 'f'; pure x }
    varC = if null header
             then var
             else var <|> varH
    varH = choice $ map (uncurry getParserVar) header
    getParserVar k v = do string k <|> enveloped k
                          pure $ Var v
    enveloped s = (char ' ' <|> char '(') >> string s >> (char ' ' <|> char ')') >> pure ""

enumerate :: [a] -> [(a, Int)]
enumerate = (`zip` [0..])

splitHeader :: B.ByteString -> [(B.ByteString, Int)]
splitHeader = enumerate . B.split ','

-- parseSR :: SRAlgs -> B.ByteString -> Result (SRTree Int Double)
parseSR :: SRAlgs -> B.ByteString -> B.ByteString -> Either String (SRTree Int Double)
parseSR HL header    = eitherResult . parse (parseHL $ splitHeader header) . putEOL
parseSR Bingo header = eitherResult . parse (parseBingo $ splitHeader header) . putEOL
parseSR TIR header = eitherResult . parse (parseTIR $ splitHeader header) . putEOL
parseSR Operon header = eitherResult . parse (parseOperon $ splitHeader header) . putEOL

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
parseTIR :: [(B.ByteString, Int)] -> ParseTree
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

parseOperon :: [(B.ByteString, Int)] -> ParseTree
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

parseHL :: [(B.ByteString, Int)] -> ParseTree
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

parseBingo :: [(B.ByteString, Int)] -> ParseTree
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
