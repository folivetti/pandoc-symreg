{-# language OverloadedStrings #-}
module PandocSR where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Data.SRTree
import Control.Applicative ( (<|>) )
import Data.Char ( toLower )

type ParseTree = Parser (SRTree Int Double)

data SRAlgs = TIR | HL | Bingo deriving (Show, Enum, Bounded)
data Output = Python | Math deriving (Show, Enum, Bounded)

envelope :: a -> [a] -> [a]
envelope c xs = c : xs <> [c]

sralgsHelp :: [String]
sralgsHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: SRAlgs ..]
outHelp :: [String]
outHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: Output ..]

binary :: B.ByteString -> (a -> a -> a) -> Assoc -> Operator B.ByteString a
binary name fun  = Infix (do{ string name; pure fun })

prefix :: B.ByteString -> (a -> a) -> Operator B.ByteString a
prefix  name fun = Prefix (do{ string name; pure fun })

parens :: Parser a -> Parser a
parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

parseExpr :: [[Operator B.ByteString (SRTree Int Double)]] -> ParseTree -> [(B.ByteString, Int)] -> ParseTree
parseExpr table var header = expr
  where
    term  = parens expr <|> coef <|> varC <?> "term"
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

parseHL :: [(B.ByteString, Int)] -> ParseTree
parseHL = parseExpr table var
  where
    table = [ [prefix "sqr" (**2), prefix "cube" (**3)]
            , [prefix "cbrt" (Fun Cbrt)]
            , [prefix "logabs" (log.abs), prefix "sqrtabs" (sqrt.abs), prefix "log" log, prefix "sqrt" sqrt, prefix "exp" exp, prefix "abs" abs, prefix "sin" sin, prefix "cos" cos]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = char 'x' >> Var <$> decimal <?> "var"

parseBingo :: [(B.ByteString, Int)] -> ParseTree
parseBingo = parseExpr table var
  where
    table = [ [prefix "sqr" (**2), prefix "cube" (**3)]
            , [prefix "cbrt" (Fun Cbrt)]
            , [prefix "log" log, prefix "exp" exp]
            , [prefix "abs" abs, prefix "sqrt" sqrt]
            , [prefix "sin" sin, prefix "cos" cos]
            , [binary "^" Power AssocLeft]
            , [binary "/" (/) AssocLeft, binary "" (*) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var  = string "X_" >> Var <$> decimal <?> "var"

enumerate :: [a] -> [(a, Int)]
enumerate = (`zip` [0..])

-- parseSR :: SRAlgs -> B.ByteString -> Result (SRTree Int Double)
parseSR HL = eitherResult . parse (parseHL $ enumerate ["r", "p_d", "epsilon", "x", "y", "z"]) . putEOL
parseSR _  = undefined

putEOL :: B.ByteString -> B.ByteString
putEOL bs | B.last bs == '\n' = bs
          | otherwise         = B.snoc bs '\n'
