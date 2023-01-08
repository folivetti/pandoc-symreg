{-# language OverloadedStrings #-}
module PandocSR where

import Text.Parsec
import Text.Parsec.Expr ( buildExpressionParser, Assoc(..), Operator(..), OperatorTable )
import Text.Parsec.Token hiding ( decimal )
import Text.Parsec.Language ( emptyDef )
import Text.ParserCombinators.Parsec.Number ( floating, sign, decimal )
import Control.Monad ( ap )
import Data.Char ( toLower )
import Data.Functor.Identity ( Identity )
import Data.SRTree

type TextTyp   = String
type Parser    = Parsec TextTyp ()
type ParserSR  = Parser (SRTree Int Double)
type Op a      = Operator TextTyp () Identity a
type OpTable a = OperatorTable TextTyp () Identity a

data SRAlgs = TIR | HL | Bingo deriving (Show, Enum, Bounded)
data Output = Python | Math deriving (Show, Enum, Bounded)

lexer :: TokenParser ()
lexer = makeTokenParser emptyDef{ reservedOpNames = [" + ", " * ", " - ", " / "] }

envelope :: a -> [a] -> [a]
envelope c xs = c : xs <> [c]

sralgsHelp :: [String]
sralgsHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: SRAlgs ..]
outHelp :: [String]
outHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: Output ..]


binary :: TextTyp -> (a -> a -> a) -> Assoc -> Op a
binary  name fun = Infix (do{ reservedOp lexer name; return fun })
prefix :: TextTyp -> (a -> a) -> Op a
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })
postfix :: TextTyp -> (a -> a) -> Op a
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })

--parens :: Parser a -> Parser a
--parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

parseExpr :: OpTable (SRTree Int Double) -> ParserSR -> [(String, Int)] -> ParserSR
parseExpr table var header = do 
    whiteSpace lexer
    ex <- expr
    eof
    pure ex
  where
    term  = parens lexer expr <|> coef <|> varC <?> "term"
    expr  = buildExpressionParser table term
    coef = Const <$> ap sign floating <?> "const"
    varC = if null header
             then var
             else var <|> varH
    varH = choice $ map (uncurry getParserVar) header
    getParserVar :: TextTyp -> Int -> ParserSR
    getParserVar k v = do string k <|> enveloped k
                          pure $ Var v
    enveloped :: TextTyp -> Parser TextTyp
    enveloped s = (char ' ' <|> char '(') >> string s >> (char ' ' <|> char ')') >> pure ""

parseHL :: ParserSR
parseHL = parseExpr table var [("r",0), ("p_d", 1), ("x", 2), ("epsilon", 3), ("y", 4), ("z", 5)]
  where
    table = [ [prefix "logabs" (log.abs), prefix "sqrtabs" (sqrt.abs), prefix "log" log, prefix "sqrt" sqrt, prefix "exp" exp, prefix "abs" abs, prefix "sin" sin, prefix "cos" cos, prefix "sqr" (**2), prefix "cube" (**3), prefix "cbrt" (Fun Cbrt)]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocRight]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = char 'x' >> Var <$> decimal <?> "var"

parseSR :: SRAlgs -> TextTyp -> Either ParseError (SRTree Int Double)
parseSR HL = runParser parseHL () "HeuristicLab" 
parseSR _  = undefined
