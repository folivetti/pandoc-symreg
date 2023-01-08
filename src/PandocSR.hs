{-# language OverloadedStrings #-}
module PandocSR
    ( parseHL 
    ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Number ( floating, sign, decimal )
import Control.Monad ( ap )
import Data.Functor.Identity ( Identity )
import Data.SRTree

type TextTyp   = String
type Parser    = Parsec TextTyp ()
type ParserSR  = Parser (SRTree Int Double)
type Op a      = Operator TextTyp () Identity a
type OpTable a = OperatorTable TextTyp () Identity a

binary :: TextTyp -> (a -> a -> a) -> Assoc -> Op a
binary  name fun = Infix (do{ string name; return fun })
prefix :: TextTyp -> (a -> a) -> Op a
prefix  name fun       = Prefix (do{ string name; return fun })
postfix :: TextTyp -> (a -> a) -> Op a
postfix name fun       = Postfix (do{ string name; return fun })

parens :: Parser a -> Parser a
parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

parseExpr :: OpTable (SRTree Int Double) -> ParserSR -> [(String, Int)] -> ParserSR
parseExpr table var header = expr
  where
    term  = parens expr <|> coef <|> varC <?> "term"
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
parseHL = parseExpr table var []
  where
    table = [ [prefix "sqr" (**2), prefix "cube" (**3), prefix "cbrt" (Fun Cbrt)]
            , [prefix "logabs" (log.abs), prefix "sqrtabs" (sqrt.abs), prefix "log" log, prefix "sqrt" sqrt, prefix "exp" exp, prefix "abs" abs, prefix "sin" sin, prefix "cos" cos]
            , [binary " * " (*) AssocLeft, binary " / " (/) AssocLeft]
            , [binary " + " (+) AssocLeft, binary " - " (-) AssocLeft]
            ]
    var = char 'x' >> Var <$> decimal <?> "var"
