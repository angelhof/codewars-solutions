module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> map (\(s', a) -> (s', f a)) $ unP p s

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a = pmap (\_ -> a) 

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P predP'
  where
    predP' [] = []
    predP' (c:cs) | p c = [(cs, c)]
                  | otherwise = []
    

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (\c' -> c' == c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> [(o', f x) | (o , f) <- unP pf s,
                                   (o', x) <- unP px o]

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P $ \s -> [(o', a) | (o , a) <- unP pa s,
                                (o', _) <- unP pb o]

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P $ \s -> [(o', b) | (o , _) <- unP pa s,
                                (o', b) <- unP pb o]

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = P $ \s -> [(s, "")]
stringP (c:cs) = (:) <#> charP c <@> stringP cs   

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ \_ -> []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) pa pb = P $ \s -> unP pa s ++ unP pb s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = (:) <#> p <@> many p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> some p  <<>> (\x -> [x]) <#> p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = 
  case filter (null . fst) (unP p cs) of
    [(_, a)] -> Just a
    _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE v) = v
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e) = - evalExpr e
evalExpr ZeroE = 0


-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique pExpr

pExpr :: Parser Expr
pExpr = pConst <<>> pBinOpExpr <<>> pNeg <<>> pZero

pConst :: Parser Expr
pConst = (ConstE . read) <#> some (predP isDigit)

pBinOpExpr :: Parser Expr
pBinOpExpr = makeExpr <#> (charP '(' @> pExpr <@ pSp) <@> pBinOp <@> (pSp @> pExpr <@ charP ')')
  where
    makeExpr e1 '+' e2 = BinOpE AddBO e1 e2
    makeExpr e1 '*' e2 = BinOpE MulBO e1 e2


pBinOp :: Parser Char
pBinOp = charP '+' <<>> charP '*'

pNeg :: Parser Expr
pNeg = NegE <#> (charP '-' @> pExpr)

pZero :: Parser Expr
pZero = ZeroE <# charP 'z'

pSp :: Parser Char
pSp = charP ' '
