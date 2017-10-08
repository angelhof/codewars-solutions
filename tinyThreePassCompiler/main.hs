module TinyThreePassCompiler where

import qualified Data.Map as M
import Debug.Trace


data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         | Op Char
         | ArgS String
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1


------------------------------------
--
-- Pass 1
--
------------------------------------

pass1 :: String -> AST
pass1 = parseFun . tokenize

type ArgMap = M.Map String Int

parseFun :: [Token] -> AST
parseFun tokens = expression'
  where
    (TChar '['):rest = tokens
    (args, rest') = parseArgs rest
    (TChar ']'):rest'' = rest'
    expression = state 0 [] rest''
    expression' = replaceArgs expression args

parseArgs :: [Token] -> (ArgMap, [Token])
parseArgs tokens = (M.fromList $ zip args [0..], rest)
  where
    (args, rest) = parseArgs tokens []
    parseArgs (TStr var:rest) acc = parseArgs rest (var:acc)
    parseArgs rest acc = (reverse acc, rest)

replaceArgs (Add e1 e2) args = (Add (replaceArgs e1 args) (replaceArgs e2 args))
replaceArgs (Sub e1 e2) args = (Sub (replaceArgs e1 args) (replaceArgs e2 args))
replaceArgs (Mul e1 e2) args = (Mul (replaceArgs e1 args) (replaceArgs e2 args))
replaceArgs (Div e1 e2) args = (Div (replaceArgs e1 args) (replaceArgs e2 args))
replaceArgs (ArgS argS) args = Arg $ args M.! argS
replaceArgs ast args = ast

--
-- Bottom-Up parser states
--

state :: Int -> [AST] -> [Token] -> AST
state 0 stack (t:rest)
  | isFactor t = state 1 (makeF t:stack) rest
  | isLpar t = state 0 (makeOp t:stack) rest
state 1 stack tokens = state 2 stack tokens
state 2 stack [] = state 3 stack []
state 2 stack (t:rest)
  | isMulDiv t = state 4 (makeOp t:stack) rest
  | otherwise = state 3 stack (t:rest)
state 3 [expr] [] = expr
state 3 stack (t:rest)
  | isAddSub t = state 5 (makeOp t:stack) rest
  | isRpar t = state 8 (makeOp t:stack) rest
state 4 stack (t:rest)
  | isFactor t = state 6 (makeF t:stack) rest
  | isLpar t = state 0 (makeOp t:stack) rest
state 5 stack (t:rest)
  | isFactor t = state 7 (makeF t:stack) rest
  | isLpar t = state 0 (makeOp t:stack) rest
state 6 stack tokens = reduce6 stack tokens 
state 7 stack [] = reduce7 stack []
state 7 stack (t:rest)
  | isMulDiv t = state 4 (makeOp t:stack) rest
  | otherwise = reduce7 stack (t:rest)
state 8 stack tokens = reduce8 stack tokens

--
-- Reductions
--

reduce6 (expr2:op:expr1:stack) tokens = reduce (makeAST expr1 op expr2:stack) tokens 2
reduce7 (expr2:op:expr1:stack) tokens = reduce (makeAST expr1 op expr2:stack) tokens 3 
reduce8 (_:expr:_:stack) tokens = reduce (expr:stack) tokens 1

reduce [expr] tokens defaultS = state defaultS [expr] tokens
reduce (expr:(Op op):stack) tokens defaultS
  | op `elem` ['*', '/'] = state 6 (expr:(Op op):stack) tokens
  | op `elem` ['+', '-'] = state 7 (expr:(Op op):stack) tokens
  | otherwise = state defaultS (expr:(Op op):stack) tokens

makeAST exp1 (Op '+') exp2 = Add exp1 exp2
makeAST exp1 (Op '-') exp2 = Sub exp1 exp2
makeAST exp1 (Op '*') exp2 = Mul exp1 exp2
makeAST exp1 (Op '/') exp2 = Div exp1 exp2

makeF (TInt int) = Imm int
makeF (TStr arg) = ArgS arg

makeOp (TChar c) = Op c

--
-- Checks
--

isFactor :: Token -> Bool
isFactor (TInt _) = True
isFactor (TStr _) = True
isFactor _ = False

isMulDiv :: Token -> Bool
isMulDiv (TChar c) | c `elem` ['*', '/'] = True
                   | otherwise = False

isAddSub :: Token -> Bool
isAddSub (TChar c) | c `elem` ['+', '-'] = True
                   | otherwise = False

isLpar :: Token -> Bool
isLpar (TChar '(') = True
isLpar _ = False

isRpar :: Token -> Bool
isRpar (TChar ')') = True
isRpar _ = False

------------------------------------
--
-- Pass 2
--
------------------------------------

pass2 :: AST -> AST
pass2 = undefined

------------------------------------
--
-- Pass 3
--
------------------------------------

pass3 :: AST -> [String]
pass3 = undefined
