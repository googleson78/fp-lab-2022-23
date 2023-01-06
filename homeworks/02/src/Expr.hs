{-# LANGUAGE EmptyDataDeriving #-}

module Expr where

import Prelude hiding (lookup, unlines, unwords)

data Expr
  = Var String
  | Val Integer
  | Oper OperType Expr Expr
  | If Expr Expr Expr
  | SumList [Expr]
  | Sum String Expr Expr
  deriving (Show)

data OperType
  deriving (Show)

type Context = [(String, Integer)]

extend :: String -> Integer -> Context -> Context
extend x n = ((x, n) :)

lookup :: String -> Context -> Maybe Integer
lookup = undefined

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen = undefined

infixl 1 `maybeAndThen`

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe = undefined

freeVars :: Expr -> [String]
freeVars = undefined

eval :: Context -> Expr -> Maybe Integer
eval = undefined

intersperse :: a -> [a] -> [a]
intersperse = undefined

unwords :: [String] -> String
unwords = undefined

unlines :: [String] -> String
unlines = undefined

solvingCompiler :: Bool
solvingCompiler = False

newtype RacketProgram = MkRacketProgram [RacketExpr]

data RacketExpr
  = Name String
  | List [RacketExpr]
  deriving (Show)

printRacketExpr :: RacketExpr -> String
printRacketExpr = undefined

printRacketProgram :: Context -> RacketProgram -> String
printRacketProgram = undefined

compileToRacket :: Expr -> RacketExpr
compileToRacket = undefined

solvingPartialEval :: Bool
solvingPartialEval = False

partialEval :: Expr -> Expr
partialEval = undefined

solvingSum :: Bool
solvingSum = False
