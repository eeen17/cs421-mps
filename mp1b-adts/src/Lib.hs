--- Getting Started
--- ===============
--- Relevant Files
--- --------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 21923

tag2 = 44437

tag3 = 24929

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a
  = Cons a (List a)
  | Nil
  deriving (Show, Eq)

data Exp
  = IntExp Integer
  | PlusExp [Exp]
  | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons
list2cons :: [a] -> List a
-- don't forget to put the type declaration or you will lose points!
list2cons [] = Nil
list2cons (x : xs) = Cons x (list2cons xs)

--- ### cons2list
cons2list :: List a -> [a]
-- don't forget to put the type declaration or you will lose points!
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval
eval :: Exp -> Integer
-- don't forget to put the type declaration or you will lose points!
eval (IntExp val) = val
eval (PlusExp vals) = case vals of
  [] -> 0
  (x : xs) -> eval x + eval (PlusExp xs)
eval (MultExp vals) = case vals of
  [] -> 1
  (x : xs) -> eval x * eval (MultExp xs)

--- ### list2cons'
list2cons' :: [a] -> List a
-- don't forget to put the type declaration or you will lose points!
list2cons' = foldr Cons Nil

--- ### BinTree
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
  deriving (Show)

--- ### sumTree
sumTree :: Num a => BinTree a -> a
-- don't forget to put the type declaration or you will lose points!
sumTree Leaf = 0
sumTree (Node x a b) = x + sumTree a + sumTree b

--- ### SimpVal
data SimpVal
  = IntVal Integer
  | BoolVal Bool
  | StrVal String
  | ExnVal String
  deriving (Show)

--- ### liftIntOp
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
-- don't forget to put the type declaration or you will lose points!
liftIntOp op (IntVal a) (IntVal b) = IntVal (op a b)
liftIntOp _ _ _ = ExnVal "not an IntVal!"