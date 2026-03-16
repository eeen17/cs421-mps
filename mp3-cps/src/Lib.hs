--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
-- factk = undefined
factk = aux
    where
        aux 0 newk = newk 1  -- stop
        aux x newk = aux (x-1) (\res -> newk (x * res))  -- res := result of aux(x-1). aux(x) = x * res

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

-- list and 2 continuations:
-- cont 1: receives sum of all even elements of the list
-- cont 2: receive sum of all odd elements of the list
-- decides which cont to use based on last element's parity
evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk = aux
    where
        aux [x] kEven kOdd
            | even x = kEven x  -- if even, add last element to running even
            | odd x = kOdd x -- if odd, add last element to running odd
        aux (x:xs) kEven kOdd 
            | even x = aux xs (\res -> kEven (res + x)) kOdd  -- if even, add x to the even sum result of aux(xs) and return (apply to previous running)
            | odd x = aux xs kEven (\res -> kOdd (res + x)) -- vice versa


--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`


-- IntExp Integer
-- VarExp String
-- LamExp String Exp
-- IfExp Exp Exp Exp
-- OpExp String Exp Exp
-- AppExp Exp Exp
isSimple :: Exp -> Bool
isSimple exp =
    case exp of
        IntExp _ -> True
        VarExp _ -> True
        LamExp _ _ -> False -- ignore, just gonna return false for now
        IfExp e1 e2 e3 -> isSimple e1 && isSimple e2 && isSimple e3
        OpExp _ e1 e2 -> isSimple e1 && isSimple e2
        AppExp _ _ -> False
        

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp = undefined

--- #### Define `cpsExp` for Integer and Variable Expressions

--- #### Define `cpsExp` for Application Expressions

--- #### Define `cpsExp` for Operator Expressions

--- #### Define `cpsExp` for If Expressions

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl = undefined
