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
cpsExp exp k freshVars =
    case exp of
        IntExp x -> (AppExp k exp, freshVars)
        VarExp x -> (AppExp k exp, freshVars)
        AppExp f e
            | isSimple e ->     -- f e k 
                (AppExp (AppExp f e) k, freshVars)
            | otherwise ->
                let
                    (v, freshVars') = gensym freshVars                  -- get fresh v
                    newK = LamExp v (AppExp (AppExp f (VarExp v)) k)    -- take the evaluated result of e and put into f v(e) k
                in cpsExp e newK freshVars'                             -- recurse on non-simple argument e
                -- cpsExp e (\v -> f v k)
        OpExp op e1 e2
            | isSimple exp ->   -- k OpExp
                (AppExp k exp, freshVars)

            | isSimple e1 ->    -- cpsExp e2 (\v -> e1 `op` v)
                let
                    (v, freshVars') = gensym freshVars                  -- get fresh v
                    newK = LamExp v (AppExp k (OpExp op e1 (VarExp v))) -- take the evaluated result of e2 and put into the operation
                in cpsExp e2 newK freshVars'                            -- recurse on non-simple argument e2
                
            | isSimple e2 ->    -- cpsExp e1 (\v -> v `op` e2)
                let
                    (v, freshVars') = gensym freshVars                  -- get fresh v
                    newK = LamExp v (AppExp k (OpExp op (VarExp v) e2)) -- take the evaluated result of e2 and put into the operation
                in cpsExp e1 newK freshVars'         
                                   -- recurse on non-simple argument e1
            | otherwise ->      -- cpsExp e1 (\v1 -> (cpsExp e2 (\v2 -> v1 `op` v2)))
                let
                    (v1, freshVars1) = gensym freshVars                 -- get fresh v1
                    (v2, freshVars2) = gensym freshVars1                -- get fresh v2
                    nestedK = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                    (e2Eval, freshVars3) = cpsExp e2 nestedK freshVars2
                in cpsExp e1 (LamExp v1 e2Eval) freshVars3
        IfExp cond e1 e2 
            | isSimple cond -> 
                let
                    (e1Eval, freshVars2) = cpsExp e1 k freshVars
                    (e2Eval, freshVars3) = cpsExp e2 k freshVars2
                in (IfExp cond e1Eval e2Eval, freshVars2)
            | otherwise ->
                let
                    (v, freshVars1) = gensym freshVars
                    (e1Eval, freshVars2) = cpsExp e1 k freshVars1
                    (e2Eval, freshVars3) = cpsExp e2 k freshVars2

                    innerIf = IfExp (VarExp v) e1Eval e2Eval
                in cpsExp cond (LamExp v innerIf) freshVars3

--- #### Define `cpsExp` for Integer and Variable Expressions

--- #### Define `cpsExp` for Application Expressions

--- #### Define `cpsExp` for Operator Expressions

--- #### Define `cpsExp` for If Expressions

--- ### Define `cpsDecl`

-- Stmt = Decl String [String] Exp
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl funcName args bodyExp) =
    let
        newArgs = args ++ ["k"]
        cpsBodyExp = fst (cpsExp bodyExp (VarExp "k") 1)
    in Decl funcName newArgs cpsBodyExp
