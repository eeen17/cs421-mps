module Lib where

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

--- Data Types
--- ----------

--- ### Environments and Results

type Env = H.HashMap String Val

type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val
  = IntVal Int
  | BoolVal Bool
  | CloVal [String] Exp Env
  | ExnVal String
  deriving (Eq)

instance Show Val where
  show (IntVal i) = show i
  show (BoolVal i) = show i
  show (CloVal xs body env) =
    "<"
      ++ show xs
      ++ ", "
      ++ show body
      ++ ", "
      ++ show env
      ++ ">"
  show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp
  = IntExp Int
  | BoolExp Bool
  | FunExp [String] Exp
  | LetExp [(String, Exp)] Exp
  | AppExp Exp [Exp]
  | IfExp Exp Exp Exp
  | IntOpExp String Exp Exp
  | BoolOpExp String Exp Exp
  | CompOpExp String Exp Exp
  | VarExp String
  deriving (Show, Eq)

--- ### Statements

data Stmt
  = SetStmt String Exp
  | PrintStmt Exp
  | QuitStmt
  | IfStmt Exp Stmt Stmt
  | ProcedureStmt String [String] Stmt
  | CallStmt String [Exp]
  | SeqStmt [Stmt]
  deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps =
  H.fromList
    [ ("+", (+)),
      ("-", (-)),
      ("*", (*)),
      ("/", (div))
    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps =
  H.fromList
    [ ("and", (&&)),
      ("or", (||))
    ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps =
  H.fromList
    [ ("<", (<)),
      (">", (>)),
      ("<=", (<=)),
      (">=", (>=)),
      ("/=", (/=)),
      ("==", (==))
    ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp op a b = ExnVal "Cannot lift"
-- liftIntOp op a b = ExnVal $ "Cannot lift" ++ show a ++ show b  -- TODO: remove

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp op a b = ExnVal "Cannot lift"
-- liftBoolOp op a b = ExnVal $ "Cannot lift" ++ show a ++ show b  -- TODO: remove

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp op a b = ExnVal "Cannot lift"
-- liftCompOp op a b = ExnVal $ "Cannot lift" ++ show a ++ show b  -- TODO: remove

--- Eval
--- ----

pairsEnv :: Env -> [(String, Exp)] -> Env
pairsEnv env pairs = H.fromList $ map (\(name, exp) -> (name, eval exp env)) pairs

-- duplicating env into argument pairs env
-- pairsEnv env [] = env
-- pairsEnv env (p@(s, e):ps) = pairsEnv (H.insert s (eval e env) env) ps

    -- (var, exp) = (var, eval exp env)

eval :: Exp -> Env -> Val
--- ### Constants

eval (IntExp i) _ = IntVal i
eval (BoolExp i) _ = BoolVal i
--- ### Variables

eval (VarExp s) env =
  case H.lookup s env of
    Just val -> val
    Nothing -> ExnVal "No match in env"
    -- Nothing -> ExnVal $ "No match in env " ++ show s ++ show env   -- TODO: remove
--- ### Arithmetic

eval (IntOpExp op e1 e2) env
  | op == "/" && v2 == IntVal 0 = ExnVal "Division by 0"
  | otherwise = liftIntOp f v1 v2
  where
    v1 = eval e1 env
    v2 = eval e2 env
    Just f = H.lookup op intOps
-- in liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op boolOps
   in liftBoolOp f v1 v2
eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op compOps
   in liftCompOp f v1 v2
--- ### If Expressions

-- eval (IfExp e1 e2 e3) env =
--     case e1 of
--         BoolExp b ->

eval (IfExp condition p q) env =
  case eval condition env of
    BoolVal True -> eval p env
    BoolVal False -> eval q env
    _ -> ExnVal "Condition is not a Bool"


eval (FunExp params body) env = CloVal params body env

eval (AppExp func args) env =
  case eval func env of
    CloVal var body clenv -> eval body (H.union (pairsEnv env (zip var args)) clenv)
    _                     -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = eval body (pairsEnv env pairs)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
  where
    val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, H.insert var (eval e env) env)
--- ### Sequencing

exec (SeqStmt stmts) penv env = execSeq stmts penv env
  where
    execSeq [] pe e = ("", pe, e)
    execSeq (x:xs) pe e =
      let
        (out, npe, ne)    = exec x pe e
        (nout, nnpe, nne) = execSeq xs npe ne
      in (out ++ nout, nnpe, nne)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
  case eval e1 env of
    BoolVal True -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name argExps) penv env =
  case H.lookup name penv of
    Just (ProcedureStmt _ argNames body) -> exec body penv $ H.union(pairsEnv env (zip argNames argExps)) env   -- include outside environment for free vars
    Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)

