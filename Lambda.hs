{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lambda () where

data Lambda
  = Lam String Lambda
  | Var String
  | App Lambda Lambda
  | Con Double
  | If Lambda Lambda Lambda
  | Oper String Lambda Lambda
  deriving (Show, Eq)

lookupEnv (n : ns, v : vs) name = if n == name then v else lookupEnv (ns, vs) name
lookupEnv ([], []) name = name

rename (Lam name expr) (ns, vs) count = Lam namen (rename expr (name : ns, namen : vs) countn)
  where
    countn = count + 1
    namen = name ++ show countn
rename lambda env count = case lambda of
  (Var name) -> Var (lookupEnv env name)
  (App func arg) -> App (re func) (re arg)
  (If cond true false) -> If (re cond) (re true) (re false)
  (Oper oper left right) -> Oper oper (re left) (re right)
  x -> x
  where
    re expr = rename expr env count

subs lambda target value = case lambda of
  (Var name) -> if name == target then value else Var name
  (Lam name expr) -> if name == target then Lam name expr else Lam name (subst expr)
  (App func arg) -> App (subst func) (subst arg)
  (If cond true false) -> If (subst cond) (subst true) (subst false)
  (Oper oper left right) -> Oper oper (subst left) (subst right)
  x -> x
  where
    subst expr = subs expr target value

evalN (App func arg) = case evalN func of
  (Lam name expr) -> evalN (subs expr name arg)
  f -> App f arg
evalN (If cond true false) = case evalN cond of
  op@Oper {} -> If op true false
  Con 1 -> evalN true
  _ -> evalN false
evalN (Oper oper left right) = case (evalN left, evalN right) of
  (Con l, Con r) -> case oper of
    "+" -> Con (l + r)
    "-" -> Con (l - r)
    "*" -> Con (l * r)
    "/" -> Con (l / r)
    "=" -> Con (if l == r then 1 else 0)
    _ -> error "Unknown oper!"
  (l, r) -> Oper oper l r
evalN x = x

yn = Lam "h" (App x x)
  where
    x = Lam "x" (App (Var "h") (App (Var "x") (Var "x")))

evalV (App func arg) = case (evalV func, evalV arg) of
  (Lam name expr, argV) -> evalV (subs expr name argV)
  (f, argV) -> App f argV
evalV (If cond true false) = case evalV cond of
  op@Oper {} -> If op true false
  Con 1 -> evalV true
  _ -> evalV false
evalV (Oper oper left right) = case (evalV left, evalV right) of
  (Con l, Con r) -> case oper of
    "+" -> Con (l + r)
    "-" -> Con (l - r)
    "*" -> Con (l * r)
    "/" -> Con (l / r)
    "=" -> Con (if l == r then 1 else 0)
    _ -> error "Unknown oper!"
  (l, r) -> Oper oper l r
evalV x = x

yv = Lam "h" (App x x)
  where
    a = Lam "a" (App (App (Var "x") (Var "x")) (Var "a"))
    x = Lam "x" (App (Var "h") a)

m =
  Lam
    "f"
    ( Lam
        "n"
        ( If
            (Oper "=" (Var "n") (Con 0))
            (Con 1)
            (Oper "*" (Var "n") (App (Var "f") (Oper "-" (Var "n") (Con 1))))
        )
    )

main = evalV expr
  where
    expr = App (App yv m) (Con 30)
