-- Chapter 8 Q10
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MULT Int

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mult x y) c = eval x (EVAL y : MULT (value x) : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (ADD y : c) n = exec c (n + y)
exec (MULT y : c) n = exec c (n * y)
exec (EVAL y : c) n = eval y (ADD n : c)

value' :: Expr -> Int
value' e = eval e []