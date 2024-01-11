-- High level language
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add l r) = eval l + eval r

-- Virtual machine
type Code = [Op]

type Stack = [Int]

data Op = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c ((m + n) : s)

-- Compiler
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp :: Expr -> Code
comp e = comp' e []
