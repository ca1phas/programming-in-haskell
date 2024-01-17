import Data.List (permutations)

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

p1 :: Prop
p1 =
  And
    (Var 'A')
    (Not (Var 'A'))

p2 :: Prop
p2 =
  Imply
    (And (Var 'A') (Var 'B'))
    (Var 'A')

p3 :: Prop
p3 =
  Imply
    (Var 'A')
    (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 =
  Imply
    ( And
        (Var 'A')
        (Imply (Var 'A') (Var 'B'))
    )
    (Var 'B')

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find c s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

find :: Char -> Subst -> Bool
find _ [] = False
find c (x : xs)
  | c == fst x = snd x
  | otherwise = find c xs

vars :: Prop -> [Char]
vars (Var x) = [x]
vars (Const _) = []
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

subst :: Prop -> [Subst]
subst p = map (zip vs) (bools (length vs)) where vs = unique (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- subst p]