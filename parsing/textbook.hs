import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P $ \inp -> [(head inp, tail inp) | not $ null inp]

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \inp -> [(f a, out) | (a, out) <- parse p inp]

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P $ \inp -> [(a, inp)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P $ \inp -> [(f a, out) | (f, inp') <- parse pf inp, (a, out) <- parse pa inp']

three :: Parser (Char, Char)
three = do
  x <- item
  item
  z <- item
  return (x, z)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \inp -> concat [parse (f a) out | (a, out) <- parse p inp]

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const []

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> p' = P $ \inp ->
    let ls = parse p inp
     in if null ls then parse p' inp else ls

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return $ read xs

space :: Parser ()
space = do
  many $ sat isSpace
  return ()

int :: Parser Int
int =
  ( do
      char '-'
      n <- nat
      return (-n)
  )
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol cs = token $ string cs

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

-- Arithmetic Parser
-- Q6,Q7,Q8
expr :: Parser Int
expr = do
  t <- term
  ( do
      symbol "+"
      e <- expr
      return $ t + e
    )
    <|> ( do
            xs <-
              many
                ( do
                    symbol "-"
                    term
                )
            return $ foldl (-) t xs
        )
    <|> return t

term :: Parser Int
term = do
  t' <- term'
  ( do
      symbol "*"
      t <- term
      return $ t' * t
    )
    <|> ( do
            xs <-
              many
                ( do
                    symbol "/"
                    term'
                )
            return $ foldl div t' xs
        )
    <|> return t'

term' :: Parser Int
term' = do
  f <- factor
  ( do
      symbol "^"
      t' <- term'
      return $ f ^ t'
    )
    <|> return f

factor :: Parser Int
factor =
  ( do
      symbol "("
      e <- expr
      symbol ")"
      return e
  )
    <|> token int

eval :: String -> [Int]
eval cs =
  if null xs then error "Invalid input" else xs
  where
    xs =
      [ case out of
          [] -> n
          _ -> error ("Unused input" ++ out)
        | (n, out) <- parse expr cs
      ]

-- Calculator
-- IO utilities
type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

box :: [String]
box =
  [ "+---------------+",
    "|               |",
    "+---+---+---+---+",
    "| q | c | d | = |",
    "+---+---+---+---+",
    "| 1 | 2 | 3 | + |",
    "+---+---+---+---+",
    "| 4 | 5 | 6 | - |",
    "+---+---+---+---+",
    "| 7 | 8 | 9 | * |",
    "+---+---+---+---+",
    "| 0 | ( | ) | / |",
    "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

display :: String -> IO ()
display xs = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse $ take 13 (reverse xs))

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if c `elem` buttons
    then process c xs
    else do
      beep
      calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs
  | c `elem` "cC" = clear
  | c `elem` "qQ\ESC" = quit
  | c `elem` "=\n" = evalCalc xs
  | c `elem` "dD\BS\DEL" = delete xs
  | otherwise = press c xs

clear :: IO ()
clear = calc []

quit :: IO ()
quit = goto (1, 14)

evalCalc :: String -> IO ()
evalCalc xs =
  case parse expr xs of
    [(n, [])] -> calc (show n)
    _ -> do
      beep
      calc xs

delete :: String -> IO ()
delete [] = calc []
delete xs = calc $ init xs

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear

-- Chapter 13
-- Q1
notChar :: Char -> Parser Char
notChar x = sat (/= x)

comment :: Parser ()
comment = do
  symbol "--"
  many $ notChar '\n'
  return ()

-- Q5
data Expr = Add Expr Expr | Mul Expr Expr | Nat Int

expr5 :: Parser Expr
expr5 =
  do
    t <- term5
    ( do
        symbol "+"
        Add t <$> expr5
      )
      <|> return t

term5 :: Parser Expr
term5 =
  do
    t <- factor5
    ( do
        symbol "*"
        Mul t <$> term5
      )
      <|> return t

factor5 :: Parser Expr
factor5 =
  ( do
      symbol "("
      e <- expr5
      symbol ")"
      return e
  )
    <|> Nat <$> token int

evalExpr :: Expr -> Int
evalExpr (Nat n) = n
evalExpr (Add l r) = evalExpr l + evalExpr r
evalExpr (Mul l r) = evalExpr l * evalExpr r

eval5 :: String -> [Int]
eval5 cs =
  if null xs then error "Invalid input" else xs
  where
    xs =
      [ case out of
          [] -> evalExpr e
          _ -> error ("Unused input" ++ out)
        | (e, out) <- parse expr5 cs
      ]
