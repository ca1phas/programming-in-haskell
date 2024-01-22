import System.IO (hSetEcho, stdin)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

getTarget :: IO String
getTarget = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- getTarget
      return (x : xs)

getGuess :: IO String
getGuess = do
  putStr "Guess the secret word: "
  getLine

match :: String -> String -> String
match ts gs = [if t `elem` gs then t else '-' | t <- ts]

correct :: String -> IO Bool
correct "" = do return True
correct (c : cs) = do
  result <- correct cs
  return (c /= '_' && result)

play :: String -> IO ()
play target = do
  guess <- getGuess
  if guess == target
    then putStrLn "Correct!"
    else do
      putStrLn (match target guess)
      play target

main :: IO ()
main = do
  putStr "Enter the secret word: "
  target <- getTarget
  play target
