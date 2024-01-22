getGuess :: IO String
getGuess = do
  putStr "Guess the secret word: "
  getLine

process' :: String -> String -> String -> String
process' _ "" rs = rs
process' ts (g : gs) rs =
  process'
    ts
    gs
    [ if r /= '_'
        then r
        else if t == g then t else r
      | (t, r) <- zip ts (rs ++ repeat '_')
    ]

process :: String -> String -> IO String
process target guess = do return $ process' target guess ""

correct :: String -> IO Bool
correct "" = do return True
correct (c : cs) = do
  result <- correct cs
  return (c /= '_' && result)

play :: String -> IO ()
play target = do
  guess <- getGuess
  result <- process target guess
  complete <- correct result

  putStrLn result

  if complete
    then putStrLn "Your guess is correct"
    else play target

main :: IO ()
main = do
  putStr "Enter the secret word: "
  target <- getLine
  play target
