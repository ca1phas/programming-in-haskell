import AI (playAI)
import Data.Char (toUpper)
import Game (Player (O, X), play)

main :: IO ()
main = do
  putStrLn "Select mode:"
  putStrLn "1. Human vs Human"
  putStrLn "2. Human vs AI"

  mode <- getLine

  case mode of
    "1" -> play
    "2" -> do
      putStr "Select player (X/O)"
      player <- getLine
      case toUpper $ head player of
        'X' -> playAI O
        'O' -> playAI X
        _ -> do
          putStrLn "Invalid player."
          main
    _ -> do
      putStrLn "Invalid mode."
      main
