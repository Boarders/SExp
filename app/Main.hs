module Main where

import SExp.Parser
import Text.Megaparsec
import Data.Void


main :: IO ()
main =
  case parseInput input of
    Left bun ->
      do
        putStrLn "Parse error: "
        putStrLn (errorBundlePretty bun)

    Right expr ->
      do
        putStrLn "input was: "
        putStrLn input
        putStrLn ""
        putStrLn ""
        print expr
        putStrLn ""
        putStrLn . pretty $ expr


parseInput :: String -> Either (ParseErrorBundle String Void) SExp
parseInput inp = parse parseSExp "" inp


input :: String
input = "(5 6 (1 2 3\n seven (nine) (ten 12)))"
