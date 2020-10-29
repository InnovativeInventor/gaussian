module Main where

import Lib
import System.IO

main :: IO ()
main = do putStr "Input:\n"
          hFlush stdout
          aStr <- getLine
          bStr <- getLine
          cStr <- getLine
          dStr <- getLine
          let a = read aStr :: Int
          let b = read bStr :: Int
          let c = read cStr :: Int
          let d = read dStr :: Int
          putStrLn "Division"
          putStrLn (show (gaussianDivision (parser a b) (parser c d)))
          putStrLn (show (gaussianMultiplication (parser a b) (parser c d)))
          putStrLn "GCD"
          putStrLn (show (gaussianGcd (parser a b) (parser c d)))
          putStrLn "LCM"
          putStrLn (show (gaussianDivision (gaussianMultiplication (parser a b) (parser c d)) (gaussianGcd (parser a b) (parser c d))))
