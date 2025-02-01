module Main where

import Data.Vector (Vector)
import Data.Vector qualified as V
import MyLib (someFunc)

data Point = Pass | Portal Int

type Path = Vector Point

step :: Point -> Int
step p = case p of
  Pass -> 1
  (Portal n) -> -n

walk :: Path -> (Point -> Int) -> Int
walk path f =
  let len = length path
  in undefined
  -- where
  -- helper :: Path -> (Point -> Int) -> Int -> Int
  -- helper p f i = 

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  someFunc
