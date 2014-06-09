module Main where

import Control.Applicative
import Data.Fold.Katana

main :: IO ()
main = do
  let k1 = fold1 3 $ fold1 2 $ fold1 1 kincdec
  let k2 = fold1 1 kincdec
  let k3 = fold1 3 kincdec
  print . fst $ unfold1 k1
  print . fst $ unfold1 $ snd $ unfold1 (k2 <|> k3)
  print . fst $ katana kincdec [1,2,3]

-------------------------------------------------------------------------

kincdec :: Katana Int Int
kincdec = K inc dec 0
  where
  inc x a = max 0 $ x + a
  dec x
    | x > 0 = (Just x,x-1)
    | otherwise = (Nothing,x)

