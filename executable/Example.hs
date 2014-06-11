module Main where

import           Control.Applicative
import           Control.Arrow
import           Data.Fold.Katana
import           Pipes
import qualified Pipes.Prelude       as P

-------------------------------------------------------------------------

main :: IO ()
main = do
  let k1 = fold1 3 $ fold1 2 $ fold1 1 kincdec
  let k2 = fold1 1 kincdec
  let k3 = fold1 3 kincdec
  let k4 = fold kincdec [1,2,3]
  print . fst $ unfold1 k1
  print . fst $ unfold1 $ snd $ unfold1 (k2 <|> k3)
  print . fst $ katana kincdec [1,2,3]
  runEffect $ each [1..3] >-> toPipe kincdec >-> P.print
  print . fst $ unfold k4
  print . fst $ katana (first k3) [(1,1),(2,2)::(Int,Int)]
  print . fst $ unfold (k1 &&& k3)
  print . fst $ katana (k3 *** k3) [(1,1) :: (Int,Int)] 
  print . fst $ katana (k1 *** k3) [(2,3),(1,2) :: (Int,Int)] 
  print . fst $ katana (k1 >>> k3) [1,2 :: Int] 

-------------------------------------------------------------------------

kincdec :: Katana Int Int
kincdec = K inc dec 0
  where
  inc x a = max 0 $ x + a
  dec x
    | x > 0 = (Just x,x-1)
    | otherwise = (Nothing,x)

