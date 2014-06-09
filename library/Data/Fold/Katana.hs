{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module Data.Fold.Katana where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Data.Foldable       (Foldable,foldl)
import           Data.Monoid
import           Pipes
import           Prelude             hiding (foldl,id,(.))

-------------------------------------------------------------------------

--TODO: clean up variable names

fold1 :: a -> Katana a b -> Katana a b
fold1 x (K c a s) = K c a (c s x)

fold :: (Foldable f) => Katana a b -> f a -> Katana a b
fold (K c a s) as = K c a (foldl c s as)

unfold1 :: Katana a b -> (Maybe b, Katana a b)
unfold1 (K c a s) = second (K c a) $ a s

unfold :: Katana a b -> ([b], Katana a b)
unfold (K c a s) = let (bs,s''') = unfold' s in (bs,K c a s''')
  where
  unfold' s' = case a s' of 
    (Just b,s'') -> let (bs',s'''') = unfold' s'' in (b:bs',s'''')
    (Nothing,_)  -> ([],s')

katana :: (Foldable f) => Katana a b -> f a -> ([b], Katana a b)
katana k = unfold . fold k

recurse :: Katana a (Either a b) -> Katana a b
recurse (K c a s) = K c a' s
  where
  a' s' = 
    let (b,s'') = a s'
    in  maybe (Nothing,s'') (either (a' . c s'') ((,s'') . Just)) b

toPipe :: (Monad m) => Katana a b -> Pipe a b m ()
toPipe (K c a s) = go s
  where
  go s' = do
    a' <- await
    let s'' = c s' a'
    unfold' s''
  unfold' s' = case a s' of
    (Just b,s'') -> yield b >> unfold' s'' 
    (Nothing,_) -> go s'

-------------------------------------------------------------------------

data Katana a b = forall x. K (x -> a -> x) (x -> (Maybe b,x)) x -- (b -> b -> b)

instance Functor (Katana a) where
  fmap f (K c a s) = K c (first (fmap f) . a) s

instance Applicative (Katana a) where
  pure b = K (\() _ -> ()) (\() -> (Just b,())) () 
  K xc xfa xs <*> K yc ya ys = K c' a' s'
    where
    c' (Pair x y) a = Pair (xc x a) (yc y a)
    a' (Pair x y) = 
      let (xfb,x') = xfa x
          (yb,y') = ya y
      in  (xfb <*> yb,Pair x' y') 
    s' = Pair xs ys

instance Alternative (Katana a) where
  empty = K (\() _ -> ()) (\() -> (Nothing,())) () 
  K xc xa xs <|> K yc ya ys = K c' a' s'
    where
    c' (Pair x y) a = Pair (xc x a) (yc y a)
    a' (Pair x y) = 
      let (xb,x') = xa x
          (yb,y') = ya y
          f Nothing = (yb,Pair x' y')
          f xb' = (xb', Pair x' y)          
      in  f xb
    s' = Pair xs ys

instance Category Katana where
  id = arr id
  K yc ya ys . K xc xa xs = K c' a' s'
    where 
    c' (Pair x y) a = Pair (xc x a) y
    a' = gety
      where
      gety s@(Pair x y) = 
        let (yb,y') = ya y
        in  maybe (getx s) (\yb' -> (Just yb',Pair x y')) yb 
      getx s@(Pair x y) = 
        let (xb,x') = xa x
        in  maybe (Nothing,s) (\xb' -> gety (Pair x' (yc y xb'))) xb
    s' = Pair xs ys   

instance Arrow Katana where
  arr f =  K (\_ a -> Just (f a)) (\s -> (s,Nothing)) Nothing where
  first (K c a s) = K c' a' s' 
    where
    c' (s'',_) (b,d) = (c s'' b,Just d)
    a' (s'',d) = a'' (a s'') d
    a'' (Just b', s'') (Just d') = (Just (b',d'),(s'',Just d'))
    a'' _ d' = (Nothing,(s,d'))
    s' = (s,Nothing)

-------------------------------------------------------------------------

data Pair a b = Pair !a !b

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend (Pair a b) (Pair c d) = Pair (mappend a c) (mappend b d)

