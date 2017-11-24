module Lib where

import Data.Conduit

{-# LANGUAGE ExtendedDefaultRules #-}

import Control.Monad.Trans.List
import Conduit
import Control.Applicative
import Data.MonoTraversable

-- EXERCISE Implement iterMC in terms of mapMC.
iterMC' :: Monad m => (a -> m ()) -> Conduit a m a
iterMC' f = mapMC (\a -> (f a) >> return a)

-- EXERCISE Rewrite sink to not use do-notation. Hint: it'll be easier to go Applicative.
--  (<*>) :: f (a -> b) -> f a -> f b
-- liftA :: Applicative f => (a -> b) -> f a -> f b
sink' :: Monad m => ConduitM Int o m (String, Int)
sink' = liftA2 (,) a b
  where
   a = takeC 5 .| mapC show .| foldC
   b = sumC

-- EXERCISE Modify trans so that it does something different for the first 3, second 3, and final 3 values from upstream, and drops all other values.



-- EXERCISE Reimplement yieldMany for lists using the yield primitive and monadic composition.
-- ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
-- yield :: Monad m => o -> ConduitM i [o] m ()
yieldMany' :: (Data.MonoTraversable.MonoFoldable mono, Monad m) =>
        mono
        -> ConduitM i (Data.MonoTraversable.Element mono) m ()
yieldMany' = ofoldr (\a b -> yield a >> b) mempty

myMapC :: Monad m => (i -> o) -> ConduitM i o m ()
myMapC f = loop
  where 
    loop = do
       mx <- await
       case mx of
         Nothing -> return ()
         Just x  -> do 
           yield (f x)
           loop

--EXERCISE Try implementing filterC and mapMC. For the latter, you'll need to use the lift function.

-- filterC :: Monad m => (a -> Bool) -> Conduit a m a
filterC' :: Monad m => (a -> Bool) -> Conduit a m a
filterC' p = loop
  where 
    loop = do
       mx <- await
       case mx of
         Nothing -> return ()
         Just x ->
             if (p x) 
               then (yield x) >> loop
               else loop

-- mapMC :: Monad m => (a -> m b) -> Conduit a m b
-- λ: >:i Conduit
-- type Conduit i (m :: * -> *) o = ConduitM i o m ()
mapMC' :: Monad m => (a -> m b) -> ConduitM a b m ()
mapMC' f = loop
  where 
    loop = do
       mx <- await
       case mx of
         Nothing -> return ()
         Just x  -> do
           lift $ f x
           loop
