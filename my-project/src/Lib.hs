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
yieldMany' = ofoldr (\a				    b -> yield a >> b) mempty

