module Main where

{-# LANGUAGE ExtendedDefaultRules #-}

import Control.Monad.Trans.List
import Conduit
import Control.Applicative
import Data.MonoTraversable

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

main :: IO ()
main = do
    putStrLn "List version:"
    mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 1000000000
         .| mapMC magic
         .| mapM_C print

-- mapMC :: Monad m => (a -> m b) -> Conduit a m b

iterMC' :: Monad m => (a -> m ()) -> Conduit a m a
iterMC' f = mapMC (\a -> (f a) >> return a)


--  (<*>) :: f (a -> b) -> f a -> f b
-- liftA :: Applicative f => (a -> b) -> f a -> f b
sink' :: Monad m => ConduitM Int o m (String, Int)
sink' = liftA2 (,) a b
  where
   a = takeC 5 .| mapC show .| foldC
   b = sumC


-- ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
-- yield :: Monad m => o -> ConduitM i [o] m ()
yieldMany' :: (Data.MonoTraversable.MonoFoldable mono, Monad m) =>
        mono 
        -> ConduitM i (Data.MonoTraversable.Element mono) m ()
yieldMany' = ofoldr (\a b -> yield a >> b) mempty