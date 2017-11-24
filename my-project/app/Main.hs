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

