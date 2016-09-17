{-# LANGUAGE
    LambdaCase
  #-}

module Lib where


import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Array


foldM1 f (x:xs) = foldM f x xs

f = undefined

f1 :: [Integer] -> Bool
f1 (a:b:xs) = undefined
f1 list = True

-- whereDoesListFall :: [Integer] -> [Integer]
-- whereDoesListFall xs
--     | length xs < 2 = []
--     | otherwise = snd . runWriter $ foldM1 f xs
--     where
--     f :: Integer -> Integer -> WriterT [Integer] (StateT Integer Identity) Integer
--     f a b
--         | pairIsNonDecreasing a b = return b
--         | otherwise = tell [position] >> return b
--     position = 13

listIsNonDecreasing :: [Integer] -> Bool
listIsNonDecreasing [] = True
listIsNonDecreasing [a] = True
listIsNonDecreasing [a,b] = pairIsNonDecreasing a b
listIsNonDecreasing (a:b:xs)
    | pairIsNonDecreasing a b = listIsNonDecreasing (b:xs)
    | otherwise = False

pairIsNonDecreasing :: Integer -> Integer -> Bool
pairIsNonDecreasing a b = a <= b

data Answer = Answer { truth :: Bool, permutation :: Maybe [Integer] }
    deriving Show

fNaive :: [Integer] -> Answer
fNaive list
    | length list < 3 = Answer { truth = True, permutation = Nothing }
    | length answers > 0 = head answers
    | otherwise = Answer { truth = False, permutation = Nothing }
    where
        range = [0 .. (fromIntegral . length) list - 1]
        answers = (filter truth) $
            [ Answer
                { truth = listIsNonDecreasing (swapInList x y list)
                , permutation = if x == y then Nothing else Just [x, y]
                }
            | x <- range, y <- range
            ]

swapInList :: Integer -> Integer -> [a] -> [a]
swapInList a b list
    | a == b = list
    | otherwise =
        let
            m = listArray (0, (fromIntegral . length) list - 1) list
            a' = fromIntegral a :: Int
            b' = fromIntegral b :: Int
            storeSwap = m ! a'
        in
            elems $ m // [(a', m ! b')] // [(b', storeSwap)]

