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

bounds' list = (1, (fromIntegral . length) list)
range' list = [ (fst . bounds') list .. (snd . bounds') list ]

fNaive :: [Integer] -> Answer
fNaive list
    | length list < 3 = Answer { truth = True, permutation = Nothing }
    | length answers > 0 = head answers
    | otherwise = Answer { truth = False, permutation = Nothing }
    where
        answers = (filter truth) $
            [ Answer
                { truth = listIsNonDecreasing (swapInList x y list)
                , permutation = if x == y then Nothing else Just [x, y]
                }
            | x <- range' list, y <- range' list
            ]

swapInList :: Integer -> Integer -> [a] -> [a]
swapInList a b list
    | a == b = list
    | otherwise =
        let
            m = listArray (bounds' list) list
            a' = fromIntegral a :: Int
            b' = fromIntegral b :: Int
            storeSwap = m ! a'
        in
            elems $ m // [(a', m ! b')] // [(b', storeSwap)]

