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

type W = WriterT [Integer] (StateT Integer Identity) Integer

-- locateDisorders :: [Integer] -> [Integer]
-- locateDisorders xs
--     | length xs < 2 = []
--     | otherwise = snd . runWriter $ foldl' f (return . return (head xs)) (tail xs)
--     where
--     f :: W -> Integer -> W
--     f a b
--         | a `ord` b = return b
--         | otherwise = tell [position] >> return b
--     position = 13

isOrdered :: [Integer] -> Bool
isOrdered [] = True
isOrdered [a] = True
isOrdered [a,b] = ord a b
isOrdered (a:b:xs)
    | a `ord` b = isOrdered (b:xs)
    | otherwise = False

ord :: Integer -> Integer -> Bool
ord a b = a <= b

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
                { truth = isOrdered (swapInList x y list)
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

