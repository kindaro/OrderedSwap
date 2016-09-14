module Lib where


import Control.Monad.Writer


f = undefined

f1 :: [Integer] -> Bool
f1 (a:b:xs) = undefined
f1 l = True

whereDoesListFall :: [Integer] -> [Integer]
whereDoesListFall xs
    | length xs < 2 = []
    | otherwise = snd . runWriter $ foldM f (head xs) (tail xs)
    where
    f :: Integer -> Integer -> Writer [Integer] Integer
    f a b
        | pairIsNonDecreasing a b = return b
        | otherwise = tell [position] >> return b
    position = 13

listIsNonDecreasing :: [Integer] -> Bool
listIsNonDecreasing [] = True
listIsNonDecreasing [a] = True
listIsNonDecreasing [a,b] = pairIsNonDecreasing a b
listIsNonDecreasing (a:b:xs)
    | pairIsNonDecreasing a b = listIsNonDecreasing (b:xs)
    | otherwise = False

pairIsNonDecreasing :: Integer -> Integer -> Bool
pairIsNonDecreasing a b = a <= b

