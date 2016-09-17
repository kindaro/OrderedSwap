import Lib

main :: IO ()
main = do
    putStrLn . (++) "Test cases to pass: " $ show . length $ testCases
    putStrLn . (++) "Failed cases: " $ show . length . filter not $ testCases

testCases
    =   [ ord 1 2 == True
        , ord 1 1 == True
        , ord 1 0 == False
        ]
    ++  [ isOrdered [1,2,3] == True
        , isOrdered [1,2,2] == True
        , isOrdered [1,2,1] == False
        , isOrdered [1,2,1,2] == False
        ]
    ++  [ swapInList 2 4 [1,2,3,4,5] == [1,4,3,2,5]
        ]
    ++  [ truth (fNaive [1,3,2,4]) == True
        ]

