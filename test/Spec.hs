import Lib

main :: IO ()
main = do
    putStrLn . (++) "Test cases to pass: " $ show . length $ testCases
    putStrLn . (++) "Failed cases: " $ show . length . filter not $ testCases

testCases
    =   [ pairIsNonDecreasing 1 2 == True
        , pairIsNonDecreasing 1 1 == True
        , pairIsNonDecreasing 1 0 == False
        ]
    ++  [ listIsNonDecreasing [1,2,3] == True
        , listIsNonDecreasing [1,2,2] == True
        , listIsNonDecreasing [1,2,1] == False
        , listIsNonDecreasing [1,2,1,2] == False
        ]
    ++  [ swapInList 2 4 [1,2,3,4,5] == [1,4,3,2,5]
        ]
    ++  [ truth (fNaive [1,3,2,4]) == True
        ]

