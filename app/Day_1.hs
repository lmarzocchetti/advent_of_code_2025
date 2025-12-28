module Day_1 where

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

updateCurRotationPartOne :: Int -> Char -> Int
updateCurRotationPartOne rot 'L' =
    if rot >= 0
    then rot
    else updateCurRotationPartOne (rot + 100) 'L'
updateCurRotationPartOne rot 'R' =
    if rot <= 99
    then rot
    else updateCurRotationPartOne (rot - 100) 'R'
updateCurRotationPartOne _ _ = 0

solveDay1PartOne :: Int -> [String] -> Int
solveDay1PartOne _ [] = 0
solveDay1PartOne _ [""] = 0
solveDay1PartOne value (x : xs) =
    if curRotation == 0
    then 1 + solveDay1PartOne curRotation xs
    else solveDay1PartOne curRotation xs
    where
        direction = head x
        number = tail x |> read :: Int
        curRotation = updateCurRotationPartOne
            (if direction == 'L'
            then value - number
            else value + number)
            direction

addTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuple (a, b) (a', b') = (a + a', b + b')

updateCurRotationPartTwo :: Int -> Int -> Char -> (Int, Int)
updateCurRotationPartTwo rot oldRotation 'L'
  | rot >= 0 = (0, rot)
  | checkUpdate = addTuple (0, 0) (updateCurRotationPartTwo (rot + 100) (rot + 100) 'L')
  | otherwise = addTuple (1, 0) (updateCurRotationPartTwo (rot + 100) oldRotation 'L')
  where
      checkUpdate = oldRotation == 0
updateCurRotationPartTwo rot oldRotation 'R'
  | rot <= 99 = (0, rot)
  | checkUpdate = addTuple (0, 0) (updateCurRotationPartTwo (rot - 100) oldRotation 'R')
  | otherwise = addTuple (1, 0) (updateCurRotationPartTwo (rot - 100) oldRotation 'R')
  where 
    checkUpdate = rot == 100
updateCurRotationPartTwo _ _ _ = (0, 0)

solveDay1PartTwo :: Int -> [String] -> Int
solveDay1PartTwo _ [] = 0
solveDay1PartTwo _ [""] = 0
solveDay1PartTwo value (x : xs) =
    if curRotation == 0
    then 1 + count' + solveDay1PartTwo curRotation xs
    else count' + solveDay1PartTwo curRotation xs
    where
        direction = head x
        number = tail x |> read :: Int
        (count', curRotation) = updateCurRotationPartTwo
            (if direction == 'L'
            then value - number
            else value + number)
            value
            direction

day_1 :: IO ()
day_1 = do
    putStrLn "Day 1:"
    input_file <- readFile "files/day_1_input.txt"
    putStr "Part One: "
    print $ solveDay1PartOne 50 (input_file |> lines)
    putStr "Part Two: "
    print $ solveDay1PartTwo 50 (input_file |> lines)