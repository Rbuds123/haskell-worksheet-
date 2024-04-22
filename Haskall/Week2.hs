absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && y == z = 3
  | x == y || y == z || x == z = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = diagonalLength a + diagonalLength b + diagonalLength c
  where
  diagonalLength :: Float -> Float
  diagonalLength a = sqrt (2 * a ^ 2)


taxiFare :: Int -> Float
taxiFare distance
  | distance <= 10 = baseFare + 0.50 * fromIntegral distance
  | otherwise = baseFare + fare * 10 + addedFare
  where
    baseFare = 2.20
    fare = 0.50
    addedFare = 0.30 * fromIntegral (distance - 10)

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c 
      | fromIntegral a > avg && fromIntegral b > avg = 2
      | fromIntegral a > avg && fromIntegral c > avg = 2
      | fromIntegral b > avg && fromIntegral c > avg = 2
      | fromIntegral c > avg || fromIntegral b > avg || fromIntegral c > avg = 1
      | otherwise = 0
      where
      avg = fromIntegral (a + b + c) / 3
  


validDate :: Int -> Int -> Bool
validDate day mon
  | mon == 1 || mon == 3 || mon == 5 || mon == 7 || mon == 8 || mon == 10 || mon == 12 = day >= 1 && day <= 31
  | mon == 4 || mon == 6 || mon == 9 || mon == 11 = day >= 1 && day <= 30
  | mon == 2 = day >= 1 && day <= 28
  | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth mon year
  | mon == 1 || mon == 3 || mon == 5 || mon == 7 || mon == 8 || mon == 10 || mon == 12 = 31
  | mon == 4 || mon == 6 || mon == 9 || mon == 11 = 30
  | mon == 2 = if isLeapYear then 29 else 28
  where
    isLeapYear = mod year 4 == 0

{-
  sumThree 3 5 7
  3 + 5 + 7
  8 + 7
  15
-}
{-
  sumThree 8 (1 + 3) 2
  8 (1 + 3) 2
  8 + 4 + 2
  12 + 2
  14
-}

{-
  threeDifferent 1 4 2
  1 =/ 4 True and
  1 =/ 2 True and
  4 =/ 2 True
  True
-}

{-
  threeDifferent 1 7 7
  1 =/ 7 True
  1 =/ 7 True
  7 /= 7 False
  False
-}

{-
  howManyEqual 5 2 5
  5 == 2 and 2 == 5 no move to next line
  5 == 2 not equal or 2 == 5 not equal or 5 == 5 Equal
  2
-}

{-
  howManyEqual 3 5 2
  3 == 5 and 5 == 2 no move to next line
  3 == 5 or 5 == 2 or 3 == 2 no
  0
-}