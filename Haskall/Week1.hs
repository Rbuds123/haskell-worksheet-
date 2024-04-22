circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen num = num * 10

sumThree :: Int -> Int -> Int -> Int
sumThree n1 n2 n3 = n1 + n2 + n3

areaOfCircle :: Float -> Float
areaOfCircle radius = pi * radius ^ 2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h radius = areaOfCircle radius * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = mod a b == 0

isEven :: Int -> Bool
isEven a = divisibleBy a 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute x = if x < 0 then -x else x