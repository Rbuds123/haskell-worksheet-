import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

{-- An example list of student marks
--testData :: [StudentMark]
--testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]
-}
addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (_, mark)
  | mark >= 70 && mark <= 100 = 'A'
  | mark >= 60 && mark < 70 = 'B'
  | mark >= 50 && mark < 60 = 'C'
  | mark >= 40 && mark < 50 = 'D'
  | mark >= 0 && mark <= 100 = 'F'
  | otherwise = error "Invalid mark. Mark should be between 0 and 100."

capMark :: StudentMark -> StudentMark
capMark (name, mark)
  | mark > 40 && mark <= 100 = (name, 40)
  | mark < 40 && mark >= 0 = (name, mark)
  | otherwise = error "Invalid mark. mark should be between 0 and 100."

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [n ^ 2 | n <- [1 .. n]]

capitalise :: String -> String
capitalise string = [toUpper char | char <- string]

onlyDigits :: String -> String
onlyDigits str = [c | c <- str, isDigit c]

capMarks :: [StudentMark] -> [StudentMark]
capMarks studentMarks = [capMark mark | mark <- studentMarks]

{-- Test cases
testData :: [StudentMark]
testData = [("Yahya", 37), ("Phan", 76)]

-}

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents [] = []
gradeStudents stdntMrks = [(student, grade (student, marks)) | (student, marks) <- stdntMrks]

duplicate :: String -> Int -> String
duplicate str n
  | n <= 0 = ""
  | otherwise = str ++ duplicate str (n - 1)

duplicate2 :: String -> Int -> String
duplicate2 str n = concat [str | _ <- [1 .. n]]

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

{--split :: [(a, b)] -> ([a], [b])
split pairs =
  ( [fst pair | pair <- pairs],
    [snd pair | pair <- pairs]
  )
--}

split :: [(a, b)] -> ([a], [b])
split pairs =
  ( [x | (x, _) <- pairs],
    [y | (_, y) <- pairs]
  )
