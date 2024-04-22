import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x =  f  (f x) 

twice' :: (a -> a) -> a -> a 
twice' f x =  f  (f x)

--point free style 
twice'' :: (a -> a) -> a -> a
twice'' f   = f . f 

twice''' :: (a -> a) -> a -> a
twice'''  f x   = (f . f) x 

mult = (*)

charAt :: String -> Int -> Char
charAt string index = string !! index

fredChars = charAt "fred"
samChars = charAt "sam"

double = (*2)

addNewLine :: String -> String
addNewLine = (++ "\n")



{--
-- map :: (a -> b) -> [a] -> [b]

--}

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

addNewLines = map addNewLine 

keepPositive = filter (>0)



inc :: Int -> Int 
inc x = x + 1