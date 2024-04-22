-- algebraic type

--type AGE = Int

data Degree = Ordinary | Third | LowerSeccond | UpperSecond | First 
    --has all types that have equalitiy in them INT FLOAT DOUBLE TUPLE LISTS but not FUNCTIONS 
    deriving (Eq, Ord, Show, Read)


isGood :: Degree -> Bool
isGood degree = degree == UpperSecond || degree == First 


isGood' :: Degree -> Bool 
isGood' = (>= UpperSecond)


data Person = Person String Int 
    deriving(Eq, Show)
--recod syntax 

--People :: [Person]
--People = [Person "John" 53,
--          Person "Sam" 16,
--          Person "Kate" 85,
--          Person "Jill" 65,
--          Person "Bill" 37,
--          Person "Amy" 22]

name :: Person -> String 
name (Person n _) = n

age :: Person -> Int
age (Person _ a)= a 

names :: [Person] -> [String]
names = map name 


data Shape = Circle Float | 
                Rectangle Float Float
            deriving (Eq, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)