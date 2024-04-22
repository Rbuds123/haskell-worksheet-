import Distribution.Simple.CCompiler (filenameCDialect)
sayHello :: IO()
sayHello = do
    putStrLn "Hello"
    putStrLn "world"

getInt :: String -> IO Int
getInt prompt = do
    putStr prompt 
    strInput <- getLine
    let intInput = read strInput :: Int
    return intInput 


testGetInt :: IO()
testGetInt = do
    number <- getInt "enter a number: "
    print(number + 1)


outputFile :: IO ()
outputFile = do
    putStr "enter file name: "
    fileName <- getLine
    content <- readFile fileName
    putStrLn content

--------it in ghci shows the previous output  lines turn it into a list of strings unlines turn it back 

addIndentation :: String -> Int -> String
addIndentation string n = unlines newStrLines
    where
    indentation = replicate n ' '
    strLines = lines string
    newStrLines = map (indentation ++) strLines

indentAllLines :: Int -> IO()
indentAllLines n = do
    putStr "enter file name:"
    fileName <- getLine
    content <- readFile fileName
    let newContent = addIndentation content n 
    putStr "enter Outfile: "
    outFile <- getLine
    writeFile outFile newContent

addIfNew :: String -> [String] -> [String]
addIfNew string strings
    | string `elem` strings    = strings
    | otherwise                = string : strings


makeList :: [String] -> IO()
makeList list = do 
    putStr "Enter a word (or exit)"
    word <- getLine
    if word == "exit" then
        print list 
    else do 
        let newList = addIfNew word list 
        makeList newList 