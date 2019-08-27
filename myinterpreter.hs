-- The language was created by Suyash Dubey (sdd1n17), Sanjeevan Sritharan (ss6n17)

import System.IO
import System.Environment
import Control.Exception
import Data.Char
import COBRATokens
import COBRAGrammar
import COBRAEvaluator

main :: IO ()
main = catch main' noParse

main' = do (programName : _ ) <- getArgs 
           program <- readFile programName
           inputText <- hGetContents stdin
           verifyInput <- evaluate(verifyStr(inputText))
           let input = (createMultiStream(getInput(lines inputText)))
           let parsedProg = parseCalc(alexScanTokens program)
           typeCheck <- evaluate (typeOf parsedProg [])
           let result = evalFull1 (parsedProg) (input)
           let output = unParse(result)
           hPutStrLn stdout output

noParse :: ErrorCall -> IO ()
noParse e = do let err = show e
               hPutStr stderr err
               putStrLn ("\n")


-- Fucntion to create a stream in our language 
createStream :: [Int] -> PureStream
createStream [] = EmptyStream
createStream (x:xs) = (PartOf x (createStream xs))

-- Function to create a multistream in our language
createMultiStream :: [[Int]] -> PureMultiStream
createMultiStream [] = Empty
createMultiStream (x:xs) = AddedTo (createStream x) (createMultiStream xs)

-- Function to split all strings in a list
splitStr :: [String] -> [[String]]
splitStr xs = map words xs

-- Function that verifies if the input is legal
verifyStr :: String -> Bool
verifyStr "" = True
verifyStr [x] | isDigit x || isSpace x = True
              | isLetter x = error ("Illegal alphabet in input: " ++ (show x))
              | isSymbol x || isPunctuation x = error ("Illegal symbol in input: " ++ (show x))
              | not(isDigit x) || not(isSpace x) = error ("Illegal character in input: " ++ (show x))
              | otherwise = False
verifyStr (x:y:xs) | isDigit x || isSpace x || (x == '-' && isDigit y) = verifyStr ([y] ++ xs)
                   | isLetter x = error ("Illegal alphabet in input: " ++ (show x))
                   | isSymbol x || isPunctuation x = error ("Illegal symbol in input: " ++ (show x))
                   | not(isDigit x) || not(isSpace x) = error ("Illegal character in input: " ++ (show x))
                   | otherwise = False


-- Function to convert all string elements to integers
toInt :: [[String]] -> [[Int]]
toInt xs = map (map (read :: String -> Int)) xs

-- Function to get heads of lists in a list if they are not null
firsts :: [[Int]] -> [[Int]]
firsts [] = []
firsts xxs = [[head xs | xs <- xxs, not(null xs)]]

-- Function to get tails of lists in a list if they are not null
tails :: [[Int]] -> [[Int]]
tails [] = []
tails [[]] = [[]]
tails xxs = [tail xs | xs <- xxs, not(null (tail xs))]

-- Function that converts the input to a list of lists of integers
getInput :: [String] -> [[Int]]
getInput [] = []
getInput xs = firsts ss ++ firsts (tails ss)
               where ss = (toInt(splitStr(xs)))       
