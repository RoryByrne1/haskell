import Data.Char

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth x y = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = pyth a b == square c

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z = isTriple x y z || isTriple x z y || isTriple y z x

halfEvens :: [Int] -> [Int]
halfEvens xs = [if mod x 2 == 0 then div x 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [x | x <- xs, x >= a, x <= b]

countPositives :: [Int] -> Int
countPositives xs = length [1 | x <- xs, x > 0]

capitalised :: String -> String
capitalised (h:hs) = toUpper h : [toLower char | char <- hs]

lowerString :: String -> String
lowerString hs = [toLower char | char <- hs]

title :: [String] -> [String]
title [] = []
title (h:hs) = capitalised h : [if length word > 3 then capitalised word else lowerString word | word <- hs]