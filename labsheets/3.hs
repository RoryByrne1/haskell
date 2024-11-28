mult :: [Int] -> Int
mult = foldr (*) 1

posList :: [Int] -> [Int]
posList = filter (>=0)

trueList :: [Bool] -> Bool
trueList = foldr (&&) True

evenList :: [Int] -> Bool
evenList = trueList . map even

maxList :: Ord a => [a] -> a
maxList (a:as) = foldr max a as

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b = filter (>=a) . filter (<=b)

countPositives :: [Int] -> Int
countPositives = foldr ((+) . min 1 . max 0) 0

myLength :: [a] -> Int
myLength = foldr (+) 0 . map (\a -> 1)

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myLength' :: [a] -> Int
myLength' = foldr ((+) . \a -> 1) 0