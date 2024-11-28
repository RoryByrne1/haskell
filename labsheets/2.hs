import Data.Char

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b (x:xs) | a <= x && x <= b = x : inRange a b xs
                   | otherwise        = inRange a b xs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = (if x > 0 then 1 else 0) + countPositives xs

lowerWord :: String -> String
lowerWord [] = []
lowerWord (h:hs) = toLower h : lowerWord hs

capitalised :: String -> String
capitalised (h:hs) = toUpper h : lowerWord hs

titleWords :: [String] -> [String]
titleWords [] = []
titleWords (h:hs) | length h > 3 = capitalised h : titleWords hs
                  | otherwise    = lowerWord h : titleWords hs

title :: [String] -> [String]
title [] = []
title (h:hs) = capitalised h : titleWords hs

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) | a > x     = x : insert a xs
                | otherwise = a : x : xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a b | head a <= head b = head a : merge (tail a) b
          | otherwise        = head b : merge a (tail b)

rotor :: Int -> String -> String
rotor x hs | x < 0          = error "x too damn small"
           | x >= length hs = error "x too damn big"
           | otherwise      = drop x hs ++ take x hs

makeKey :: Int -> [(Char,Char)]
makeKey x = zip ['A'..'Z'] (rotor x ['A'..'Z'])

lookUp :: Eq a => a -> [(a,a)] -> a
lookUp c [] = c
lookUp c (k:ks) | c == fst k = snd k
                | otherwise  = lookUp c ks

encipher :: Int -> Char -> Char
encipher x c = lookUp c (makeKey x)

normalise :: String -> String
normalise [] = []
normalise (h:hs) | h `elem` ['0'..'9']         = h : normalise hs
                 | toUpper h `elem` ['A'..'Z'] = toUpper h : normalise hs
                 | otherwise                   = normalise hs

encipherStr :: Int -> String -> String
encipherStr x hs = map (encipher x) (normalise hs)

fAnalysisChar :: Char -> Int
fAnalysisChar c | c `elem` ['A'..'Z'] = [812,149,271,432,1202,230,203,592,731,10,69,398,261,695,768,182,11,602,628,910,288,111,209,17,211,7] !! (ord c - ord 'A')
                | otherwise = 0

fAnalysisStr :: String -> Int
fAnalysisStr [] = 0
fAnalysisStr (h:hs) = fAnalysisChar h + fAnalysisStr hs

bruteForce :: String -> String
bruteForce hs = encipherStr (lookUp (maximum [fAnalysisStr (encipherStr x hs) | x <- [0..25]]) [(fAnalysisStr (encipherStr x hs), x) | x <- [0..25]]) hs