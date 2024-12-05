
-- 1

squares :: [Int] -> [Int]
squares = map (^2)

sumSquares :: [Int] -> Int
sumSquares = foldr (+) 0 . squares

allPositive :: [Int] -> Bool
allPositive = foldr ((&&) . (>0)) True

-- 2

minFunction :: (Int -> Int) -> Int -> Int
minFunction f n = minimum (map f [0..n])

equalFunction :: (Int -> Int) -> Int -> Bool
equalFunction f n = foldr ((&&) . (== f 0) . f) True [1..n]

positiveFunction :: (Int -> Int) -> Int -> Bool
positiveFunction f n = foldr ((&&) . (> 0) . f) True [0..n]

increasingFunction :: (Int -> Int) -> Int -> Bool
increasingFunction f n = foldr ((&&) . (\x -> f x < f (x + 1))) True [0..(n-1)]

-- 3

twice :: (a -> a) -> a -> a
twice f = f . f

-- 4

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- 5

double :: Num a => a -> a
double x = x * 2

powerTwo :: Int -> Int
powerTwo n = iter n double 1

-- 6

data RhType = Positive | Negative deriving (Show, Eq)

data ABOType = A | B | AB | O deriving (Show, Eq)

type BloodType = (RhType,ABOType)

patient :: Int -> (RhType,ABOType)
patient n | n == 1 = (Positive,B)
          | n == 2 = (Negative,AB)
          | n == 3 = (Negative,A)
          | n == 4 = (Negative,O)
          | n == 5 = (Positive,AB)

showRh :: RhType -> IO ()
showRh = print

showABO :: ABOType -> IO ()
showABO = print

showBloodType :: BloodType -> IO ()
showBloodType (rh,abo) = do showRh rh
                            showABO abo

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (rh1,abo1) (rh2,abo2) | abo1 == abo2 = True
                                  | abo1 == O    = True
                                  | abo2 == AB   = True

-- 7

data Answer = Yes | No | Unknown deriving Eq

wonky :: Answer -> Answer
wonky a | a == Yes     = No
        | a == No      = Unknown
        | a == Unknown = Yes

-- shortest is 3
-- longest is 3
-- depends on the version, weird question

-- 8 

data Shape = Ellipse Float Float
           | Rect Float Float
        
area :: Shape -> Float
area (Rect x y) = x * y
area (Ellipse r1 r2) = pi * r1 * r2
