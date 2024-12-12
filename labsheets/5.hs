{-1-}

-- pre: n >= 0
-- post: fac n = n!
fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x - 1)

{-
basis: n = 0, fac 0 = 1 = 0!
induction: assume fac n = n!
fac (n+1) = (n+1) * fac n
      = (n+1) * n!
      = (n+1)!
-}


{-2-}

-- pre: n >= 0
-- post: fib n = fibonacci(n)
-- where fibonacci(n) is the nth fibonacci number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

{-
UNFINISHED
basis: n = 0, fib 0 = 0 = fibonacci(0)
       n = 1, fib 1 = 1 + fib 0 = 1 = fibonacci(1)
induction: assume fib (n-1) = fibonacci(n-1)
fib n = fib (n-1) + fib (n-2)
      = fibonacci(n-1)
-}


{-3-}

-- pre: True
-- post: reverse xs = reversed xs
reverse' :: [a] -> [a] -> [a]
reverse' [] sx = sx
reverse' (x:xs) sx = reverse' xs (x : sx)

{-
NOT RIGHT I DON'T THINK
basis: length k = 0, reverse' k [] = [] = reversed([])
induction: assume reverse' ks [k] = reversed(ks) ++ [k]
reverse' k:ks = reverse' ks [k]
              = reversed(ks) ++ [k]
              = reversed(k:ks)
-}


{-4-}

-- [1,3,5,7]

displayBoard :: [Int] -> IO ()
displayBoard = print

emptyBoard :: [Int] -> Bool
emptyBoard = foldr ((&&) . (== 0)) True 

makeMove :: [Int] -> Int -> Int -> [Int]
makeMove board row num = take (row-1) board ++ [board !! (row-1) - num] ++ drop row board

play :: [Int] -> Bool -> IO ()
play board isP1 = do displayBoard board
                     if isP1 
                        then putStrLn "player 1's turn"
                        else putStrLn "player 2's turn"
                     putStr "enter heap: "
                     input1 <- getLine
                     let row = (read input1 :: Int)
                     putStr "enter number to take: "
                     input2 <- getLine
                     let num = (read input2 :: Int)

                     let newBoard = makeMove board row num

                     if emptyBoard newBoard
                        then if isP1 
                                then putStrLn "player 2 wins" 
                                else putStrLn "player 1 wins" 
                        else play newBoard (not isP1)
           
nimGame :: IO ()
nimGame = do putStrLn "welcome to nim"
             play [1,3,5,7] True
             