module Main where

import System.CPUTime

------------------------------Queue class---------------------------------------

class Queue q where
    empty :: q a -> Bool
    enqueue :: q a -> a -> q a
    peek :: q a -> a
    dequeue :: q a -> (a, q a)

------------------------------First Queue---------------------------------------

data FirstQueue a = FirstQueue [a] deriving (Show)

instance Queue FirstQueue where
    empty (FirstQueue xs) = null xs

    enqueue (FirstQueue xs) y = FirstQueue $ xs ++ [y]

    peek (FirstQueue []) = error "The queue is empty"
    peek (FirstQueue xs) = head xs

    dequeue (FirstQueue []) = error "The queue is empty"
    dequeue (FirstQueue (x:xs)) = (x, FirstQueue xs)

-------------------------------Second Queue-------------------------------------

data SecondQueue a = SecondQueue [a] [a] deriving (Show)

instance Queue SecondQueue where
    empty (SecondQueue xs ys) = null xs && null ys

    enqueue (SecondQueue xs ys) y = SecondQueue xs (y:ys)

    peek (SecondQueue [] []) = error "The queue is empty"
    peek (SecondQueue [] ys) = peek (SecondQueue (reverse ys) [])
    peek (SecondQueue xs _) = head xs

    dequeue (SecondQueue [] []) = error "The queue is empty"
    dequeue (SecondQueue [] ys) = dequeue (SecondQueue (reverse ys) [])
    dequeue (SecondQueue (x:xs) ys) = (x, SecondQueue xs ys)

--------------------------------Third Queue-------------------------------------

data ThirdQueue a = ThirdQueue [a] Int [a] Int deriving (Show)

instance Queue ThirdQueue where
    empty (ThirdQueue xs _ ys _) = null xs

    enqueue (ThirdQueue xs lsize ys rsize) y
        | lsize > rsize + 1 =
            ThirdQueue xs lsize (y:ys) (rsize + 1)
        | otherwise =
            ThirdQueue (xs++(reverse (y:ys))) (lsize + rsize + 1) [] 0

    peek (ThirdQueue [] _ _ _) = error "The queue is empty"
    peek (ThirdQueue xs _ _ _) = head xs

    dequeue (ThirdQueue [] _ _ _) = error "The queue is empty"
    dequeue (ThirdQueue (x:xs) lsize ys rsize)
        | lsize - 1 > rsize =
            (x, ThirdQueue xs (lsize - 1) ys rsize)
        | otherwise =
            (x, ThirdQueue (xs ++ (reverse ys)) (lsize - 1 + rsize) [] 0)

-------------------------------------end----------------------------------------

enq q x i
    | i == x = enqueue q i
    | otherwise = enqueue (enq q x (i + 1)) i

deq q
    | empty q = 1
    | otherwise = deq (snd $ dequeue q)

test q x = do
    start <- getCPUTime
    let q' = enq q x 0
    let q'' = deq q'
    print $ (show q'') !! 0
    end <- getCPUTime
    print $ fromIntegral (end - start)/(10^12)

main = do
    let q1 = FirstQueue []
    test q1 10000
    let q2 = SecondQueue [] []
    test q2 100000
    let q3 = ThirdQueue [] 0 [] 0
    test q3 100000
