module Ejer3 where
--Ejercicio 1
halve :: [a] -> ([a],[a])
halve xs = (take n xs ,drop n xs)
        where 
        n=length xs `div` 2
--Ejercicio 2
thrid :: [a] -> a
thrid (a:(b:c:xs)) = c

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' xs = head (tail (tail xs))
--Ejercicio 3
safetail :: [a] -> [a]
safetail [] = []
safetail (a:xs)=xs

safetail' :: [a] -> [a]
safetail' xs = if length xs == 0 then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' xs | length xs == 0  = []
              | otherwise       = tail xs

{-Ejercicio 4
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False|| b = b

(||) :: Bool -> Bool -> Bool
b || c | b==c =b
       | otherwise = True
 
 _ || _ =True
False || False =False
-}

--mult :: int -> int -> int -> int Ejercicio 7
mult = \x -> (\y -> (\z -> x*y*z))

luhnDoble:: Int -> Int
luhnDoble x | x*2 <= 9 = x*2
            | otherwise =x*2-9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w =((luhnDoble x)+y+(luhnDoble z)+ w) `mod` 10 == 0 