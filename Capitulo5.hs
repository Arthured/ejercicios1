module Capitulo5 where

import Data.Char

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

positions' ::  Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..]

lower :: String -> Int
lower xs = length[x | x <- xs,x >='A' && x <= 'Z']

count :: Char -> String -> Int
count x xs =length[x' | x' <-xs,x==x']  

pitagoras :: Int -> [(Int,Int,Int)]
pitagoras n=[(x,y,z)| x<-[1..n],y <-[1..n],z <-[1..n],x^2+y^2==z^2]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n )

shift :: Int -> Char -> Char
shift n c | isLower c =int2let ((let2int c + n)`mod` 26)
            |otherwise =c

codificar :: Int -> String -> String
codificar n xs= [shift n x | x <- xs]

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n/fromIntegral m) *100

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


freqs :: String -> [Float]
freqs xs = [porcentaje (count x xs) n | x <- ['A'..'z']]
            where n = lower xs

letras :: String -> [(Char,Float)]
letras xs = [(x,porcentaje (count x xs) n)|x <- ['a'..'z']]
            where n = lower xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = codificar (-factor) xs
        where
            factor = head (positions (minimum chitab) chitab)
            chitab = [chisqr (rotate n table') table | n <- [0..25]]
            table' = freqs xs

---------------------------------
concatt :: [[a]] -> [a]
concatt xss = [x | xs <- xss,x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

l :: (Enum a ,Num a) => a -> a
l n = sum[ x^2 | x <- [1..n]]

grid :: Int -> Int -> [(Int, Int)] 
grid m n = [(x,y)| x <- [0..m], y <-[0..n]]

square :: Int -> [(Int, Int)] 
square n =[(x,y)|(x,y)<-grid n n , x/=y]

replica :: Int -> a -> [a]
replica n x = [x |_<-[1..n]]

perfects :: Int -> [Int]
perfects n =[x | x<- [1..n],
                sum (tail (reverse (factors x))) == x ]

--7
--(x,y)=(x| x <- [1,2], y+2| y<- [1,2] )

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct a b= sum [x * y | (x,y) <- zip a b]
---------
let2int' :: Char -> Int
let2int' c | ord c <= ord 'z' && ord c >= ord 'a' = ord c - ord 'a'
            |ord c <= ord 'Z' && ord c >= ord 'A' = ord c - ord 'A'
            | otherwise = ord c

int2let' :: Int -> Char 
int2let' n = chr(ord 'A' + n )

shift' :: Int -> Char -> Char
shift' n c | isLower c =int2let ((let2int' c + n)`mod` 26)
            | isUpper c =int2let' ((let2int' c + n)`mod` 26)
            | otherwise =c

codificar' :: Int -> String -> String
codificar' n xs= [shift' n x | x <- xs]



letras' :: String -> [(Char,Float)]
letras' xs = [(x,porcentaje (count x xs) n)|x <- ['A'..'Z']]
            where n = lower xs


freqs' :: String -> [Float]
freqs' xs =[porcentaje (count x xs) n | x <- ['a'..'z']]
            where n = lower xs

{-}
freqs :: String -> [Float]
freqs xs = [porcentaje (count x xs) n | x <- ['a'..'z']]
            where n = lower xs

letras :: String -> [(Char,Float)]
letras xs = [(x,porcentaje (count x xs) n)|x <- ['a'..'z']]
            where n = lower xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = codificar (-factor) xs
        where
            factor = head (positions (minimum chitab) chitab)
            chitab = [chisqr (rotate n table') table | n <- [0..25]]
            table' = freqs xs-}

crack' :: String -> String
crack' xs = codificar' (-factor) xs
        where
            factor = head (positions (minimum chitab) chitab)
            chitab = [chisqr (rotate n table') table | n <- [0..25]]
            table' = freqs' xs
