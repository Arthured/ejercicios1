module Prueba where
double x= x+x
quadruple x =double(double x)
--div  =x/y
suma []=0
suma (n:ns)=n+suma(ns)
average :: [Int]  -> Float
average ns = ( fromIntegral (sum ns))/( fromIntegral (length ns))

factorial n = product[1..n]
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last' :: [a] -> a
last' xs= xs !! ((length xs)-1)
 
init' :: [a] -> [a] 
init' xs =take (length xs - 1) xs

init'' :: [a] -> [a]
init'' xs = reverse (drop 1 (reverse xs))

a=b+c
    where
        b=1
        c=2
d=a*2

swap :: (a,a) -> (a,a)
swap (x,y) = (y,x)

second :: [a] -> a
second xs = head (tail xs)

pair:: a -> a -> (a,a)
pair x y =(x,y)

doubl :: Num a => a -> a
doubl x = x*2

--palindrome :: [a] -> [a] ->[==]-> Bool
palindrome ::  [Int] -> Bool
palindrome xs = reverse xs == xs 

twice :: (a->a) -> a -> a 
twice f x = f (f x)

bools :: [[Bool]]
bools=[[True],[False]]

num :: [[Int]]
num=[[2,3],[4,5]]

copy :: a -> (a,a)
copy a = (a , a)

apply :: (a->b) -> a -> b 
apply f x = f x
