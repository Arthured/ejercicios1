module Cap7 where

import Data.List
import Data.Char
type Bit = Int

a :: [a] ->( a -> b ) ->( a -> Bool)-> [b]
a xs f p=map f (filter p xs)

a1 ::  [a] ->( a -> b ) ->( a -> Bool)-> [b]
a1 xs f p= [f x | x <- xs, p x]

bin2int :: [Bit] -> Int
bin2int xs= sum [x*(2^n) | (n,x)<-zip [0..l] xs]
    where l=length xs

bin2inte :: [Bit] -> Int
bin2inte bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

----
agregar :: [Bit]-> [Bit]
agregar xs |count 1 xs `mod` 2 == 0 = xs ++ [0]
            | otherwise = xs ++ [1]

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

eva :: [[Bit]] -> [[Bit]]
eva []=[]
eva (xs:xss) | (count 1 (init xs) `mod` 2 == 0) && head (reverse xs) == 0 = init xs : eva (xss) 
              | (count 1 (init xs) `mod` 2 == 1) && head (reverse xs) == 1 = init xs : eva (xss)
              | otherwise = error "Hubo un error en la codificacion"

encode' :: String -> [Bit]
encode' = concat . map (agregar.make8 . int2bin . ord)

decode' :: [Bit] -> String
decode' = map (chr . bin2int) .eva . chop9

transmit' :: String -> String
transmit' = decode' . channel . encode'

channel' :: [Bit] -> [Bit]
channel' = f.g  
    where f x = init x
          g x = x ++ [1,1]  

channel'' :: [Bit] -> [Bit]
channel'' = tail       
----
---- Votos

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

--Ejercicios 
all' :: (a -> Bool ) -> [a] -> Bool
all' p = and.map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []=[]
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                     | otherwise = [] 

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] =[]
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = (x:xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs)[] 

dec2int :: [Int] -> Int
dec2int = foldl (\y x -> x + y*10) 0 

aa :: [Int] -> Int
aa= foldl (\y x -> x + y) 0

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f= \x -> \y -> f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (==[]) (take 8) (drop 8)


map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' h x=unfold null (h . head) tail x

        --where 
          --  h =( a -> b)
{-}

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x =[]
                |  otherwise = h x : unfold p h t (t x)

map'' _ [] = []
map'' h x@(a:as)=unfold (\_ -> x == [] ) (h) (\_ -> tail x)

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' h (a:as)=unfold (\_ -> False) h (\_ -> ) a 


--map'' :: (a -> b) -> [a] -> [b]
map'' h= unfold (==[]) h (tail)
       -- where 
         --   h :: (a->b)

--iterate :: (a -> a) -> a -> [a]
--iterate f x = x : iterate f (f x)

iterate' :: (a -> a) -> a -> [a]
iterate' f x= unfold (== 0) (f) (f x)-}


unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x =[]
                |  otherwise = h x : unfold p h t (t x)

iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate f (f x)
b = take 10 $ iterate''' (+1) 0

iterate''' :: (a -> a) -> a -> [a]
iterate''' f x= unfold (\_ -> False) (id) (f) x

altmap:: (a->b) -> (a->b) -> [a]->[b]
altmap _ _ []=[]
altmap f h [x] = (f x):[]
altmap f h (x:y:xs)=(f x) : (h y) :altmap f h xs

luhnDoble:: Int -> Int
luhnDoble x | x*2 <= 9 = x*2
            | otherwise =x*2-9

luhn :: [Int] -> Bool
luhn  x=sum (altmap luhnDoble id x) `mod` 10 == 0 


