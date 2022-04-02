module Cap6 where

data Bool' = True' | False'

(&&&&) :: Bool' -> Bool' -> Bool'
(&&&&) a b = if a == True' then b else False'

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:(y:(ys))
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fac :: Int -> Int
fac n = product[1..n]
--Con fac' no acabaria la funcion pues nunca llegaria a 0
fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n-1)

fac'' :: Int -> Int
fac'' 0 = 1
fac'' n | n > 0 = n * fac'' (n-1)
        | otherwise = error "No esta defindo para negativos"

suma :: (Num a, Eq a)=> a -> a
suma 0 = 0
suma n = n + suma (n-1)

(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m  * (m ^^^ (n-1))

euclides :: Int -> Int -> Int
euclides x y | x == y = x
            | x < y = euclides x (y-x)
            | x > y = euclides (x-y) y

------
and' :: [Bool] -> Bool
and' []= True
and' (x:xs) | x == True = and' xs
            | otherwise = False 
{-concat' :: [[a]] -> [a]
concat' []       = []
concat' (x:[])   = x
concat' (a:b:xs) = concat'' (concat'' a b) (concat' xs)
  where
    concat'' :: [a] -> [a] -> [a]
    concat'' [] y      = y
    concat''  (y:xs) y =x:(concat'' xs y)

{-
concat' (x:[]) = x
concat' ([]:x) = x
concat' ((x:[]):y) = x:y
concat' ((x:z:[]):y) = x:z:y
concat' ((x:xs): y)=x:concat'(xs:y)-}
--concat' (xs:xss) = xs-}

replicat :: Int -> a -> [a]
replicat 0 _ = []
replicat n a = a:(replicat (n-1) a)

(!!!) :: [a]-> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = (xs) !!! (n-1)

eleme :: Eq a => a -> [a] -> Bool
eleme a []= False
eleme a (x:xs) | a /= x = eleme a xs
                | a==x = True

merge:: Ord a => [a] -> [a] -> [a]
merge x []= x
merge [] x = x
merge (x:xs) (y:ys) | x <= y   = x:(merge (xs) (y:ys))
                    | y < x   = y:(merge (x:xs) (ys))

halve :: [a] -> ([a],[a])
halve xs = (take n xs,drop n xs)
        where 
        n=length xs `div` 2

mqsort :: Ord a => [a] -> [a] 
mqsort [] = []
mqsort (x : []) = [x]
mqsort xs = merge (mqsort ys)  (mqsort zs)
  where
    (ys,zs) = halve xs
