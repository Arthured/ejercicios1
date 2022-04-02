module Cap9 where
import Data.List

data Op = Add | Sub | Mul | Div | Exp
    deriving (Eq,Ord)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = (y /= 1) && (y /= 0 )&& (x `mod` y == 0)  
valid Exp x y = y>0 &&y /= 1 && x /= 1

{-}
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
valid Exp _ _ = True-}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
    deriving (Eq, Ord)

instance Show Expr where 
    show (Val n) =show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       ="(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)= [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n>0 ]
eval (App o l r)= [apply o x y |x <- eval l,
                                y <- eval r, 
                                valid o x y]

subs :: [a]-> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs

interveale :: a ->[a]->[[a]]
interveale x [] = [[x]]
interveale x (y:ys) = (x:y:ys) :map(y:) (interveale x ys)

perms :: [a] -> [[a]]
perms []= [[]]
perms (x:xs) = concat(map (interveale x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [ _ ] = []
split (x : xs) = ([x],xs): [(x :ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =[ e |(ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =[e  | ns' <- choices ns, e <- exprs ns', eval e == [n]]

merge:: Ord a => [a] -> [a] -> [a]
merge x []= x
merge [] x = x
merge (x:xs) (y:ys) | x <= y   = x:(merge (xs) (y:ys))
                    | y < x   = y:(merge (x:xs) (ys))

halve :: [a] -> ([a],[a])
halve xs = (take n xs,drop n xs)
        where 
        n=length xs `div` 2

--mqsort :: Ord a => [a] -> [a] 
mqsort [] = []
mqsort (x : []) = [x]
mqsort xs = merge (mqsort ys)  (mqsort zs)
  where
    (ys,zs) = halve xs

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n)|n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx <- results ls, 
                    ry <- results rs,
                    res <- combine' lx ry]

combine':: Result -> Result -> [Result]
combine' (l,x) (r,y) =[(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

{-solucion :: [Int] -> Int -> [Expr]
solucion ns n | solutions' ns n == [] = [e | ns' <- choices ns, (e,m) <- results ns', m == mini ns' (n)]
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns',  m == o'(o (b (mini ns') n) n) ]
-}
mini :: [Int] -> [Int]
mini ns =  [m |  (e,m)<-results ns]

b :: [Int] -> Int -> [Int]
b ns n=mqsort (n:ns)

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

o :: [Int] -> Int -> [Int]
o ns n = drop (z-1) (take (z+2) ns)
    where 
        z = head(positions n ns)
o' :: [Int] -> Int
o' (x:y:z:[]) | abs(x-y) <= abs (z-y) = x
              | otherwise = z


---- Ejercicios
choices' :: [a] -> [[a]]
choices' xs =[zs | ys <- subs xs, zs <- perms ys]

concatt :: [[a]] -> [a]
concatt xss = [x | xs <- xss,x <- xs]
--comprobar :: [Int] -> [a]
comprobar xs = [exprs ys| ys <- (choices xs),ys /= []]

--demos :: [Int] -> Int
demos xs =  (concatt  (comprobar xs))

--demostra :: [Int] -> Int
demostra xs = ([eval e | e <- concatt (comprobar xs),eval e /= []])

---

solcerca :: [Int] -> Int -> [Expr]
solcerca ns n = map snd (takeWhile (\x -> fst x == first) todasSol)
  where
    todasSol = sort [(abs (m - n), e) | ns' <- choices ns, (e, m) <- results ns']
    first = (fst . head) todasSol

solordena :: [Int] -> Int -> [Expr]
solordena ns n = map snd ordenar
             where
                ordenar = sort [(length (values e), e) | e <- solcerca ns n]