main :: IO ()
main = print (demostra [1,3,7,10,25,50])

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True  
valid Mul _ _ = True
valid Div x 0 = False
valid Div x y = x `mod` y == 0 

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

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
eval (Val n) = [n]
eval (App o l r)= [apply o x y | x <- eval l,
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
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
   [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

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
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
---
choices' :: [a] -> [[a]]
choices' xs =[zs | ys <- subs xs, zs <- perms ys]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

remover :: Eq a => a -> [a] -> [a]
remover x [] = []
remover x (y:ys)    | x == y = ys
                    | otherwise = y: remover x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys= elem x ys && isChoice xs (remover x ys)

--No terminaria, porque ya no se garantiza que las llamas recursivas a exprs reduzcan la longitud de lista

concatt :: [[a]] -> [a]
concatt xss = [x | xs <- xss,x <- xs]
--comprobar :: [Int] -> [a]
comprobar xs = [exprs ys| ys <- (choices xs),ys /= []]

--demos :: [Int] -> Int
demos xs =  (concatt  (comprobar xs))

--demostra :: [Int] -> Int
demostra xs = length ([eval e | e <- concatt (comprobar xs),eval e /= []])

