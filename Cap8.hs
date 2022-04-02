module Cap8 where

type Pos = (Int,Int)

type Trans = Pos -> Pos

type Assoc k v = [(k,v)]

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

data Shape = Circle Float | Rect Float Float
            deriving (Eq, Ord, Show, Read)
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

--newtype Nat = N Int

data Nat = Zero | Succ Nat
            deriving (Eq, Ord, Show, Read)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs
{-}
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

ocurs :: Eq a => a -> Tree a -> Bool
ocurs x (Leaf y)= x == y
ocurs x (Node l y r) = x == y || ocurs x l || ocurs x r

flattes :: Tree a -> [a]
flattes (Leaf a) = [a]
flattes (Node l a r)= ((flattes l)++[a]++(flattes r))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)= x == y
occurs x (Node l y r) | x == y  =True
                        | x < y  = occurs x l
                        | otherwise = occurs x r-}
data Prop = Const Bool
            |Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Sii Prop Prop
            deriving (Eq, Ord, Show, Read)

type Subst = Assoc Char Bool

p1 :: Prop
p1 = Not (And (Var 'A') (Not (Var 'A')))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p6 :: Prop 
p6 = Sii(Sii (Var 'P')(Var 'Q')) (And (Imply (Var 'P') (Var 'Q')) (Imply (Var 'Q') (Var 'P')))
p7 :: Prop 
p7 = Not(Sii (Or (Var 'P')(Var 'Q')) (Not(Or (Var 'P')(Var 'Q'))))

eval:: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Sii p q) = eval s p <= eval s q && eval s q <= eval s p

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q)= vars p ++ vars q
vars (Sii p q)= vars p ++ vars q
vars (Or p q) = vars p ++ vars q 

bools :: Int -> [[Bool]]
bools n = reverse(map (reverse.map cov. make n. int2bin) range)
        where
            range =[0..(2^n)-1]
            make n bs =take n (bs ++ repeat 0)
            cov 0 = False
            cov 1 = True

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (True:) bss ++ map (False:) bss
            where bss = bools' (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools' (length vs))
         where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

data Expr = Val Int | Add Expr Expr
            deriving (Eq, Ord, Show, Read)

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int

evalu :: Expr -> Cont -> Int
evalu (Val n ) c = exec c n
evalu (Add x y) c=evalu x (EVAL y:c)

exec:: Cont -> Int -> Int 
exec [] n = n
exec (EVAL y : c) n = evalu y (ADD n : c)
exec (ADD n : c) m = exec c n+m

value' :: Expr -> Int
value' e = evalu e []


data Expr' = Val' Int | Add' Expr' Expr'| Mult Expr' Expr'
            deriving (Eq, Ord, Show, Read)
type Cont' = [Op']
data Op' = EVAL' Expr'|EVALM Expr'| ADD' Int |MULT Int


evalu' :: Expr' -> Cont' -> Int
evalu' (Val' n ) c = exec' c n
evalu' (Add' x y) c=evalu' x (EVAL' y:c)
evalu' (Mult x y) c = evalu' x (EVALM y:c)--

exec':: Cont' -> Int -> Int 
exec' [] n = n
exec' (EVAL' y : c) n = evalu' y (ADD' n : c)
exec' (EVALM y : c) n = evalu' y (MULT n : c)
exec' (MULT n : c) m = exec' c (n*m) 
exec' (ADD' n : c) m = exec' c n+m

value'' :: Expr' -> Int
value'' e = evalu' e []

a1 :: Expr'
a1 = Add' (Mult (Val' 2) (Val' 5)) (Add' (Val' 2) (Val' 5))

----

p:: Nat
p = Succ (Succ (Succ Zero))

q:: Nat
q =Succ (Succ Zero)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add' m (mult m n)
{-}
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)= x == y
occurs' x (Node l y r)= if n == EQ then True else if n == LT then  occurs' x l else occurs' x r  
        where 
            n = compare x y 

occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y)= x == y
occurs'' x (Node l y r) | x == y  =True
                        | x < y  = occurs x l
                        | otherwise = occurs x r-}
data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Eq, Ord, Show, Read)
leaves :: Tree a-> Int
leaves (Leaf _ ) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _)  = True
balanced (Node l r) =abs ( leaves l -leaves r) <= 1 

t :: Tree Int
t = Node (Node (Node (Leaf 3) (Node (Leaf 3) (Leaf 3))) (Leaf 4))  (Node (Leaf 6)  (Leaf 9))

halve :: [a] -> ([a],[a])
halve xs = (take n xs,drop n xs)
        where 
        n=length xs `div` 2

balance :: [a]-> Tree a
balance [x] = Leaf x
balance xs= Node (balance ys) (balance zs)
        where
            (ys,zs) = halve xs

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add z x) = g (folde f g z) (folde f g x)

a :: Expr
a = Add (Add (Val 5) (Val 5)) (Val 5)
h x y= y + x

eval' :: Expr -> Int 
eval' x = 
  case x of
    Val n -> folde f g (Val n) 
    Add x y ->  folde f g (Add x y)
  where 
    g= \x -> \y -> x+y
    f = \x -> x

size :: Expr -> Int
size x =
  case x of
    Val n -> folde f g (Val n) 
    (Add x y) -> folde f g (Add x y)--size x + size y
  where 
    g= \x -> \y -> x+y
    f = \x -> 1

{-}
instance Eq a => Eq (Maybe a) where
  (Just a) == (Just b) = a==b
  Nothing == Nothing = True
  _ == _ = False

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  _ == _ = False
-}
data Maybe' a = Nothing' | Just' a

data Lista' a = a::: Lista' a | Null  

instance Eq a => Eq (Maybe' a) where
  (Just' a) == (Just' b) = a==b
  Nothing' == Nothing' = True
  _ == _ = False

instance Eq a => Eq (Lista' a) where
 -- (==) :: Lista' a -> Lista' a -> Bool
  Null == Null = True
  (x:::xs) == (y:::ys) = (x == y) && (xs == ys)
  _ == _ = False
