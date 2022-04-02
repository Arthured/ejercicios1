module Cap12 where
import Control.Monad
import Data.Char
{-}
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

inc' :: [Float] -> [Float]
inc' = map (+1) 


data Maybe' a = Nothing' | Just' a
    deriving (Eq, Ord, Show, Read)

instance Functor Maybe' where
-- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing' = Nothing'
  fmap g (Just' x) = Just' (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)


t :: Tree Int
t = Node (Node (Node (Leaf 3) (Node (Leaf 3) (Leaf 3))) (Leaf 4))  (Node (Leaf 6)  (Leaf 9))
{-}
instance Functor IO where
-- fmap :: (a -> b) -> IO a -> IO b
  fmap g mx = do {x <- mx; return (g x)}-}

{-}
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}
--(<*>) :: f (a -> b) -> f a -> f b

--inc'' :: Functor f => f Int -> f Int
--inc'' = fmap (+1)
{-}
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure (g <*> x)

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure (g <*> x <*> y)

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure (g <*> x <*> y <*> z)

instance Applicative Maybe where
-- pure :: a -> Maybe a
  pure = Just
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx
instance Applicative IO where
-- pure :: a -> IO a
  pure = return
-- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do {g <- mg; x <- mx; return (g x)}-}

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]
{-}
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)-}

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

--safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

evalu :: Expr -> Maybe Int
evalu (Val n) = Just n
evalu (Div x y) = case evalu x of
            Nothing -> Nothing
            Just n -> case evalu y of
                  Nothing -> Nothing
                  Just m -> safediv n m
{-}
eval' :: Expr -> Maybe Int
eval' (Val n) = pure n
eval' (Div x y) = pure (safediv <*> (eval' x) <*> (eval' y))-}
{-}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
    Nothing -> Nothing
    Just x -> f x-}

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = eval' x >>= \n ->
              eval' y >>= \m ->
              safediv n m

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do 
  x <- xs
  y <- ys
  return (x,y)

-}
--type State = Int
--type ST = State -> State
--type ST a = State -> (a,State)
{-}
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
-- fmap :: (a -> b) -> ST a -> ST b
fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
   let 
     (f,s') = app stf s
     (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
-- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
          deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l',n') = rlabel l n
    (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do 
            n <- fresh
            return (Leaf n)
mlabel (Node l r) = do 
            l' <- mlabel l
            r' <- mlabel r
            return (Node l' r')

conv :: Char -> Maybe Int
conv c  | isDigit c = Just (digitToInt c)
        | otherwise = Nothing
-}
--1
data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
          deriving Show

instance Functor Tree' where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf' = Leaf'
  fmap g (Node' l x r) = Node' (fmap g l) (g x) (fmap g r)
---2
data Lista c a = Null c | Lista' a (Lista c a)

--f :: Lista -> Lista
--f = undefinied 

instance Functor (Lista c) where
  -- fmap :: (a->b) -> f a -> f b
-- fmap :: (a->b) -> Lista c a -> Lista c b
  fmap _ (Null c) = Null c
  fmap g (Lista' a as)= Lista' (g a) (fmap g as)
{-}data Arrow a b = 
instance Functor ((->) a)  where
  -- fmap :: (a->b) -> f a -> f b
  -- fmap :: (b->c) -> ((->) a) b -> ((->) a) c
  -- fmap :: (b->c) -> (a -> b) -> (a -> c)
  fmap f g= g . f
 ---3
instance Applicative ((->) a) where
  --pure:: b -> f b
  --pure:: b -> ((->) a) b
  --pure:: b -> (a->b)
  pure x = \_ -> x
  --(<*>) ::  f (b -> c) -> f b -> f c
  -- (<*>) :: ((->) a)  (b -> c) -> ((->) a) b -> ((->) a) c
  --(<*>) :: ( a ->  (b -> c)) -> (a-> b) -> (a -> c)
  (<*>) f g = \y -> f y (g y) -}
----4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]


--5

{-
1.-pure id <*> x = x
id:: a->a 
pure id :: f (a->a)
x :: f a
pure id <*> x :: f a

2.- pure (g x) = pure g <*> pure x
g :: a -> b
x :: a
g x :: b
pure (g x) :: f b

pure g :: f ( a-> b)
pure x :: f a
pure g <*> pure x :: f b

3.-x <*> pure y = pure (\g -> g y ) <*> x 
x :: f (a->b)
pure y :: f a
x <*> pure y :: f b

y :: a
g y :: b
\g -> g y :: (a-> b) -> b
pure (\g -> g y) :: f((a->b) -> b)
pure (\g -> g y ) <*> x ::f b

4.- x <*> (y <*> z)  = (pure (.) <*> x <*> y ) <*> z
x :: f (b->c)
y:: f (a -> b)
z :: f a
y <*> z :: f b
x <*> (y <*> z) :: f c

(.) :: (b -> c) -> (a -> b) -> (a -> c)
pure (.) :: f ((b -> c) -> (a -> b) -> (a -> c))
pure (.) <*> x :: f ((a -> b) -> (a -> c))
pure (.) <*> x <*> y :: f (a -> c)
(pure (.) <*> x <*> y) <*> z :: f c

-}
---6


---7


data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x)   = Var (f x)
    fmap _ (Val x)   = Val x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var
  ---
    _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var x = Var (f x)
  Var f <*> Add x y = Add (fmap f x) (fmap f y)
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (>>=) (Val x) _ = Val x
    (>>=) (Var x) f = f x
    Add a b >>= f = Add (a >>= f) (b >>= f)

p :: Expr Int
p=Add (Var 10) (Add (Var 2) (Var 5)) >>= \x -> Var (x * 5)
---8

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do 
                  s <- st
                  return  (g s)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     return  (f x)

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
