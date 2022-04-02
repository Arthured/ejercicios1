import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)


type Pos = (Int,Int)

size :: Int
size = 3
type Grid = [[Player]]

data Player = O | B | X
            deriving (Eq,Ord,Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins :: Player -> Grid -> Bool
wins p g =any line (rows ++ cols ++ dias)
        where
            line =all (==p)
            rows = g
            cols = transpose g
            dias = [diag g,diag(map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside. interleave bar . map showPlayer 
            where 
                beside = foldr1 (zipWith (++))
                bar = replicate 3 "|"


showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer X = ["   "," X ","   "]
showPlayer B = ["   ","   ","   "]


interleave:: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y:x:interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]
move g i p =if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
                    putStr prompt
                    xs <- getLine 
                    if xs /= [] && all isDigit xs then
                        return (read xs)
                    else 
                        do
                            putStrLn "Error: Invalid number"
                            getNat prompt


cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO()
goto (x,y) =putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do  
            cls
            goto (1,1)
            putGrid g
            run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins! \n"
        | wins X g = putStrLn "Player X wins! \n"
        |  full g = putStrLn "It's a draw! \n"
        |otherwise =
            do 
                i <-getNat (prompt p)
                case move g i p of
                    [] -> do 
                            putStrLn "Error: invalido "
                            run' g p
                    [g'] -> run g' (next p)


prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
{-}
newton epsilon f f' adi = let nuevo = adi - (f adi / f' adi)
                                err =  abs (nuevo - adi)
                            in if (err < epsilon)
                                  then nuevo
                                  else newton epsilon f f' nuevo
-}

data Tree a = Node a [Tree a]
                deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g     =[] 
    | full g     =[]
    | otherwise = concat [move g i p| i <- [0..((size^2)-1)]] 

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
    | wins O g = Node (g,O) []
    | wins X g = Node (g,X) []
    |otherwise = Node (g,B) []
minimax (Node g ts)
    |turn g == O = Node (g, minimum ps) ts'
    |turn g == X = Node (g, maximum ps) ts'
                    where
                        ts' = map minimax ts
                        ps = [p| Node (_,p) _<-ts']
        
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _<- ts, p' == best]
                where 
                    tree =prune depth (gametree g p)
                    Node (_,best) ts = minimax tree


findNode :: Eq a => a -> Tree a -> Tree a
findNode x t = findNode' x t []

findNode' :: Eq a => a -> Tree a -> [Tree a] -> Tree a
findNode' x (Node y []) [] = Node y []
findNode' x (Node y []) (sb1:sbs) =
  if x == y
    then Node y []
    else findNode' x sb1 sbs
findNode' x (Node y (t:ts)) [] =
  if x == y
    then Node y (t : ts)
    else findNode' x t ts
findNode' x (Node y (t:ts)) (sb1:sbs)
  | x == y = Node y (t : ts)
  | x == child = Node child ct
  | otherwise = Node sibling st
  where
    Node child ct = findNode' x t ts
    Node sibling st = findNode' x sb1 sbs

main :: IO ()
main = do 
            hSetBuffering stdout NoBuffering
            --play empty O
            promptSINO "¿Quieres ir primero? (s/n): "

play :: Grid -> Player -> IO ()
play g p = do
            goto(1,1)
            putGrid g
            play' g p 

play' :: Grid -> Player -> IO ()
play' g p
    | wins O g =putStrLn "Jugador O gano"
    |wins X g  =putStrLn "Jugador X gano"
    |full g    =putStrLn "Empate"
    | p == O  = do 
                    i <- getNat (prompt p)
                    case move g i p of
                        [] -> do
                                putStrLn "ERROR novimiento invalido"
                                play' g p
                        [g'] -> play g' (next p)
    | p== X    =do
                    cls
                    putStrLn "Jugador X esta pensando..."
                    (play $! (bestmove'' g p)) (next p)

-------- EJERCICIOS------------------          
arbol :: Tree Grid
arbol = gametree empty O

nodos ::  Tree a -> Int
nodos (Node _ ts) = 1 + sum ( map nodos ts)

profundo :: Tree a -> Int
profundo (Node _ []) = 0
profundo (Node _ ts) = 1 + maximum (map profundo ts)
      
bestmove' :: Grid -> Player -> [Grid]
bestmove' g p = [g' | Node (g',p') _<- ts, p' == best]
                where 
                    tree =prune depth (gametree g p)
                    Node (_,best) ts = minimax tree
---Ejercicio 3
bestmove'' :: Grid -> Player -> Grid
bestmove'' g p = head [g' | Node (g', p') _ <- sortOn profundo ts, p' == best]
                where
                    tree = prune depth (gametree g p)
                    Node (_, best) ts = minimax tree
----
play'' :: Grid -> Player -> IO ()
play'' g p
    | wins O g =putStrLn "Jugador O gano"
    |wins X g  =putStrLn "Jugador X gano"
    |full g    =putStrLn "Empate"
    | p == O  = do 
                    i <- getNat (prompt p)
                    case move g i p of
                        [] -> do
                                putStrLn "ERROR novimiento invalido"
                                play'' g p
                        [g'] -> play g' (next p)
    | p== X    =do
                    cls
                    putStrLn "Jugador X esta pensando..."
                    cls
                    let gs = bestmove' g p 
                    n <- randomRIO (0, (length gs)-1)
                    putStr "\n"
                    play (gs !! n) (next p)


promptSINO :: String -> IO ()
promptSINO prompt = do
    putStr prompt
    ans <- getChar
    if ans == 's' then
        jugar empty O
    else if ans == 'n' then
        jugar empty X
    else
        do 
            putStrLn "ERROR: Por favor presione 's' o 'n'"
            cls
            promptSINO prompt

---------4c

gametree' :: Grid -> Player -> Tree Grid
gametree' g p = Node g [gametree' g' (next p) | g' <- moves g p]

bestmov :: Grid -> Player -> Tree Grid -> Grid
bestmov g p gt = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = findNode g gt
    Node (_, best) ts = minimax tree

jugar :: Grid -> Player -> IO ()
jugar g p = do
                cls
                goto (1, 1)
                putGrid g
                jugar' g p (gametree' g p)

jugar' :: Grid -> Player -> Tree Grid -> IO ()
jugar' g p gt
            | wins O g = putStrLn "Jugador O gana!\n"
            | wins X g = putStrLn "Jugador X gana!\n"
            | full g = putStrLn "Empate!\n"
            | p == O = do
                i <- getNat (prompt p)
                case move g i p of
                  [] -> do
                    putStrLn "ERROR: Movimiento invalido"
                    jugar' g p gt
                  [g'] -> jugar g' (next p)
            | p == X = do
                    putStr "Jugador X esta pensando... "
                    (jugar $! bestmov g p gt) (next p)



