module Cap10 where
import System.IO
import Data.Char

{-getChar :: IO Char
getChar = ...
-}
act :: IO (Char,Char)
act = do 
        x <- getChar
        y <- getChar
        return (x,y)

nameReturn  ::  IO  String 
nameReturn  =  do  
                putStr  "¿Cuál es su nombre?" 
                first  <-  getLine 
                putStr  "¿Y su apellido?" 
                last  <-  getLine 
                let  full  =  first  ++  " "  ++  last 
                putStrLn  ( "Encantado de conocer usted, "  ++  full  ++  "!" ) 
                return  full

getLine' :: IO String
getLine' = 
        do 
            x <- getChar
            if x == '\n' then
                return []
            else
                do 
                    xs <- getLine'
                    return (x:xs)

puttr' :: String -> IO()
puttr' [] = return ()
puttr' (x:xs) = 
                do 
                putChar x
                puttr' xs
        
strlen :: IO ()
strlen =do
        putStr "Enter a string: "
        xs <- getLine
        putStr "The string has "
        putStr (show (length xs))
        putStrLn "Charcters"

hangman :: IO ()
hangman = do 
            putStrLn "Think of a word:"
            word <- sgetLine
            putStrLn "Try to guess it:"
            play word

sgetLine :: IO String
sgetLine = do
            x <- getCh
            if x =='\n' then
                do
                    putChar x
                    return []
            else
                do 
                    putChar '_'
                    xs <- sgetLine
                    return (x:xs)
                
getCh :: IO Char
getCh = do 
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return x

play :: String -> IO ()
play word = do
            putStr "?"
            guess <- getLine
            if guess == word then
                putStrLn "You got it!!"
            else 
                do
                    putStrLn (match word guess)
                    play word 

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]

--- Game utilities
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO()
putRow row num = do
                    putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO()
putBoard [a,b,c,d,e] = do
                          putRow 1 a
                          putRow 2 b 
                          putRow 3 c 
                          putRow 4 d 
                          putRow 5 e 

getDigit :: String -> IO Int
getDigit prompt = do
                    putStr prompt
                    x <- getChar
                    if isDigit x then 
                        return (digitToInt x)
                    else
                        if x == '\n' then
                                getDigit prompt
                        else
                            do
                                putStrLn "ERROR : Invalid digit"
                                getDigit prompt
              
newline :: IO ()
newline = putChar '\n'

play' :: Board -> Int -> IO()
play' board player =
    do 
        newline
        putBoard board
        if finished board then
            do  
                newline    
                putStr "Player "
                putStr (show (next player))
                putStrLn "Wins!"
        else
            do
                newline
                putStr "Player"  
                putStrLn (show player)  
                row <- getDigit "Enter a row number: "
                num <- getDigit "Star to remove : "
                if valid board row num then
                    play' (move board row num) (next player)
                else
                    do
                        newline
                        putStrLn "ERROR : Invalid move"
                        play' board player
          
nim :: IO ()
nim = play' initial 1

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO()
writeat p xs = do 
                goto p
                putStr xs
goto :: Pos -> IO()
goto (x,y) =putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board' = [Pos]

glider :: Board'
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board' -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                           (x+1,y-1),(x-1,y),
                           (x+1,y),(x-1,y+1),
                           (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod`height) + 1 )

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
births b = [(x,y)| x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3 ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

life :: Board' -> IO ()
life b = do  
        cls
        showcells b
        wait 500000
        life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]


---------------------------------------

putStr' xs = sequence_ [putChar x | x <- xs]


--putBoard' = putBoard 1
r :: Int
r=1

initial' :: Board
initial' = [1,2,3,4,5,6,7,8]

putBoard' :: Int -> Board -> IO ()
putBoard' r [] = return()
putBoard' r (n:ns) = do
  putRow r n
  putBoard' (r + 1) ns

putBoard''' :: Board -> IO ()
putBoard''' = putBoard' 1

putBoard'' b = sequence_ [putRow r n | (r,n) <- zip [1..] b]
                 
suma:: Int -> Int -> IO Int
suma t n = do
            x <-getDigit " " 
            if n == 1 then
                return (t+x) 
            else 
                suma (t + x) (n - 1)

sumador :: IO ()
sumador = do 
            n <- getDigit "¿Cuántos números quieres ingresar?: " 
            if n < 1 then
                do
                putStrLn "Error, deben de ser mas de 0 números..."
                sumador
            else
                do
                    newline
                    total <- suma 0 n
                    putStr "El total es "
                    putStrLn (show total)

suma' :: Int -> IO [Int]
suma' n = sequence (replicate n (getDigit ""))   
--- sequence :: [IO a] -> IO [a]

sumador' :: IO ()
sumador' = do 
            n <- getDigit "¿Cuántos números quieres ingresar?:  "
            if n < 1 then
                do 
                    putStrLn "ERROR: Debes de tener mas de 0 numeros"
                    sumador'
            else
                do 
                    listanumeros <- suma' n
                    putStr "El total es "
                    putStrLn (show (sum listanumeros))


readLine' :: String -> IO String
readLine' cs = do 
                    c <- getCh
                    if c == '\n' then
                        do 
                            putChar c
                            return cs
                    else 
                        if c == '\DEL' then
                            do  
                              --  putChar 'a'
                                putChar '\b' 
                                putChar ' '  
                                putChar '\b' 
                                readLine' (init cs)
                        else
                            do 
                                putChar c
                                readLine' (cs ++ [c])

readLine :: IO String
readLine = readLine' []