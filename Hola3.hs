module Hola3 where

holamundo :: IO ( )
holamundo= putStrLn "Hola mundo"
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <x]
                    larger = [b | b <- xs, b> x]
suma []=0
suma (n:ns)=n+suma(ns)

producto [] = 1
producto (n:ns) = n*producto(ns)