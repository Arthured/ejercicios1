module Prueba where
double x= x+x
quadruple x =double(double x)
--div  =x/y
suma []=0
suma (n:ns)=n+suma(ns)
--average :: int -> float
average ns = sum ns / length ns
