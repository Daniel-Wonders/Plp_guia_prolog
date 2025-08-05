{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}

foldu :: t1 -> (t2 -> t1 -> t1) -> [t1 -> t2] -> t1
foldu z f []     = z
foldu z f (x:xs) = f (x (foldu z f xs)) (foldu z f xs)


foldr2::t1->(t2->t1->t1)->[t2]->t1
foldr2 base funcion lista = foldu base funcion (map const lista)

foldu2:: t1 -> (t2 -> t1 -> t1) -> [t1 -> t2] -> t1
foldu2 base funcion listaDeFunciones = foldr2 base (\cabeza rec -> funcion (cabeza rec) rec) listaDeFunciones 

foldr::(t2->t1->t1)->t1->[t2]->t1
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)