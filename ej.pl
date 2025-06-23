% ------------ 1 ----------
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- 
    padre(X,Z), 
    padre(Z,Y).

hijo(X,Y):- 
    padre(Y,X).
hermano(X,Y) :-
    hijo(X,Z),
    hijo(Y,Z),
    X \= Y.

descendiente(X, Y) :- padre(Y, X).              % Caso base: Y es padre de X
descendiente(X, Y) :- padre(Y, Z), descendiente(X, Z).  % Caso recursivo

%IV abuelo(juan,X).
%V hermano(pablo,X).

%----------------------- 3 -------------------
/*Ejercicio 3 ⋆
Considerar las siguientes deniciones:
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X).
i. Explicar qué sucede al realizar la consulta menorOIgual(0,X). -> Encuentra la solucion x=0 
y despues busca mas solucionses con menorOIgual(0, Y) y se queda generando infinitas soluciones(succ(Y),succ(succ(Y)))
ii. Describir las circunstancias en las que puede colgarse un programa en Prolog. 
La recursión no está bien fundada, es decir, no tiene una condición base o esta no se alcanza nunca.

El orden de las reglas y condiciones causa exploración infinita antes de llegar a una solución.

Cuando se genera una rama infinita de búsqueda sin poda o sin acotación.
Es decir, ejecutarse innitamente sin arrojar soluciones.
iii. Corregir la denición de menorOIgual para que funcione adecuadamente.*/

% Definición de número natural
natural(0).
natural(suc(X)) :- natural(X).

/*
  menorOIgual(X, Y) significa que X ≤ Y

  Para que funcione correctamente y no se cuelgue,
  primero colocamos la base: X ≤ X si X es natural,
  luego la recursión: X ≤ suc(Y) si X ≤ Y
*/

menorOIgual(X, X) :- 
    natural(X).

menorOIgual(X, suc(Y)) :- 
    menorOIgual(X, Y).
