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

%--------------- 4 -------------
/*enir el predicado juntar(?Lista1,?Lista2,?Lista3), que tiene éxito si Lista3 es la concatenación de
Lista1 y Lista2. Por ejemplo:
?- juntar([a,b,c], [d,e], [a,b,c,d,e]). → true.
?- juntar([a,b,c], [d,e], L). → L = [a,b,c,d,e].
?- juntar([a,b,c], L, [a,b,c,d,e]). → L = [d,e].
?- juntar(L, [d,e], [a,b,c,d,e]). → L = [a,b,c].
?- juntar(L1, L2, [1,2,3]). → L1 = [], L2 = [1, 2, 3]; L1 = [1], L2 = [2, 3];
L1 = [1,2], L2 = [3]; L1 = [1,2,3], L2 = [].
Al igual que la mayoría de los predicados, puede dar false después de agotar los resultados.*/

% Caso base: concatenar lista vacía con L2 da como resultado L2
juntar([], L2, L2).

% Paso recursivo: agregar cabeza de L1 al resultado de concatenar el resto
juntar([X|XS], L2, [X|ZS]) :-
    juntar(XS, L2, ZS).

%---------------- 5 -----------------
/*Denir los siguientes predicados sobre listas usando append:
i. last(?L, ?U), donde U es el último elemento de la lista L.
ii. reverse(+L, ?R), donde R contiene los mismos elementos que L, pero en orden inverso.
Ejemplo: reverse([a,b,c], [c,b,a]).
Mostrar el árbol de búsqueda para el ejemplo dado.
iii. prefijo(?P, +L), donde P es prejo de la lista L.
iv. sufijo(?S, +L), donde S es sujo de la lista L.
v. sublista(?S, +L), donde S es sublista de L.
vi. pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L. (Este predicado ya viene
denido en Prolog y se llama member).
*/

%i
%!last(?L,?U)
/*last(Lista,Ultimo):-
    length(Lista,Largo),
    Xlargo is Largo -1,
    length(Xlista,Xlargo),
    append(Xlista,[Ultimo],Lista).*/
last(Lista,Ultimo):-append(_,[Ultimo],Lista).

reverse(L,R):-
