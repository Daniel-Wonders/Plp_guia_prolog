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

reverse([], []).                                         % Caso base
reverse(L, [Ultimo|Resto]) :-                                    % U es último de L
    last(L, Ultimo),                                          % Usamos tu predicado last/2
    append(Init, [Ultimo], L),                                % L = Init ++ [U]
    reverse(Init, Resto).                                   % Revertimos el resto

prefijo(Pre, Lista):-
    append(Pre,_, Lista).

sufijo(Suf,Lista):-
    append(_,Suf,Lista).

sublista(Sub, Lista) :-
    append(_, Suf, Lista),
    Suf \= [],
    append(Sub, _, Suf).

pertenece(Elem,Lista):-
    append(_,[Elem|_],Lista).

%---------------- 6 --------------

/*Denir el predicado aplanar(+Xs, -Ys), que es verdadero sii Ys contiene los elementos de todos los niveles de
Xs, en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
Ejemplos:
?- aplanar([a, [3, b, []], [2]], L).→ L=[a, 3, b, 2]
?- aplanar([[1, [2, 3], [a]], [[[]]]], L).→ L=[1, 2, 3, a]
Nota: este predicado ya está denido en prolog con el nombre flatten.*/
%!aplanar(+Xs,-Ys)
% Caso base: lista vacía
aplanar([], []).

% Caso recursivo: cabeza y cola son listas
aplanar([X|XS], R) :-
    aplanar(X, RX),        % Aplanar la cabeza
    aplanar(XS, RXS),      % Aplanar la cola
    append(RX, RXS, R).    % Combinar ambas

% Caso base: X no es lista (ni [] ni [A|B]), lo metemos en lista unitaria
aplanar(X, [X]) :-
    X \= [],
    X \= [_|_].    % Esto evita que X sea una lista (sin usar is_list)

%---------------- 7 ------------------
/*Denir los siguientes predicados, usando member y/o append según sea conveniente:
i. intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, 
respetando en L3 el orden en que aparecen los elementos en L1.
partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. Si L tiene menos de N
elementos el predicado debe fallar. ¾Cuán reversible es este predicado? Es decir, ¾qué parámetros pueden
estar indenidos al momento de la invocación?
ii. borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista
ListaOriginal.
iii. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1
iv. permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. ¾Hay una manera más eciente
de denir este predicado para cuando L2 está instanciada?
v. reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier
longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.
vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de LListas puede ser
vacía, y la longitud de LListas puede variar.*/
interseccion([], _, []).

interseccion([X|XS], Y, [X|CS]) :-
    member(X, Y),
    interseccion(XS, Y, CS).

interseccion([X|XS], Y, CS) :-
    not(member(X, Y)),
    interseccion(XS, Y, CS).

borrar([],_,[]).
borrar([X|XS],X,YS):-   
    borrar(XS,X,YS).

borrar([X|XS], Elem, [X|Resto]) :-  % Si la cabeza no es Elem, la dejamos
    X \= Elem,
    borrar(XS, Elem, Resto).

sacarDuplicados([],[]).
sacarDuplicados([X|XS],[X|YS]):-
    not(member(X,XS)),
    sacarDuplicados(XS,YS).
sacarDuplicados([X|XS],YS):-
    member(X,XS),
    sacarDuplicados(XS,YS).


permutacion([], []).  % Caso base
permutacion(L, [X|P]) :-
    member(X, L),           % Elegimos un elemento
    borrar(L, X, Resto),    % Lo quitamos de la lista original
    permutacion(Resto, P).  % Permutamos el resto

%!reparto(+L, +N, -LListas)
reparto(L, N, LListas) :-
    length(LListas, N),        % aseguramos que haya N listas
    append(LListas, L).        % su concatenación debe dar L

%!repartoSinVacías(+L, -LListas)
reparto2(L, LListas) :-
    length(LListas,_),
    append(LListas, L),
    noHayVacias(LListas).

noHayVacias([]).
noHayVacias([X|XS]):-
    X \= [],
    noHayVacias(XS).

%------------------------------ 8 ------------------------

/*Denir el predicado parteQueSuma(+L,+S,-P) que es verdadero cuando P es una lista con elementos de L que
suman S. Por ejemplo:
?- parteQueSuma([1,2,3,4,5],9,P).
P = [1, 3, 5] ;
P = [2, 3, 4] ;
P = [4, 5] ;
false.
*/
parteQueSuma(XS,Suma,Res):- 
    subconjunto(XS,Res),
    sumatoria(Res,Suma).

subconjunto([], []).
subconjunto([X|XS], [X|YS]) :- subconjunto(XS, YS). %usa un elemento de xs
subconjunto([_|XS], YS) :- subconjunto(XS, YS).     %o no lo usa, pero en ambos casos sigue recorriendo

sumatoria([], 0).
sumatoria([X|XS], Suma) :-
    sumatoria(XS, Resto),
    Suma is X + Resto.

%-------------- 9 ----------------

/*Ejercicio 9 ⋆
Considerar el siguiente predicado:
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).
i. ¾Cómo deben instanciarse los parámetros para que el predicado funcione? (Es decir, para que no se cuelgue
ni produzca un error). ¾Por qué?
ii. Dar una nueva versión del predicado que funcione con la instanciación desdeReversible(+X,?Y), tal que
si Y está instanciada, sea verdadero si Y es mayor o igual que X, y si no lo está genere todos los Y de X en
adelante.*/


%-------------- 10 -------------------

/*Denir el predicado intercalar(L1, L2, L3), donde L3 es el resultado de intercalar uno a uno los elementos
de las listas L1 y L2. Si una lista tiene longitud menor, entonces el resto de la lista más larga es pasado sin
cambiar. Indicar la reversibilidad, es decir si es posible obtener L3 a partir de L1 y L2, y viceversa.
Ejemplo: intercalar([a,b,c], [d,e], [a,d,b,e,c]).*/

intercalar([],[],[]).
intercalar([],Algo,Algo).
intercalar(Algo,[],Algo).
intercalar([X|XS],[Y|YS],[X,Y|CS]):- intercalar(XS,YS,CS).

%Es reversible, puede generarse tanto L3 desde L1 y L2 como Generar todos los L1 y L2 desde L3

%-------------- 11 ------------------

/*Un árbol binario se representará en Prolog con:
nil, si es vacío.
bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.
Denir predicados en Prolog para las siguientes operaciones: vacío, raiz, altura y cantidadDeNodos. Asumir
siempre que el árbol está instanciado.*/
% Un árbol binario de este estilo:
%
%        10
%       /  \
%      5   15
%     / \    \
%    2   7   20

ejemploArbol(bin(bin(bin(nil, 2, nil), 5,bin(nil, 7, nil)),10,bin(nil,15,bin(nil, 20, nil)))).

vacio(nil).

altura(nil, 0).

altura(bin(Izq, _, Der), Altura) :-
    altura(Izq, AltIzq),
    altura(Der, AltDer),
    maximo(AltIzq, AltDer, MaxHijos),
    Altura is MaxHijos + 1.

maximo(X, Y, X) :- X >= Y.
maximo(X, Y, Y) :- Y > X.

%!cantidadDeNodos(+Arbol,-Cantidad)
cantidadDeNodos(nil,0).
cantidadDeNodos(bin(Izq,_,Der),Res):-
    cantidadDeNodos(Izq, NodosIzq),
    cantidadDeNodos(Der, NodosDer),
    Res is NodosDer + NodosIzq + 1. 

%-------------- 12 ----------