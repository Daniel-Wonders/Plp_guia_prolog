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

sublista([],[]).
sublista([X|XS], [X|Lista]):-
    sublista(XS,Lista).
sublista([_|XS], Lista):-
    sublista(XS,Lista).


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
false.*/
%!
parteQueSuma(XS,Suma,Res):- 
    subconjunto(XS,Res),
    sumatoria(Res,Suma).

subconjunto([], []).
subconjunto([X|XS], [X|YS]) :- subconjunto(XS, YS). %usa un elemento de xs
subconjunto([_|XS], YS) :- subconjunto(XS, YS).     %o no lo usa, pero en ambos casos sigue recorriendo

sumatoria([], 0).
sumatoria([X|XS], Suma) :-
    Suma > 0,
    between(0, Suma, X),
    Restante is Suma - X,
    sumatoria(XS, Restante).

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

raiz(bin(_,R,_),R).

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

/*Denir los siguientes predicados, utilizando la representación de árbol binario denida en el ejercicio 11:
i. inorder(+AB,-Lista), que tenga éxito si AB es un árbol binario y Lista la lista de sus nodos según el
recorrido inorder.
ii. arbolConInorder(+Lista,-AB), versión inversa del predicado anterior.
iii. aBB(+T), que será verdadero si T es un árbol binario de búsqueda.
iv. aBBInsertar(+X, +T1, -T2), donde T2 resulta de insertar X en orden en el árbol T1. Este predicado ¾es
reversible en alguno de sus parámetros? Justicar.*/

inorder(nil,[]).
inorder(bin(Izq,V,Der), Res):-
    inorder(Izq,ElemsIzq),
    inorder(Der,ElemsDer),
    append(ElemsIzq,[V|ElemsDer],Res).


inorder2(nil,[]).
inorder2(bin(Izq,V,Der), Res):-
    inorder(Izq,ElemsIzq),
    inorder(Der,ElemsDer),
    append([V,ElemsDer],ElemsIzq,Res). %no anda este, alta paja

%!aBB(+Arbol)
aBB(nil).
aBB(Arbol):- 
    inorder(Arbol,Res),
    esListaOrdenada(Res).

esListaOrdenada([]).
esListaOrdenada([_]).
esListaOrdenada([X, Y | Resto]) :-
    X =< Y,
    esListaOrdenada([Y | Resto]).

%!aBBInsertar(+X,+T1,-T2)
aBBInsertar(Elem,Arbol,Arbol):-
    inorder(Arbol, Lista),
    member(Elem, Lista),!.

aBBInsertar(Elem,nil,bin(nil,Elem,nil)).

% Caso 3: Elem es menor que la raíz, insertamos a la izquierda
aBBInsertar(Elem, bin(Izq, Raiz, Der), bin(NuevoIzq, Raiz, Der)) :-
    Elem < Raiz,
    aBBInsertar(Elem, Izq, NuevoIzq).

% Caso 4: Elem es mayor que la raíz, insertamos a la derecha
aBBInsertar(Elem, bin(Izq, Raiz, Der), bin(Izq, Raiz, NuevoDer)) :-
    Elem > Raiz,
    aBBInsertar(Elem, Der, NuevoDer).

%------------------- 13 -----------

/*Denir el predicado coprimos(-X,-Y), que genere uno a uno todos los pares de números naturales coprimos
(es decir, cuyo máximo común divisor es 1), sin repetir resultados. Usar la función gcd del motor aritmético.*/

%coprimos(X,Y):-  gcd(X,Y,1). % seria esto pero parece que no anda gcd lol

%--------------------- 14 -------------------

/*Un cuadrado semi-mágico es una matriz cuadrada de naturales (incluido el cero) donde todas las las de la
matriz suman lo mismo. Por ejemplo:
1 3 0
2 2 0 todas las las suman 4
1 1 2
Representamos la matriz como una lista de las, donde cada la es una lista de naturales. El ejemplo anterior
se representaría de la siguiente manera: [[1,3,0],[2,2,0],[1,1,2]].
i. Denir el predicado cuadradoSemiMágico(+N, -XS). El predicado debe ir devolviendo matrices 
(utilizando la representación antes mencionada), que sean cuadrados semi-mágicos de dimensión N*N. Dichas
matrices deben devolverse de manera ordenada: primero aquellas cuyas las suman 0, luego 1, luego 2,
etc. No es necesario utilizar la técnica Generate & Test.
Ejemplo: cuadradoSemiMágico(2,X). devuelve:
X = [[0, 0], [0, 0]] ;
X = [[0, 1], [0, 1]] ;
X = [[0, 1], [1, 0]] ;
X = [[1, 0], [0, 1]] ;
X = [[1, 0], [1, 0]] ;
X = [[0, 2], [0, 2]] ;
etc.
ii. Denir utilizando Generate & Test el predicado cuadradoMagico(+N, -XS), que instancia XS con cuadrados cuyas las y columnas suman todas un mismo valor.*/

%TBA

%---------------- 15 ------------------

/*En este ejercicio trabajaremos con triángulos. La expresión tri(A,B,C) denotará el triángulo cuyos lados tienen
longitudes A, B y C respectivamente. Se asume que las longitudes de los lados son siempre números naturales.
Implementar los siguientes predicados:
i. esTriángulo(+T) que, dada una estructura de la forma tri(A,B,C), indique si es un triángulo válido.
En un triángulo válido, cada lado es menor que la suma de los otros dos, y mayor que su diferencia (y
obviamente mayor que 0).
ii. perímetro(?T,?P), que es verdadero cuando T es un triángulo (válido) y P es su perímetro. No
se deben generar resultados repetidos (no tendremos en cuenta la congruencia entre triángulos: si
dos triángulos tienen las mismas longitudes, pero en diferente orden, se considerarán diferentes entre sí). 
El predicado debe funcionar para cualquier instanciación de T y P (ambas instanciadas, ambas sin instanciar, 
una instanciada y una no; no es necesario que funcione para triángulos parcialmente instanciados), debe generar todos los resultados válidos 
(sean nitos o innitos), y no debe colgarse (es decir, no debe seguir ejecutando innitamente sin producir nuevos resultados). Por ejemplo:
?- perímetro(tri(3,4,5),12). → true.
?- perímetro(T,5). → T = tri(1, 2, 2) ; T = tri(2, 1, 2) ; T = tri(2, 2, 1) ; false.
?- perímetro(tri(2,2,2),P). → P = 6.
?- perímetro(T,P). → T = tri(1, 1, 1), P = 3 ; T = tri(1, 2, 2), P = 5 ; . . .
iii. triángulo(-T), que genera todos los triángulos válidos, sin repetir resultados.*/

tri(1,2,2).
ladoValido(A,B,C):-
    A > 0, B > 0, C > 0,
    A < (B + C),
    A > (B - C).
%!esTriángulo(+T)
esTriangulo(tri(A,B,C)):-
    ladoValido(A,B,C),
    ladoValido(B,A,C),
    ladoValido(C,A,B).


%!perímetro(?T,?P)
perimetro(Tri,_):-
    generarPerimetro(3,P),
    generarTriangulo(P, Tri),
    esTriangulo(Tri).

%!generarPerimetro(+P,?P)
generarPerimetro(P,P).
generarPerimetro(P,P2):-
    Paux is P + 1,
    generarPerimetro(Paux,P2).

% generarTriangulo(+P, ?tri(A,B,C))
generarTriangulo(P, tri(A,B,C)) :-
    % 1) Reparto de P en tres lados ≥1
    MaxA is P - 2,
    between(1, MaxA, A),
    Rem1 is P - A,
    MaxB is Rem1 - 1,
    between(1, MaxB, B),
    C is P - A - B,
    % 2) Poda mínima: desigualdades de triángulo
    A + B > C,
    A + C > B,
    B + C > A.

%!triángulo(-T)
triangulo(Tri):-perimetro(Tri,_).

%--------------- 16 -------------
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).
leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X),!, leGusta(Y).
/*                         cucurucho(X,Y)
                              |
                       leGusta(X) [A]
                              |
           frutal(X),cremoso(X) succeed?
                      /       \
             X = frutilla    backtrack frutal→banana
                 |                    |
            leGusta(frutilla)         frutal(X),cremoso(X) succeed?
                 |                  /           \
             succeeds           X = banana   backtrack banana→manzana
                 |                 |
        ┌────────┴────────┐        manzana es frutal sí, pero
        │                 │        cremoso(manzana) falla → backtrack
        |                 |
[1] leGusta(X) ends      back to [A]: no more frutal → overall fail
   with X=frutilla

   └──> now Y for X=frutilla
            leGusta(Y) [B]
               |
       frutal(Y),cremoso(Y)
            /       \
    Y=frutilla     backtrack frutal→banana
        |                 |
    cremoso(frutilla)   cremoso(banana)
        |                 |
 [Solution 1]         [Solution 2]
 X=frutilla,Y=frutilla  X=frutilla,Y=banana
        |                 |
 backtrack [B]           backtrack [B]
        |                 |
 no more frutal → back to [A]

── back to A for second branch:
 X = banana
      |
   leGusta(banana)
      |
 succeeds
      |
 now Y for X=banana:
   leGusta(Y) [C]
      |
 frutal(Y),cremoso(Y)
      /       \
 Y=frutilla  backtrack frutal→banana
      |            |
 cremoso(frutilla) cremoso(banana)
      |            |
[Solution 3]    [Solution 4]
X=banana,Y=frutilla X=banana,Y=banana
      |            |
 backtrack [C]    backtrack [C]
      |            |
 no more frutal → back to [A]
      | 
 backtrack [A] 
      |
 no more frutal → end */

 %---------------------- 17 ---------------

 /*i. Sean los predicados P(?X) y Q(?X), ¾qué signica la respuesta a la siguiente consulta?
?- P(Y), not(Q(Y)).
Sea un Y tal que P(Y) sea true pero Q(Y) sea false, ej Y=1 P=Impar(X), Q=Par(X)
ii. ¾Qué pasaría si se invirtiera el orden de los literales en la consulta anterior?
Cuando ve esto:

not(par(Y))
Con Y libre, intenta demostrar que no existe ningún Y que haga par(Y) cierto.
Como par(0) es cierto → entonces not(par(Y)) falla y nunca se evalúa impar(Y).

iii. Sea el predicado P(?X), ¾Cómo se puede usar el not para determinar si existe una única Y tal que P(?Y)
es verdadero?
unicoP(Y) :-
    P(Y),
    not((P(X), X \= Y)).
*/

% ---------------- 18 ----------------

/*Denir el predicado corteMásParejo(+L,-L1,-L2) que, dada una lista de números, realiza el corte más parejo
posible con respecto a la suma de sus elementos (puede haber más de un resultado). Por ejemplo:
?- corteMásParejo([1,2,3,4,2],L1,L2). → L1 = [1, 2, 3], L2 = [4, 2] ; false.
?- corteMásParejo([1,2,1],L1,L2). → L1 = [1], L2 = [2, 1] ; L1 = [1, 2], L2 = [1] ; false.*/

% corteMasParejo(+Lista, -L1, -L2)
corteMasParejo(Lista, L1, L2) :-
    generarDiferencia(0, Differencia),                 % primero generamos una diferencia (de menor a mayor)
    append(L1, L2, Lista),                   % partimos la lista en dos sublistas contiguas
    sumatoria2(L1, S1),
    sumatoria2(L2, S2),
    Differencia =:= abs(S1 - S2), !.                      % aceptamos solo si la diferencia real es la generada

% Genera naturales crecientes: 0, 1, 2, ...
generarDiferencia(P, P).
generarDiferencia(P, P2) :-
    P1 is P + 1,
    generarDiferencia(P1, P2).

sumatoria2([], 0).
sumatoria2([X|XS], S) :-
    sumatoria2(XS, SRest),
    S is X + SRest.
% ------------ 20 ---------------
/*Ejercicio 20 ⋆
Un número poderoso es un número natural m tal que por cada número primo p que divide a m, p
2
también
divide a m. Denir el predicado próximoNumPoderoso(+X,-Y) que instancie en Y el siguiente número poderoso
a partir de X. */
proximoNroPoderoso(In,Out):-
    InDoble is In*2,
    between(1,InDoble,P),
    Out is P*P,
    0 is mod(Out,P),
    Out > In ,!.

%----------- 22 ----------- 
% ————————————————————————————————
%   Consultas de ejemplo para caminoSimple/4
%   caminoSimple(+G, +Desde, +Hasta, ?Camino)
% ————————————————————————————————
%% Ejemplos que deben SÍ tener solución:
% 1) Camino directo de longitud 2: a → b → c
%?- caminoSimple(g1, a, c, [a,b,c]).
% true.
% 2) Camino más largo: a → b → d → e → f
%?- caminoSimple(g1, a, f, [a,b,d,e,f]).
% true.
% 3) Generación de todos los caminos simples de a a e:
%?- caminoSimple(g1, a, e, L).
% L = [a,b,d,e] ;
% L = [a,b,c,e] ;
% L = [a,d,e] ;
% false.
%% Ejemplos que deben FALLAR (no son caminos simples válidos):
% 4) No existe arista directa b → f
%?- caminoSimple(g1, b, f, [b,f]).
% false.
% 5) [a,b,a] no es simple (repite nodo)
%?- caminoSimple(g1, a, a, [a,b,a]).
% false.
% 6) No hay camino entre a y un nodo inexistente x
%?- esNodo(g1,x).          % false
%?- caminoSimple(g1, a, x, L).
% false.
% ————————————————————————————————
%   Base de conocimiento del grafo G1
% ————————————————————————————————

% esNodo(+Grafo, ?Nodo)
esNodo(g1, a).
esNodo(g1, b).
esNodo(g1, c).
esNodo(g1, d).
esNodo(g1, e).
esNodo(g1, f).

% arista/3 guarda cada arista en una sola orientación
arista(g1, a, b).
arista(g1, b, c).
arista(g1, b, d).
arista(g1, c, e).
arista(g1, d, e).
arista(g1, e, f).

% esArista(+Grafo, ?X, ?Y) responde indistintamente para (X,Y) o (Y,X)
esArista(G,X,Y) :- arista(G,X,Y).
esArista(G,X,Y) :- arista(G,Y,X).

%!caminoSimple(+Grafo,+Nodo inicial,+Nodo final,?Lista de nodos) 
% caminoSimple(+Grafo, +Inicio, +Fin, ?Camino)
caminoSimple(G, D, H, Camino) :-
    esNodo(G, D),
    esNodo(G, H),
    camino(G, D, H, [D], CaminoReverso),
    reverse(CaminoReverso, Camino).

% camino(+Grafo, +Actual, +Destino, +Visitados, -Camino)
% Invariante: Actual ya está en Visitados
camino(_, H, H, Visitados, Visitados).
camino(G, Actual, Destino, Visitados, Camino) :-
    esArista(G, Actual, Siguiente),
    not(member(Siguiente, Visitados)),  % evita ciclos
    camino(G, Siguiente, Destino, [Siguiente|Visitados], Camino).

% inciso b: Hamiltoniano
hamiltoniano(Grago,Camino):-
    head(Camino,Nodo1),
    ultimo(Camino,NodoF),
    caminoSimple(Grago,Nodo1,NodoF,Camino),
    todosLosNodos(Grago,Nodos),
    length(Nodos,Largo),
    length(Camino,Largo). 
    
head([X],X).
head([X|_],X).

ultimo([X],X).
ultimo([_|XS],Resto):-
    ultimo(XS,Resto).

caminoValido(_,[_]).
caminoValido(Grafo,[X,Y|Resto]):-
    esArista(Grafo,X,Y),
    caminoValido(Grafo,[Y|Resto]).

%% todosLosNodos(+Grafo,‑ListaNodos)
%% ListaNodos es la lista de todos los nodos de Grafo, sin repeticiones.
todosLosNodos(G,L) :-
    todosNodosAcc(G,[],Acc),      % acumula sin repetidos
    reverse(Acc,L).               % y al final invierte para respetar orden

%% Caso de parada: no hay más nodos nuevos que agregar
todosNodosAcc(G,Acc,Acc) :-
    \+ ( esNodo(G,N),            % no existe un N que
         \+ member(N,Acc)     % sea nodo y no esté ya en Acc
       ).

%% Paso recursivo: encuentra un nodo N que aún no esté en Acc, lo añade
todosNodosAcc(G,Acc,Res) :-
    esNodo(G,N),
    \+ pertenece(N,Acc),
    todosNodosAcc(G,[N|Acc],Res).
%------------- 23 ----------

/*Trabajaremos con árboles binarios, usando nil y bin(AI, V, AD) para representarlos en Prolog.
i. Implementar un predicado arbol(-A) que genere estructuras de árbol binario, dejando los valores de los nodos sin instanciar.
Deben devolverse todos los árboles posibles (es decir, para toda
estructura posible, el predicado debe devolverla luego de un número nito de pedidos). No debe devolverse dos veces el mismo árbol.
? arbol(A).
A = nil ;
A = bin(nil, _G2388, nil) ;
A = bin(nil, _G2391, bin(nil, _G2398, nil)) ;
A = bin(bin(nil, _G2398, nil), _G2391, nil) ;*/
arbol(Arbol):-
    generarAltura(0,Altura),
    arbolConXNodos(Arbol,Altura).

arbolConXNodos(Arbol,Cant):-
    generarArbol(Arbol),
    cantNodos(Arbol,Cant).

cantNodos(nil,0).
cantNodos(bin(I,_,D),Res):-
    cantNodos(I,R1),
    cantNodos(D,R2),
    Res is 1+ R1 + R2.

generarArbol(nil).
generarArbol(bin(I,_,D)):-
    generarArbol(I),
    generarArbol(D).

generarAltura(N,N).
generarAltura(P,P2):-
    P1 is P + 1,
    generarAltura(P1, P2).
/*
arbol_de_altura(0, nil).
arbol_de_altura(H, bin(I, _, D)) :-
  H > 0,
  H1 is H - 1,
  % Si el árbol tiene altura H, uno de los subárboles
  % puede tener altura H1 y el otro ≤ H1, o viceversa:
  arbol_de_altura(H1, I),
  arbol_de_altura(Otra, D),
  Otra =< H1.


altura2(nil,0).
altura2(bin(I,_,D),Res):-
    altura2(I,A1),
    altura2(D,A2),
    Res is 1+ max(A1,A2).

ii. Implementar un predicado nodosEn(?A, +L) que es verdadero
cuando A es un árbol cuyos nodos pertenecen al conjunto conjunto
de átomos L (representado mediante una lista no vacía, sin orden
relevante y sin repetidos). Puede asumirse que el árbol se recibe
instanciado en su estructura, pero no necesariamente en sus nodos.
? arbol(A), nodosEn(A, [ka, pow]).
A = nil ;
A = bin(nil, ka, nil) ;
A = bin(nil, pow, nil) ;
A = bin(nil, ka, bin(nil, ka, nil)) ;
A = bin(nil, ka, bin(nil, pow, nil)) ;*/

/*iii. Implementar un predicado sinRepEn(-A, +L) que genere todos los árboles cuyos nodos pertenezcan al alfabeto L y usando como máximo una
vez cada símbolo del mismo. En este caso, no hay innitos árboles posibles; es importante que el predicado no devuelva soluciones repetidas y
que no se quede buscando indenidamente una vez terminado el espacio
de soluciones.
? arbolSinRepEn(A, [ka, pow]).
A = nil ;
A = bin(nil, ka, nil) ; ...
A = bin(nil, ka, bin(nil, pow, nil)) ;
... ;
No.*/
% Parcial 2024 2c
subsecuencia([],_).
subsecuencia([X|XS],[X|Resto]):-
    subsecuencia(XS,Resto).
subsecuencia([_|XS],Resto):-
    subsecuencia(XS,Resto).

subsecuenciaCreciente2(Lista,Res):-
    subsecuencia(Lista,Res),
    esCreciente(Res).

esCreciente([_]).
esCreciente([X,Y|XS]):-
    X=<Y,
    esCreciente([Y|XS]).

subsecuenciaCrecienteMasLarga2(L, Res) :-
    subsecuenciaCreciente2(L, Res),
    length(Res, LargoF),
    not((
      subsecuenciaCreciente2(L, Otro),
      length(Otro, LargoO),
      LargoO > LargoF
    )).




fibonacci2(N) :-
    fibonacciAux(1, 1, N).

fibonacciAux(_, A, A).

fibonacciAux(A, B, Res) :-
    Sum is A + B,
    fibonacciAux(B, Sum, Res).

%No es reversible, pues para los valores que no estan en la secuencia de fibonacci, 
% va a intentar matchear infinitamente sin exito en el caso base de fiboAux


%------------------ parcial 2C 2024

creciente([]).
creciente([_]).
creciente([X,Y|XS]) :-
    X =< Y,
    creciente([Y|XS]).

%!subsecuenciaCreciente(+L,-S) %
subsecuenciaCreciente(Lista, Res):-
    sublista(Lista,Res),
    sort(Res,Res).

%!subsecuenciaCrecienteMasLarga(+L,-S)
subsecuenciaCrecienteMasLarga(Lista,Res):-
    subsecuenciaCreciente(Lista,Res),
    length(Res,ResLargo),
    not((
        subsecuenciaCreciente(Lista,Otra),
        length(Otra,OtraLargo),
        OtraLargo > ResLargo
    )).

% fibonacci(-X): genera todos los números de Fibonacci (con repeticiones al principio)
fibonacci(X) :- fibGen(1, 1, X).

% fibGen(A, B, X): genera los elementos de la sucesión partiendo desde A y B
fibGen(A, _, A).          % Primer número
fibGen(_, B, B).          % Segundo número
fibGen(A, B, X) :-        % Resto de la sucesión
    S is A + B,
    fibGen(B, S, X).
/*No, el predicado fibonacci(X) no es reversible en general, porque depende de is, que no es reversible.

✔️ Funciona bien si X es una variable (para generar valores).
❌ Falla o da error si X está instanciado con un número que no está en la secuencia (porque is no puede deshacerse).*/


%---------- Parcial 2 ------------

% Estudiantes


% Notas: lista de triplas (Estudiante, Materia, Nota)
% Estudiantes
estudiante(ana).
estudiante(luis).
estudiante(pedro).

% Lista de notas (fijate que es UNA lista en un único hecho)
notas([
    (ana, algebra, 2),
    (ana, algebra, 5),
    (ana, analisis, 3),
    (ana, analisis, 4),
    (ana, quimica, 2),
    (luis, algebra, 3),
    (luis, algebra, 2),
    (luis, analisis, 7),
    (pedro, fisica, 4),
    (pedro, fisica, 6)
]).



%% pertenece(+Elemento, +Lista)
pertenece(Elemento, [Elemento|_]).
pertenece(Elemento, [_|Resto]) :-
    pertenece(Elemento, Resto).

%% a) tieneMateriaAprobada(+Estudiante, +Materia)
tieneMateriaAprobada(Estudiante, Materia) :-
    estudiante(Estudiante),
    notas(ListaDeNotas),
    pertenece((Estudiante, Materia, Calificacion), ListaDeNotas),
    Calificacion >= 4.

%% b) eliminarAplazos(+Notas, -NotasFiltradas)
eliminarAplazos([], []).
% 1) Si está aprobado, lo conservo
eliminarAplazos([(Est, Mat, Cal)|Resto], [(Est, Mat, Cal)|Filtrado]) :-
    Cal >= 4,
    eliminarAplazos(Resto, Filtrado).
% 2) Si es aplazo pero existe un aprobado posterior, lo elimino
eliminarAplazos([(Est, Mat, Cal)|Resto], Filtrado) :-
    Cal < 4,
    pertenece((Est, Mat, CalAprob), Resto),
    CalAprob >= 4,
    eliminarAplazos(Resto, Filtrado).
% 3) Si es aplazo y no hay aprobado posterior, lo conservo
eliminarAplazos([(Est, Mat, Cal)|Resto], [(Est, Mat, Cal)|Filtrado]) :-
    Cal < 4,
    not((
      pertenece((Est, Mat, CalAprob), Resto),
      CalAprob >= 4
    )),
    eliminarAplazos(Resto, Filtrado).

%% c) promedio(+Estudiante, -Promedio)
promedio(Estudiante, Promedio) :-
    notas(ListaDeNotas),
    filtrarPorEstudiante(Estudiante, ListaDeNotas, NotasEstudiante),
    eliminarAplazos(NotasEstudiante, NotasSinAplazos),
    sumarCalificaciones(NotasSinAplazos, 0, SumaTotal),
    length(NotasSinAplazos, CantidadMaterias),
    CantidadMaterias > 0,
    Promedio is SumaTotal / CantidadMaterias.

% filtrarPorEstudiante(+Estudiante, +TodasLasNotas, -NotasDelEstudiante)
filtrarPorEstudiante(_, [], []).
filtrarPorEstudiante(Est, [(Est, Mat, Cal)|Resto], [(Est, Mat, Cal)|Filtrado]) :-
    filtrarPorEstudiante(Est, Resto, Filtrado).
filtrarPorEstudiante(Est, [(Otro, _, _)|Resto], Filtrado) :-
    Est \= Otro,
    filtrarPorEstudiante(Est, Resto, Filtrado).

% sumarCalificaciones(+Notas, +Acumulador, -SumaFinal)
sumarCalificaciones([], Acum, Acum).
sumarCalificaciones([(_, _, Cal)|Resto], Acum, SumaFinal) :-
    Acum1 is Acum + Cal,
    sumarCalificaciones(Resto, Acum1, SumaFinal).

%% d) mejorEstudiante(-Mejor)
mejorEstudiante(Mejor) :-
    estudiante(Mejor),
    promedio(Mejor, PromMejor),
    not((
      estudiante(Otro),
      Otro \= Mejor,
      promedio(Otro, PromOtro),
      PromOtro > PromMejor
    )).


% Parcial Collatz y rotacion

esRotacion(Entrada,Salida):-
    append(Entrada,Entrada,Doble),
    sublista(Doble, Salida),
    length(Entrada,Largo),
    length(Salida,Largo).


collatz(N, N).

collatz(N, X) :-
    N > 1,
    0 is N mod 2,
    N2 is N / 2,
    collatz(N2, X).

collatz(N, X) :-
    N > 1,
    1 is N mod 2,
    N2 is 3 * N + 1,
    collatz(N2, X).

% No, no es reversible, Prolog no puede “deshacer” las operaciones mod/2 ni // ni la suma 3*N+1

collatzMayor(In,Out):-
    collatz(In,Out),
    not((
        collatz(In,Otro),
        Otro>Out
    )).

%----------- 2024 1C -----------
estudiante2(ana).
estudiante2(luis).
estudiante2(pedro).

% Lista de notas (fijate que es UNA lista en un único hecho)
notas2([
    (ana, algebra, 2),
    (ana, algebra, 5),
    (ana, analisis, 3),
    (ana, analisis, 4),
    (ana, quimica, 2),
    (luis, algebra, 3),
    (luis, algebra, 2),
    (luis, analisis, 7),
    (pedro, fisica, 2),
    (pedro, fisica, 6)
]).

tieneMateriaAprobada2(Est,Mat):-
    estudiante2(Est),
    notas2(Notas),
    member((Est,Mat,Nota),Notas),
    Nota >=4.



eliminarAplazos2([],[]).
eliminarAplazos2([(Est,Mat,Nota)|Resto],[(Est,Mat,Nota)|Res]):-
    Nota < 4,
    not(tieneMateriaAprobada2(Est,Mat)),
    eliminarAplazos2(Resto,Res).
eliminarAplazos2([(Est,Mat,Nota)|Resto],[(Est,Mat,Nota)|Res]):-
    Nota >= 4,
    eliminarAplazos2(Resto,Res).

eliminarAplazos2([(Est,Mat,Nota)|Resto],Res):-
    Nota <4,
    tieneMateriaAprobada2(Est,Mat),
    eliminarAplazos2(Resto,Res).



promedio2(Est,Res):-
    notas2(NotasCrudo),
    eliminarAplazos2(NotasCrudo,NotasBien),
    acumulador(Est,0,0,NotasBien,Res).

acumulador(_,Acum,Cont,[],Res):-
        Res is Acum/Cont.
acumulador(Est,Acum,Cont,[(Est,_,Nota)|Resto],Res):-
    AcumRes is Acum + Nota,
    ContRes is Cont + 1,
    acumulador(Est, AcumRes,ContRes,Resto,Res).
acumulador(Est,Acum,Cont,[(OtroEst,_,_)|Resto],Res):-
    Est \= OtroEst,
    acumulador(Est, Acum,Cont,Resto,Res).

mejorEstudiante2(Est):-
    estudiante2(Est),
    promedio2(Est,Prom),
    not((
        estudiante2(OtroEst),
        OtroEst\=Est,
        promedio2(OtroEst,Otro),
        Otro >Prom
    )).



% ------------- 2024 1C --------

generarCapicuas(Lista) :-
    desde2(1,Largo),
    listaDeLargoN(Largo,Lista),
    esCapicua(Lista).

esCapicua(XS):-
    append(Izq,Der,XS),
    reverse(Izq,Der).
esCapicua(XS):-
    append(Izq,[_|Der],XS),
    reverse(Izq,Der).

listaDeLargoN(0, []).
listaDeLargoN(N, [X|XS]) :-
    N > 0,
    between(1, 9, X),
    N1 is N - 1,
    listaDeLargoN(N1, XS).

desde2(N,N).
desde2(N,N2):-
    NAux is N+1,
    desde2(NAux,N2).

tokenizar(_,[],[]).
tokenizar(Dict,Frase,[Pal|XS]):-
    append(Pal,Resto,Frase),
    member(Pal,Dict),
    tokenizar(Dict,Resto,XS).


%!mayorCantPalabras(+D,+F,-T) 
mayorCantPalabras(Dict, Frase, Res) :-
    tokenizar(Dict, Frase, Res),
    length(Res, Largo),
    not((
        tokenizar(Dict, Frase, Otro),
        length(Otro, OtroL),
        OtroL > Largo
    )).
