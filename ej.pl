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
    generarDiferencia(0, D),                 % primero generamos una diferencia (de menor a mayor)
    append(L1, L2, Lista),                   % partimos la lista en dos sublistas contiguas
    sumatoria2(L1, S1),
    sumatoria2(L2, S2),
    D =:= abs(S1 - S2),!.                      % aceptamos solo si la diferencia real es la generada

% Genera naturales crecientes: 0, 1, 2, ...
generarDiferencia(P, P).
generarDiferencia(P, P2) :-
    P1 is P + 1,
    generarDiferencia(P1, P2).

sumatoria2([], 0).
sumatoria2([X|XS], S) :-
    sumatoria2(XS, SRest),
    S is X + SRest.

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
arbol(nil).
arbol(bin(I,_,D)):-
    arbol(I),
    arbol(D).
/*ii. Implementar un predicado nodosEn(?A, +L) que es verdadero
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
nodosEn()

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

