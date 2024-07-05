padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).
hijo(X,Y) :- padre(Y,X).
hermano(X,Y) :- padre(Z, X), padre(Z, Y), X \= Y.
descendiente(X,Y) :- padre(Y, X).
descendiente(X,Y) :- padre(Y,Z), descendiente(X,Z).
ancestro(X,X).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).

%Ej2
vecino(X, Y, [_|Ls]) :- vecino(X, Y, Ls).
vecino(X, Y, [X|[Y|_]]).

%Ej3
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

%Ej4 juntar(?Lista1,?Lista2,?Lista3)
juntar([], Y, Y).
juntar([X|Xs], Ys, [X|Ls]):- juntar(Xs, Ys, Ls).

%Ej5 
%I last(?L, ?U)
last(X, Y) :- append(_, [Y], X).

%II reverse(+L, -L1)
revers([], []).
revers([X], [X]).
revers([H|L], L1) :- revers(L, T), append(T, [H], L1).

%III
prefijo(Xs, L) :- append(Xs, _, L).

%IV
sufijo(Xs, L) :- append(_, Xs,L).

%V
sublista([], _).
sublista(S, L) :- prefijo(P, L), sufijo(S,P), S \= [].

%VI
pertenece(X, L) :- prefijo(S, L), append(_, [X], S).

%Ej 6
aplanar([], []).
aplanar([X|Ls], L) :- aplanar(X, R), aplanar(Ls, Rl), append(R, Rl, L).
aplanar([X|Ls], [X|L]) :- no_es_lista(X), aplanar(Ls, L).

no_es_lista(X) :- X \= [], X \= [_|_].

%Ej 7
%I palindromo(?L, ?L1)
palindromo(L, L1) :- prefijo(L, L1), reverse(L, Lr), append(L, Lr, L1).

%iesimo(?I, +L, -X) ESTA MAAAAAL
% iesimo(I, L, X) :- sufijo(Suf, L), append(Suf2, [X], Suf), length(Suf, LongSuf), I is LongSuf.


%eJ8
%Interseccion(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X|L1], L2, L3) :- not(member(X, L2)), interseccion(L1, L2, L3).
interseccion([X|L1], L2, [X|L3]) :- member(X, L2), interseccion(L1, L2, L3). 

%partir(N, L, L1, L2) 
partir(N, L, L1, L2) :- length(L, Len), append(L1, L2, L), length(L1, N),  N =< Len.


%borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([], _, []).
borrar([X|ListaOriginal], X, ListaSinXs) :- borrar(ListaOriginal, X, ListaSinXs), not(member(X, ListaSinXs)).
borrar([Y|ListaOriginal], X, [Y|ListaSinXs]) :- Y \= X, borrar(ListaOriginal, X, ListaSinXs). 

%III sacarDuplicados(+L1, -L2).
sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :- member(X, L1), sacarDuplicados(L1, L2).
sacarDuplicados([X|L1], [X|L2]) :- sacarDuplicados(L1, L2), not(member(X, L1)).

%IV permutacion(+L1, ?L2) (Si L2 estuviese instanciada podria chequear que cada uno de sus elementos pertenexca?)
insertar(X,L1,L2) :- append(I, D, L1), append(I, [X|D], L2).
permutacion(_, []).
permutacion([X|L1], L2) :- permutacion(L1, P), insertar(X,P,L2).

%V reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(L, N, [L2|LListas]) :- N > 0, append(L2, L3, L), N2 is N - 1, reparto(L3, N2, LListas).

%repartoSinVacias(+L, -LListas) 
repartoSinVacias([], []).
%Si pongo el length al principio se cuelga
repartoSinVacias(L, [L2|LListas]) :- append(L2, L3, L), length(L2, Len), Len > 0,  repartoSinVacias(L3, LListas).

%Ej 9 elementosTomadosEnOrden(+L, +N, -Elementos) (NOC PQ NO FUNCA)
elementosTomadosEnOrden(_, 0, []).
elementosTomadosEnOrden([Y|L], N, [X|Elementos]) :- Y \= X, elementosTomadosEnOrden(L, N, [X|Elementos]), length(N, [X|Elementos]).
elementosTomadosEnOrden([X|L], N, [X|Elementos]) :- N2 is N - 1, elementosTomadosEnOrden(L, N2, Elementos), length(Elementos, N2).

%Ej 10
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

%I X debe estar instanciado por el uso de aritmetica sobre la variable 
% Y debe no estar instanciado para que el programa identifique correctamente todos
% los numeros desde X, caso contrario, el programa se cuelga despues de indicar si el
% rango era correcto, ya que en algun momento X sobrepasa Y y se cuelga.

%II desde2(+X, ?Y)
desde2(X,X).
desde2(X, Y) :- var(Y), N is X + 1, desde2(N, Y).
desde2(X, Y) :- nonvar(Y), Y > X.

%Ej11 intercalar(?L1, ?L2, ?L3) Similar a append,
%no hace falta que todos esten inicializados, pero no tiene mucho 
%uso las respuestas 
% CONSULTAR COMO SOLUCIONAR QUE DE REPETIDOS CON L1 Y L2 DE MISMA LONG. 
intercalar([], L2, L2).
intercalar(L1, [], L1).
intercalar([X|L1], [Y|L2], [X,Y|L3]) :- intercalar(L1, L2, L3).

%Ej 12 
vacio(nil).

raiz(bin(_, R, _), R).

altura(nil, 0).
altura(bin(I, _, D), L) :- altura(I, LI), altura(D, LD), L is max(LI, LD) + 1.

cantNodos(nil, 0).
cantNodos(bin(I, _, D), N) :- cantNodos(I, NI), cantNodos(D, ND), N is NI + ND + 1.


%Ej13
%inorder(+AB, -Lista)
inorder(nil, []).
inorder(bin(I, R, D), Lista) :- inorder(I, LI), inorder(D, LD), append(LI, [R|LD], Lista).

%arbolConInorder(+Lista, -AB) NO ESTOY SEGURO QUE ESTE BIEN
arbolConInorder([], nil).
arbolConInorder(Lista, bin(I, R, D)) :- append(LI, [R|LD], Lista), arbolConInorder(LI, I), arbolConInorder(LD,D).

%Abb(+T)
abb(nil).
abb(bin(nil, _, nil)).
abb(bin(nil, R, D)) :- raiz(D, RD), RD > R.
abb(bin(I, R, nil)) :- raiz(I, RI), RI < R.
abb(bin(I, R, D)) :- raiz(D, RD), raiz(I, RI), RD > R, RI < R, abb(I), abb(D).

%aBBInsertar(+X, +T1, -T2)
insertar(X, L, L2) :- sufijo(L, D), prefijo(L, I), append(I, [X|D], L2).
aBBInsertar(X, T1, T2) :- inorder(T1, L), insertar(X, L, L2), arbolConInorder(L2, T2), abb(T2).


%Ej14 coprimos(-X, -Y)
paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X.  

generarPares(X,Y) :- desde2(2,S), paresSuman(S,X,Y).

coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y) =:= 1.

%Ej15
filaSemilatina(0,0,[]).
filaSemilatina(0, N,[0|L]) :- N > 0, N2 is N -1, filaSemilatina(0, N2, L).
filaSemilatina(S, N, [X|L]) :- S > 0, N > 0, N2 is N - 1, between(0, S, X), S2 is S - X, filaSemilatina(S2, N2, L).

filasSemilatinas(_, 0, _, []).
filasSemilatinas(S, N, N2, [F|L]) :- N > 0, N3 is N - 1, filaSemilatina(S, N2, F), filasSemilatinas(S, N3, N2, L).

cuadradoSemilatino(N, L) :- desde(0, S), filasSemilatinas(S, N, N, L).

% Falta sumar columnas pero no quiero pensar
% cuadradoMagico(N, [X|XS]) :- cuadradoSemilatino(N, Xs), sum_list(X, S), columnasSuman(S, [X|Xs]).

%Ej 16
esTriangulo(tri(A,B,C)) :- A < B+C, B < A+C, C < B+A.


perimetro(tri(A,B,C),P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C),P) :- not(ground(tri(A,B,C))), armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)).



armarTriplas(P,A,B,C) :- desde2(3,P), between(0,P,A), S is P-A, between(0,S,B), C is S-B.



triangulos(T) :- perimetro(T,_).

%Ej 18
%I La consulta va a instanciar a Y de manera que "cumpla" el predicado P, y luego 
% se mete en el not, que fallara si esa instancia de Y cumple Q.

%II En este caso, el not va primero por lo que Y no es una variable instanciada
% asi que el not va a fallar si existe alguna posible instancia de Y que cumpla Q,
%lueago, si el not no falla, se instancia Y de manera que cumpla P.

%III P(Y), not((P(Y2), Y2 /= Y)).

%Ej 19
% corteMasParejo(+L,-L1,-L2)
corteMasParejo(L,L1,L2) :- unCorte(L,L1,L2,D), not((unCorte(L,_,_,D2), D2 < D)).

unCorte(L,L1,L2,D) :- append(L1,L2,L), sumlist(L1,S1), sumlist(L2,S2), D is abs(S1-S2).

%Ej 20
% masChico(X) :- P(X), not((P(Y), Y < X)).

%Ej 21
% I conjuntoDeNaturales(X) :- not((pertenece(Y, X), not(nat(Y))))

% II Se necesita que el conjunto X ya este instanciado, pues se hace uso del pertenece/2,
% pero supongamos que no fuese el caso que el mismo no es reversible, entonces conjuntoDeNaturales/1 
% no tendria la funcionalidad deseada, pues el mismo revisaria que todos los conjuntos posibles sean solo de naturales.

%III La alternativa falla porque el primer not va a revisar que no halla 
% instanciacion posible para E tal que el mismo sea natural, por lo que el mismo falla, 
% causando que el not externo siempre devuelva true.

%Ej 22 no lo hice :(

%Ej 23
% Predicados magicos:
% esNodo(G, X).
% esArista(G, X, Y).

% I 
% caminoSimple(G, D, H, L) :- aux(G, D, H, L, []).
% aux(G, H, H, [H], _).
% aux(G, D, H, [D|L], V) :- esArista(G, D, D2), not(member(D2, V)), aux(G, D2, H, L, [D|V]).

% II
% caminoHamiltoniano(G, L) :- esNodo(G, D), esNodo(G, H), esCaminoSimple(G, D, H, L), not((esNodo(G, X), not (member(X, L))))


%III no me acuerdo :(

%IV
% esEstrella(G) :- esNodo(G, V), not((esArista(G, Z, W), Z \= V, W \= V)).

%Ej 24

%Ej I PROBLEMA: Como vimos en el repaso, aca hay doble generacion infinita, por 
% lo que se va el arbol infinitamente hacia la derecha, podemos limitar por nivel/cantidad de nodos pero paja
arbol(nil).
arbol(bin(AI, _, AD)) :- arbol(AI), arbol(AD).

%Ej II 
nodosEn(nil, _).
nodosEn(bin(I, V, D), L) :- arbol(bin(I, V, D)), member(V, L), nodosEn(I, L), nodosEn(D, L).


sublistaMasLargaDePrimos(L,P) :- sublistaDePrimosDeLong(L,P,Long), not((sublistaDePrimosDeLong(L,_,Long2), Long2 > Long)).

sublistaDePrimosDeLong(L,P,Long) :- sublista(L,P), soloPrimos(P), length(P,Long).

sublista(_,[]).
sublista(L,S) :- append(P,_,L), append(_,S,P), S \= [].

soloPrimos(L) :- not((member(X,L), not(esPrimo(X)))).

% esPrimo(+P)
esPrimo(P) :- P \= 1, P2 is P-1, not((between(2,P2,D), mod(P,D) =:= 0)).


natural(suc(X)) :- natural(X).
natural(cero).


menor(cero,suc(X)) :- natural(X).
menor(suc(X),suc(Y)) :- menor(X,Y).


%entre(1,3,Z).
% Z = 1;
% Z = 2;
% Z = 3;
% false.


% entre(X,Y,X) :- X =< Y.
% entre(X,Y,Z) :- X < Y, X2 is X+1, entre(X2,Y,Z). 





% long([],0).
% long([X|XS],N) :- long(XS,N2), N is N2+1.

% scr([],[]).
% scr([X],[X]).
% scr([X,X|XS],L) :- scr([X|XS],L).
% scr([X,Y|XS],[X|L]) :- X \= Y, scr([Y|XS],L).

% partes([],[]).
% partes([X|XS],[X|L]) :- partes(XS,L).
% partes([_|XS],L) :- partes(XS,L).

% prefijo(L,P) :- append(P,_,L).

% insertar(X,L,LX) :- append(I,D,L),append(I,[X|D],LX).

% permutacion([],[])
% permutacion([X|XS],P) :- permutacion(XS,L), insertar(X,L,P).


% iesimo(0,[X|_],X).
% iesimo(I,[_|XS],X) :- iesimo(I2,XS,X), I is I2 + 1.

% desde2(X,X).
% desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
% desde2(X,Y) :- nonvar(Y), X < Y.

% pmq(X,Y) :- between(0,X,Y), Y mod 2 =:= 0.

% paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X.  

% generarPares(X,Y) :- desde2(2,S), paresSuman(S,X,Y).

% coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y) =:= 1. 


% altaMateria(plp).
% altaMateria(aa).
% altaMateria(metnum).

% liviana(plp).
% liviana(aa).
% liviana(eci).

% obligatoria(plp).
% obligatoria(metnum).

% leGusta(M) :- altaMateria(M).
% leGusta(M) :- liviana(M).

% hacer(M) :- leGusta(M), obligatoria(M).

% hacerV2(M) :- setof(X,(leGusta(X),obligatoria(X)),L), member(M,L).


% corteMasParejo(+L,-L1,-L2)
corteMasParejo(L,L1,L2) :- unCorte(L,L1,L2,D), not((unCorte(L,_,_,D2), D2 < D)).

unCorte(L,L1,L2,D) :- append(L1,L2,L), sumlist(L1,S1), sumlist(L2,S2), D is abs(S1-S2).



esTriangulo(tri(A,B,C)) :- A < B+C, B < A+C, C < B+A.


perimetro(tri(A,B,C),P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C),P) :- not(ground(tri(A,B,C))), armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)).



armarTriplas(P,A,B,C) :- desde2(3,P), between(0,P,A), S is P-A, between(0,S,B), C is S-B.



triangulos(T) :- perimetro(T,_).








%?- listaDeÁrboles(L).
%L = [];														
%L = [bin(nil,_,nil)]; ---> [1]
%L = [bin(nil,_,nil), bin(nil,_,nil)]; ---> [1,1]
%L = [bin(nil,_, bin(nil,_,nil))];  ---> [2]
%L = [bin(bin(nil _,nil),_,nil)];   ---> [2]
%L = [bin(nil,_,nil), bin(nil,_,nil), bin(nil,_,nil)]; --> [1,1,1]
%---> [1,2]
%---> [1,2]
%---> [2,1]
%---> [2,1]
%---> [3]

% desde(X,X).
% desde(X,Y) :- N is X+1, desde(N,Y).

% listaDeArboles(L) :- desde(0,S), listaAcotadaDeArboles(S,L).

% listaAcotadaDeArboles(0,[]).
% listaAcotadaDeArboles(S,[X|XS]) :- between(1,S,Na), 
% 		arbolDeN(Na,X), S2 is S-Na, 
% 		listaAcotadaDeArboles(S2,XS).


% arbolDeN(0,nil).
% arbolDeN(N,bin(I,_,D)) :- N > 0, N2 is N-1, paresQueSuman(N2,NI,ND), arbolDeN(NI,I), arbolDeN(ND,D).

% paresQueSuman(S,X,Y) :- between(0,S,X), Y is S-X.


% tamArbol(0,nil).
% tamArbol(N,bin(I,_,D)) :- tamArbol(NI,I), tamArbol(ND,D), N is 1+NI+ND.

% calcularTamanios(AS,TS) :- maplist(tamArbol,TS,AS).





% Dado el siguiente código:

% drone := Drone newWith: [:n1 :n2 | {n1+1 . n2+1}].
% drone avanzar.

% se obtiene la tabla de seguimiento de abajo.

% Objeto          Mensaje         Colaboradores       Ubicación del método    Resultado
% ------------------------------------------------------------------------------------------------
% Drone           newWith:        [:n1 :n2 ...]       Drone                   aDrone
% Drone           newWith:        [:n1 :n2 ...]       Robot                   aDrone
% Drone           new             -                   Object                  aDrone
% aDrone          initWith:       [:n1 :n2 ...]       Robot                   aDrone
% aDrone          init            -                   Drone                   aDrone
% aDrone          avanzar         -                   Drone                   aDrone
% 0               <               10                  SmallInteger            True
% true            ifTrue:         [z:=(z+1)]          True                    1
% [z:=(z+1)]      value           -                   BlockClosure            1
% 0               +               1                   SmallInteger            1
% aDrone          avanzar         -                   Robot                   aDrone
% [:n1 :n2 ...]   value: value:   0, 0                BlockClosure            #(1 1)
% 0               +               1                   SmallInteger            1
% 0               +               1                   SmallInteger            1
% #(1 1)          at:             1                   OrderedCollection       1
% #(1 1)          at:             2                   OrderedCollection       1
