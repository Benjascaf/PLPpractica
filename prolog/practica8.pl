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
sublista([X|Xs], [X|L]) :- append(Xs, _, L).
sublista(Xs, [_|L]) :- sublista(Xs, L).

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
%I cuadradoSemilatino(+N, -XS)
todosPositivos([]).
todosPositivos([X|Xs]) :- desde2(0, X), todosPositivos(Xs).
filasSemilatinas(_, []).
filasSemilatinas(N, [H|Xs]) :-  desde2(0, Sum),sum_list(H, Sum), length(H, N), filasSemilatinas(N, Xs),  Sum is N.

cuadradoSemilatino(N, Xs) :- filasSemilatinas(N, Xs), length(Xs, N).

filasSemilatinas2(0, _, []).
filasSemilatinas2(N, Sum, [H]) :- length(H, N), sum_list(H, Acc).
filasSemilatinas2(N, Acc, [H|Xs]) :- length(H, N), sum_list(H, Acc), filasSemilatinas2(N, Acc, Xs).
cuadradoSemilatino2(N, Xs) :- desde2(0, Sum), filasSemilatinas2(N, Sum, Xs), length(Xs, N).

