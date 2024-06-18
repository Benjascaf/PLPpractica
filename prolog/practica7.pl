natural(cero).
natural(suc(X)) :- natural(X). 
mayorOIgual(X,X) :- natural(X).
mayorOIgual(suc(X),Y) :- mayorOIgual(X, Y).

%Ej 21
analfabeto(X) :- vivo(X), noSabeLeer(X). 
noSabeLeer(X) :- mesa(X).
noSabeLeer(X) :- delfín(X).
vivo(X) :- delfín(X).
mesa(salame).
delfín(flipper).
inteligente(flipper). 
inteligente(alan).

%Ej 22


preorder(nil,[]).
preorder(bin(I,R,D),[R|L]) :- preorder(I,LI), preorder(D,LD), append(LI,LD,L).
append([],YS,YS).
append([X|XS],YS,[X|L]) :- append(XS,YS,L).

%Ej 23
parPositivo(X,Y) :- mayor(X, 0), mayor(Y, 0).
natura(0).
natura(succ(N)) :- natura(N).
mayor(succ(X),0) :- natura(X).
mayor(succ(X),succ(Y)) :- mayor(X,Y).

%Ej 24
reduce(const * X * _, X).
reduce(id * X, X).
reduce(flip * F * X * Y, F * Y * X).
reduce(M * N, M1 * N) :- reduce(M, M1).

%Ej 25
naural(0).
naural(suc(X)) :- naural(X).
parDeNat(X,Y) :- naural(X), naural(Y).
maor(suc(X),X).
maor(suc(X),Y) :- maor(X,Y).

