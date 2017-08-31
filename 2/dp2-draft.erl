-module(khf2).
-author('szepes.nori@gmail.com').
-vsn('2013-10-13').
%-export([feldarabolasa/2]).
-compile(export_all).

% felbont(L,Eleje, Vege) visszaadja az L lista két paraméter közötti elemeit, az Eleje az elsõ elem ami benne van, a Vege már nem lesz benne
felbont(L,Eleje, Vege) when(Eleje >length(L)) or  (Eleje >Vege)-> 	[];
felbont(L,Eleje, Vege) when(Eleje <1)-> felbont(L,1, Vege);
felbont(L,Eleje, Vege) -> lists:sublist(L, Eleje, Vege-Eleje).

% elso(L) visszaadja  egy lista elsõ elemet
elso(L) -> lists:nth(1, L).



value(L, Meret) when member(v(X), L) -> [X];
value(L, Meret) when member(e, L) -> lists:seq(2,Meret*Meret, 2);
value(L, Meret) when member(o, L) -> lists:seq(1,Meret*Meret, 2);
value(L, Meret) -> lists:seq(1, Meret).





% oszlop(L, Elso, Utolso, Acc) visszaadja egy mátrixnak az elsõ és utolsó paraméterek közötti oszlopait.
oszlop(L, _Elso, _Utolso, Acc) when (length(L) <1) -> Acc;
oszlop(L, Elso, Utolso, Acc) -> oszlop(lists:nthtail(1,L), Elso, Utolso, lists:append(Acc, felbont(lists:nth(1,L), Elso, Utolso))).
	
