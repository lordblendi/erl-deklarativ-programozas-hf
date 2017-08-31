%-module(khf1).
-module(dp2).
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

% elsohossz(L) visszaadja a lista elsõ elemének a hosszát
elsohossz(L) -> length(elso(L)).

% sor(L, Hol, _Lepeskoz, Acc) felbontja a megkapott mátrixot(listát) megadott lépésközzel (sorokat felbontja és külön listába teszi õket)
sor(L, Hol, _Lepeskoz, Acc) when (Hol>length(L)) -> lists:reverse(Acc);
sor(L, Hol, Lepeskoz, Acc) -> sor(L, Hol+Lepeskoz, Lepeskoz, [felbont(L, Hol, Hol+Lepeskoz)|Acc]).

% oszlophivas(L, Lepeskoz) ez kezdeményezi a mátrix oszlopokra való bontását. meghívja az oszlopsegédet, és közben megadja neki, hogy az hányszor fusson le (elsõ sor milyen hosszú)
oszlophivas(L, Lepeskoz) -> oszlopseged(L, 1, Lepeskoz, elsohossz(L),[]).

% oszlopseged(L, Hol, Lepeskoz, Elsohossz, Acc) visszaadja a megkapott mátrix (lista) oszlopait megadott lépésközzel
oszlopseged(_L, Hol, _Lepeskoz, Elsohossz, Acc) when (Hol>Elsohossz)-> lists:reverse(Acc);
oszlopseged(L, Hol, Lepeskoz, Elsohossz, Acc) -> oszlopseged(L, Hol+Lepeskoz, Lepeskoz,Elsohossz, [oszlop(L, Hol, Hol+Lepeskoz, []) | Acc]).

% oszlop(L, Elso, Utolso, Acc) visszaadja egy mátrixnak az elsõ és utolsó paraméterek közötti oszlopait.
oszlop(L, _Elso, _Utolso, Acc) when (length(L) <1) -> Acc;
oszlop(L, Elso, Utolso, Acc) -> oszlop(lists:nthtail(1,L), Elso, Utolso, lists:append(Acc, felbont(lists:nth(1,L), Elso, Utolso))).

% matrix(Mx, R, C) 	elõször felbontja sorokra a mx-ot, majd azt átadja a matrixhelpnek
matrix(Mx, R, C) -> matrixhelp(sor(Mx, 1, R, []), C, []).

% matrixhelp(L,C,Acc) ez hívja meg a oszlopdarabóló fgv-t, megadja neki a lépésközt, majd meghívja önmagát a megkapott lista farkával
matrixhelp(L, _C, Acc) when (length(L)<1) -> Acc;
matrixhelp(L,C,Acc) -> 	matrixhelp(lists:nthtail(1,L), C, lists:append(Acc, oszlophivas(lists:nth(1,L),C))).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterû feldarabolása.
feldarabolasa(Mx, {R, C}) -> matrix(Mx, R, C).	
