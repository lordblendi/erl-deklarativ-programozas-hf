# Deklaratív programozás Erlang házi

A feladat egy mátrix kisebb mátrixokra való feldarabolása, és a kis mátrixok elemeit tartalmazó listák előállítása.

A feladat paramétere egy (R,C) számpár, ahol R, ill. C a kis mátrixok sorainak, ill. oszlopainak a számát adja meg.

Egy M mátrix (R,C) paraméterű feldarabolását a következőképpen végezzük el:

 - Az első R sor után húzunk egy vízszintes elválasztó vonalat, majd minden további R sor után is.
 - Az első C oszlop után húzunk egy függőleges elválasztó vonalat, majd minden további C oszlop után is.
 - Az elválasztó vonalak által határolt minden egyes, nem üres kis mátrix elemeiből sorfolytonosan egy-egy listát képezünk.
 - A kis mátrixok elemeiből az előző pont szerint képzett listákat egyetlen listába gyűjtjük össze, tetszőleges sorrendben.

Az így előállított listák listáját nevezzük az M mátrix (R,C) paraméterű feldarabolásának.

Írjon olyan Erlang-függvényt, amely előállítja egy M mátrix (R,C) paraméterű feldarabolását!

A mátrixot  – sorok listájaként adjuk meg; az első listaelem felel meg a mátrix első sorának s.í.t. A mátrix minden egyes sorát az adott sorban levő mátrixelemek listájaként ábrázoljuk; a lista első eleme tartalmazza az adott sor első elemét s.í.t.

Feltételezheti (tehát nem kell ellenőriznie), hogy a mátrix minden sora azonos számú elemből áll; ezt a számot nevezzük a mátrix oszlopszámának. Feltételezheti, hogy a mátrix sorainak és oszlopainak a száma legalább 1. Végül feltételezheti, hogy a feldarabolás paraméterében megadott R és C mennyiségek pozitív egész számok (azaz R,C ≥ 1).

A feldarabolás eredménye egy olyan lista, amelynek elemei a bemenetként megadott mátrix elemeiből képzett, nem üres listák. Az utóbbi listák hossza nem feltétlenül egyezik meg.

## Specifikáció

Írjon Erlang-függvényt `khf1:feldarabolasa/2` néven egy tetszőleges mátrix adott paraméterű feldarabolására!

```erlang
%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterű feldarabolása.
```

A programot tartalmazó modul attribútumai ezek legyenek:
```erlang
-module(khf1).
-author(email@unit.org.hu).
-vsn('year-mm-dd').
-export([feldarabolasa/2]).
%-compile(export_all).
```

## Példák

```erlang

1> c(khf1).
{ok,khf1}
2> khf1:feldarabolasa([[a,b,c,d],
2>                     [e,f,g,h]], {2, 2}).
[[a,b,e,f],[c,d,g,h]]
3> khf1:feldarabolasa([[a,b,c,d],
3>                     [e,f,g,h],
3>                     [i,j,k,l],
3>                     [m,n,o,p]], {2, 2}).
[[a,b,e,f],
 [c,d,g,h],
 [i,j,m,n],
 [k,l,o,p]]
4> khf1:feldarabolasa([[a,b,c,d],
4>                     [e,f,g,h],
4>                     [i,j,k,l],
4>                     [m,n,o,p]], {3, 3}).
[[a,b,c,e,f,g,i,j,k],
 [d,h,l],
 [m,n,o],
 [p]]
5> khf1:feldarabolasa([[a,b,c,d],
5>                     [e,f,g,h],
5>                     [i,j,k,l],
5>                     [m,n,o,p]], {3, 2}).
[[a,b,e,f,i,j],
 [c,d,g,h,k,l],
 [m,n],
 [o,p]]
6> khf1:feldarabolasa([[a,b,c,d],
6>                     [e,f,g,h],
6>                     [i,j,k,l],
6>                     [m,n,o,p]], {2, 3}).
[[a,b,c,e,f,g],
 [d,h],
 [i,j,k,m,n,o],
 [l,p]]
7> khf1:feldarabolasa([[a,b,c,d],
7>                     [e,f,g,h],
7>                     [i,j,k,l],
7>                     [m,n,o,p]], {1, 2}).
[[a,b],
 [c,d],
 [e,f],
 [g,h],
 [i,j],
 [k,l],
 [m,n],
 [o,p]]

 ```

A kiírásnak megfelelően jók azok a programok is, amelyek a külső lista elemeit a fentiekhez képest más sorrendben adják vissza. A belső listák elemeinek a sorrendjét azonban előírtuk: minden egyes ilyen elemnek egy-egy kis mátrix elemeit **sorfolytonosan** kell felsorolnia.
