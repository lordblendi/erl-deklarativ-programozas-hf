# Deklaratív programozás Erlang házi

A feladat egy teljesen kitöltött Sudoku-megoldás _helyességének_ ellenőrzése, azaz annak eldöntése, hogy az adott Sudoku-megoldás egy, a félévi nagy házi feladatban specifikált formájú Sudoku-feladvány által előírt összes megszorítást kielégíti-e.

A Sudoku-feladványt a nagy házi feladatban leírt Erlang-párral adjuk meg.

A Sudoku-megoldás a Sudoku-feladvánnyal azonos méretű, egész számokból álló mátrix, amelyet listák listájaként ábrázolunk.

Egy n=k*k sorból és n oszlopból álló Sudoku-megoldás akkor helyes egy adott Sudoku-feladványra nézve, ha

  - minden sorában, oszlopában, illetve cellájában 1 és n közötti, egymástól különböző értékű mezők vannak,
  - kielégíti az adott Sudoku-feladványt, azaz a benne szereplő ún. segítő információk (infók) által előírt összes feltételt.

A feladvány és a megoldás méretét, illetve az infók formáját nem kell ellenőrizni, feltehető, hogy a feladványnak és a megoldásnak a megadott k cellaméretnek megfelelő számú sora és oszlopa van, és az is, hogy az infókat a specifikációban leírtak szerint adtuk meg.


## Specifikáció

Írjon Erlang-függvényt khf3:helyese/2 néven egy Sudoku-megoldás helyességének a megállapítására.
```erlang
%% @spec khf3:helyese(SSpec::sspec(), SSol::ssol()) -> B::bool().
%%   B igaz, ha a teljesen kitöltött SSol Sudoku-megoldás
%%   (a) minden sorában, oszlopában, illetve cellájában egymástól különböző
%%       értékű, az SSpec által előírt tartományba eső egészek vannak,
%%   (b) kielégíti az SSpec specifikációt.
```
A programot tartalmazó modul attribútumai ezek legyenek:

```erlang
-module(khf3).
-author(email@unit.org.hu).
-vsn('year-mm-dd').
-export([helyese/2]).
```



## Gyakorló feladatok

A házi feladat megoldásának előkészítésére a következő kisebb gyakorló feladatok megoldását javasoljuk.
  - Annak eldöntése, hogy egy listában vannak-e ismétlődő elemek.
  - Annak eldöntése, hogy egy lista, ill. mátrix minden elemére teljesül-e egy adott predikátum.
  - Annak eldöntése, hogy egy lista, ill. mátrix legalább egy elemére teljesül-e egy adott predikátum.

## Példák

``` erlang
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
true
khf3:helyese({2,
              [[   [2],   [w],     [],   [w]],
               [    [],   [s],     [],   [o]],

               [    [],    [],    [1],    []],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
true
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],    []]]},

              [[2,4, 3,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,3, 4,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [1,4, 1,4],
               [3,2, 3,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 2,3]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],    [2],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],    [e],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],   [o]]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],   [s]],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[    [],    [],     [w],    []],
               [    [],    [],     [],    []],

               [    [],    [],     [],    []],
               [    [],    [],     [],    []]]},

              [[3,2, 4,1],
               [4,1, 2,3],

               [2,3, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[   [2],   [w],    [s],    []],
               [    [],   [s],     [],   [o]],

               [    [],    [],    [1],    []],
               [    [],    [],     [],    []]]},

              [[2,3, 4,1],
               [4,1, 2,3],

               [3,2, 1,4],
               [1,4, 3,2]]
            ).
false
khf3:helyese({2,
              [[ [e,2,s],         [w],           [e],          []],
               [      [],     [e,s,w],            [],         [o]],

               [      [],          [],           [1],          []],
               [      [],          [],            [],          []]]},

              [[2,1, 4,3],
               [3,4, 2,1],

               [4,3, 1,2],
               [1,2, 3,4]]
            ).
true
khf3:helyese({3,
              [[    [],   [2],   [3],    [4],   [1],    [],    [5],   [8],    []],
               [    [],   [7],   [8],     [],    [],    [],     [],   [2],   [1]],
               [    [],    [],   [5],    [7],   [2],    [],     [],   [3],   [9]],

               [    [],   [8],   [2],    [9],   [6],   [1],     [],   [4],    []],
               [    [],   [9],    [],    [5],    [],   [4],     [],   [1],    []],
               [    [],   [6],    [],    [2],   [7],   [3],    [9],   [5],    []],

               [   [7],   [4],    [],     [],   [9],   [5],    [3],    [],    []],
               [   [2],   [3],    [],     [],    [],    [],    [8],   [9],    []],
               [    [],   [5],   [9],     [],   [3],   [2],    [1],   [7],    []]]},

              [[6,2,3, 4,1,9, 5,8,7],
               [9,7,8, 3,5,6, 4,2,1],
               [4,1,5, 7,2,8, 6,3,9],

               [5,8,2, 9,6,1, 7,4,3],
               [3,9,7, 5,8,4, 2,1,6],
               [1,6,4, 2,7,3, 9,5,8],

               [7,4,1, 8,9,5, 3,6,2],
               [2,3,6, 1,4,7, 8,9,5],
               [8,5,9, 6,3,2, 1,7,4]]
            ).
true
khf3:helyese({3,
              [[   [2],    [],   [9],    [s],    [],   [s],    [s],   [e],    []],
               [    [],   [s],   [4],    [w],   [9],   [8],     [],   [o],   [e]],
               [    [], [e,w],   [1],    [w],    [],   [o],     [],   [w], [s,w]],

               [    [],    [],    [],    [w],    [],   [w],     [],   [o],    []],
               [    [],    [],    [],     [],   [w],    [],    [w],   [w],   [8]],
               [    [],    [],    [],     [],    [],    [],    [o],   [o],    []],

               [    [],    [],    [],    [3],    [],    [],    [1],   [e],   [s]],
               [    [],    [],    [],    [1],    [],    [],    [2],    [],   [e]],
               [    [],    [],    [],    [w],   [w],   [2],     [],   [w],    []]]},

              [[2,8,9, 6,1,7, 5,4,3],
               [3,7,4, 5,9,8, 6,1,2],
               [5,6,1, 2,4,3, 9,8,7],

               [7,3,8, 9,2,1, 4,5,6],
               [1,9,5, 7,6,4, 3,2,8],
               [4,2,6, 8,3,5, 7,9,1],

               [8,4,2, 3,7,9, 1,6,5],
               [9,5,7, 1,8,6, 2,3,4],
               [6,1,3, 4,5,2, 8,7,9]]
            ).
true
```
