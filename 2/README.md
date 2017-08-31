# Deklaratív programozás Erlang házi

A feladat azoknak a megengedett mezőértékeknek a meghatározása, amelyek a félévi nagy házi feladatban specifikált Sudoku-tábla egy adott mezőjében előfordulhatnak.

Legyen a Sudoku-táblában k a cellák oldalhossza. Egy adott mező értékeként megengedett egy szám, ha 1..k*k közötti egész, teljesíti az adott mezőben szereplő szám- és paritási infók által előírt megszorításokat, továbbá különbözik az adott mezőt tartalmazó sor, oszlop és cella **többi** mezőjében szereplő **szám**infók által előírt értékektől.

A feladat bemenete tehát egy Sudoku-tábla és azon belül egy mező. A Sudoku-táblát a nagy házi feladatban specifikált Erlang-párral adjuk meg. Az adott mezőt egy sor-oszlop koordinátapárral határozzuk meg, ahol a Sudoku-tábla bal felső mezőjének koordinátája: (1,1). A feladat kimenete a megengedett mezőértékeket tartalmazó, esetleg üres lista. A lista elemeinek sorrendje tetszőleges, de ismétlődő elem nem lehet benne.

Egy Sudoku feladvány megoldásában egy adott mezőben nyilván nem szerepelhet egy – a fenti definíció szerint – nem megengedett számérték, hiszen egy ilyen érték nem tesz eleget a Sudoku megoldásokra előírt valamelyik megszorításnak. Bonyolultabb következtetésekkel más értékekről is kimutatható, hogy nem szerepelhetnek a megoldás egy adott mezőjében. Ha például egy k=2 cellaméretű Sudoku feladvány egy sorának 1. és 2. mezőjében rendre a 2 és e infók szerepelnek, akkor ennek a sornak a 3. és 4. eleméről könnyen belátható, hogy ezek csak páratlanok lehetnek, ugyanis a sorban már van két páros elem (az 1. és a 2.). Hasonló következtetésre juthatunk abban az esetben, ha a sor 2. és 3. mezőjében rendre a w és e infók szerepelnek: ekkor a sor 4. eleméről látható be, hogy az biztosan páratlan.

Fontos, hogy a kis házi feladat megoldásában a szomszédsági megszorításokat megadó s és w jeleket figyelmen kívül kell hagyni, és összetett következtetéseket **nem szabad** végezni: pontosan a fent leírt definíció szerint kell a megengedett értékek listáját előállítani. Ilyen eseteket mutatunk be alább, a Erlang példafutások között: az első két példában a megengedett értékek listájában szerepelnie kell a 4 számnak, annak ellenére, hogy belátható, hogy ez az érték az adott helyen nem szerepelhet az adott feladvány megoldásában.

A házi feladat megoldása során a paraméterekre vonatkozó formai előírások meglétét nem kell ellenőriznie, azaz feltételezheti, hogy

  - a feladványt leíró adatstruktúra megfelel a nagy házi feladat kiírásában megadott formai követelményeknek;
  - az adott mezőt meghatározó sor-oszlop koordinátapár mindkét komponense az 1..k*k tartományba esik, ahol k a feladvány cellamérete.

## Specifikáció

Írjon Erlang-függvényt `ertekek/2` néven, amely egy Sudoku-tábla adott mezőjére előállítja a megengedett mezőértékek listáját. A szóbanforgó mezőt egy {R,C} párral adjuk meg, ahol R a mező sorának, míg C a mező oszlopának a sorszáma, 1-től számozva.
```erlang
%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @spec khf2:ertekek(SSpec::sspec(), R_C::coords()) -> Vals::[integer()]
%%   Vals az SSpec specifikációval megadott Sudoku-tábla R_C
%%   koordinátájú mezőjében megengedett értékek listája.
%%   Egy érték pontosan akkor szerepel
%   a Vals listában, ha:
%    (a) 1..k*k közötti egész, ahol k az SSpec tábla cellamérete,
%    (b) teljesíti az adott mezőre vonatkozó szám- és paritási infók
%        által előírt megszorításokat, továbbá
%    (c) különbözik az adott mezőt tartalmazó sor, oszlop és cella többi
%        mezőjében szereplő száminfóktól.
```


A programot tartalmazó modul attribútumai ezek legyenek:
```erlang
-module(khf2).
-author(email@unit.org.hu).
-vsn('year-mm-dd').
-export([ertekek/2]).
```
## Gyakorló feladatok

A házi feladat megoldásának előkészítésére a következő kisebb gyakorló feladatok megoldását javasoljuk.
  - Az L1 és az L2 lista L3 = L1 - L2 különbségének előállítása. (Itt L3 az L1-nek azokat az elemeit tartalmazza, amelyek L2-ben nem fordulnak elő.) Az ismétlődő elemek kezelésére többféle módszert is kipróbálhat.
  - Egy sorok listájaként tárolt mátrix adott mezőjét befoglaló k-méretű cella elemeit tartalmazó lista előállítása.
Halmazok metszetének, uniójának, ill. különbségének előállítása (egy halmaz ábrázolására használhat rendezett vagy nem rendezett listát).
  - Lista rendezése különféle módszerekkel.

## Példák

```erlang
1> khf2:ertekek(
              {2, [[   [2],          [],         [e],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],         [1],          []],
                   [    [],         [w],          [],          []]]}, {1,4}).
[1, 3, 4]
2> khf2:ertekek(
              {2, [[   [2],         [w],          [],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],         [1],          []],
                   [    [],         [w],          [],          []]]}, {1,2}).
[1, 3, 4]
3> khf2:ertekek(
              {2, [[   [2],          [],         [e],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],         [1],          []],
                   [    [],         [w],          [],          []]]}, {1,1}).
[2]
4> khf2:ertekek(
              {2, [[   [2],          [],         [e],          []],
                   [    [],         [e],          [],         [o]],
                   [    [],          [],         [1],          []],
                   [    [],         [w],          [],          []]]}, {4,2}).
[1, 2, 3, 4]
5> khf2:ertekek(
              {2, [[    [],         [w],       [o,e],          []],
                   [    [],          [],          [],         [o]],
                   [    [],          [],          [],          []],
                   [    [],         [w],          [],          []]]}, {1,3}).
[]
6> khf2:ertekek(
              {2, [[   [o],         [w],         [e],          []],
                   [    [],     [e,s,1],          [],         [o]],
                   [    [],          [],         [4],          []],
                   [   [3],         [w],          [],          []]]}, {1,1}).
[]
7> khf2:ertekek(
              {3, [[   [2],   [],   [9],   [s],    [],   [s],   [s],  [e],    []],
                   [    [],  [s],   [4],   [w],   [9],   [8],    [],  [o],   [e]],
                   [    [],[e,w],   [1],   [w],    [],   [o],    [],  [w], [s,w]],
                   [    [],   [],    [],   [w],    [],   [w],    [],  [o],    []],
                   [    [],   [],    [],    [],   [w],    [],   [w],  [w],   [8]],
                   [    [],   [],    [],    [],    [],    [],   [o],  [o],    []],
                   [    [],   [],    [],   [3],    [],    [],   [1],  [e],   [s]],
                   [    [],   [],    [],   [1],    [],    [],   [2],   [],   [e]],
                   [    [],   [],    [],   [w],   [w],   [2],    [],  [w],    []]]}, {3,2}).
[6,8]
8> khf2:ertekek(
             {2, [[     [2],         [w],     [e],      []],
                  [      [],     [e,s,2],      [],     [o]],
                  [      [],          [],     [4],      []],
                  [      [],         [w],      [],     []]]}, {1,1}).
[]
```
