---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

This folder contains a set of simple examples illustrating how to use Markus 
Triska's CLP(FD) library distributed with SWI-Prolog with Logtalk. These 
examples are adapted with permission from the original author, Markus Triska.

The CLP(FD) library is loaded from the `loader.lgt` auxiliary loader file.
This library must always be loaded prior to compilation of the individual 
example files.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(clp_swipl(loader)).
```

Change the default term writing depth:

```logtalk
set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(50), priority(699)]).
```

<!--
true.
-->

```logtalk
puzzle::solve(Sum=Rs).
```

<!--
Sum = [9, 5, 6, 7]+[1, 0, 8, 5], Rs = [1, 0, 6, 5, 2] .
-->

```logtalk
hexagon::mhex(Vs).
```

<!--
Vs = [3, 17, 18, 19, 7, 1, 11, 16, 2, 5, 6, 9, 12, 4, 8, 14, 10, 13, 15] ;
Vs = [3, 19, 16, 17, 7, 2, 12, 18, 1, 5, 4, 10, 11, 6, 8, 13, 9, 14, 15] ;
Vs = [9, 11, 18, 14, 6, 1, 17, 15, 8, 5, 7, 3, 13, 4, 2, 19, 10, 12, 16] ;
(etc)
-->

```logtalk
soduku::sudoku(Rows), append(Rows, Vs), label(Vs).
```

<!--
Rows = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [2, 1, 4, 3, 6, 5, 8, 9, 7], [3, 6, 5, 8, 9, 7, 2, 1, 4], [8, 9, 7, 2, 1, 4, 3, 6, 5], [5, 3, 1, 6, 4, 2, 9, 7, 8], [6, 4, 2, 9, 7, 8, 5, 3, 1], [9, 7, 8, 5, 3, 1, 6, 4, 2]],
Vs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9, 1, 2, 3, 7, 8, 9, 1, 2, 3, 4, 5, 6, 2, 1, 4, 3, 6, 5, 8, 9, 7, 3, 6, 5, 8, 9, 7, 2, 1, 4, 8, 9, 7, 2|...] ;
Rows = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [2, 1, 4, 3, 6, 5, 8, 9, 7], [3, 6, 5, 8, 9, 7, 2, 1, 4], [8, 9, 7, 2, 1, 4, 3, 6, 5], [5, 3, 1, 6, 4, 2, 9, 7, 8], [6, 4, 8, 9, 7, 1, 5, 3, 2], [9, 7, 2, 5, 3, 8, 6, 4, 1]],
Vs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9, 1, 2, 3, 7, 8, 9, 1, 2, 3, 4, 5, 6, 2, 1, 4, 3, 6, 5, 8, 9, 7, 3, 6, 5, 8, 9, 7, 2, 1, 4, 8, 9, 7, 2|...] ;
(etc)
-->

```logtalk
soduku::(problem(1, Rows), sudoku(Rows)), append(Rows, Vs), label(Vs).
```

<!--
Rows = [[1, 5, 6, 8, 9, 4, 3, 2, 7], [9, 2, 8, 7, 3, 1, 4, 5, 6], [4, 7, 3, 2, 6, 5, 9, 1, 8], [3, 6, 2, 4, 1, 7, 8, 9, 5], [7, 8, 9, 3, 5, 2, 6, 4, 1], [5, 1, 4, 9, 8, 6, 2, 7, 3], [8, 3, 1, 5, 4, 9, 7, 6, 2], [6, 9, 7, 1, 2, 3, 5, 8, 4], [2, 4, 5, 6, 7, 8, 1, 3, 9]],
Vs = [1, 5, 6, 8, 9, 4, 3, 2, 7, 9, 2, 8, 7, 3, 1, 4, 5, 6, 4, 7, 3, 2, 6, 5, 9, 1, 8, 3, 6, 2, 4, 1, 7, 8, 9, 5, 7, 8, 9, 3, 5, 2, 6, 4, 1, 5, 1, 4, 9|...] .
-->

The following two queries implies that Ghostscript is available from the command-line (skip if running as a notebook):

```logtalk
(current_object(jupyter) -> true; soduku::(problem(1, Rows), show([ff], Rows))).
```

<!--
Rows = [[1, 5, 6, 8, 9, 4, 3, 2, 7], [9, 2, 8, 7, 3, 1, 4, 5, 6], [4, 7, 3, 2, 6, 5, 9, 1, 8], [3, 6, 2, 4, 1, 7, 8, 9, 5], [7, 8, 9, 3, 5, 2, 6, 4, 1], [5, 1, 4, 9, 8, 6, 2, 7, 3], [8, 3, 1, 5, 4, 9, 7, 6, 2], [6, 9, 7, 1, 2, 3, 5, 8, 4], [2, 4, 5, 6, 7, 8, 1, 3, 9]] .
-->

```logtalk
(current_object(jupyter) -> true; soduku::show([ff], Rows)).
```

<!--
Rows = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [2, 3, 1, 6, 7, 4, 8, 9, 5], [8, 7, 5, 9, 1, 2, 3, 6, 4], [6, 9, 4, 5, 3, 8, 2, 1, 7], [3, 1, 7, 2, 6, 5, 9, 4, 8], [5, 4, 2, 8, 9, 7, 6, 3, 1], [9, 6, 8, 3, 4, 1, 5, 7, 2]] ;
Rows = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [2, 3, 1, 6, 7, 4, 8, 9, 5], [8, 7, 5, 9, 1, 2, 3, 6, 4], [6, 9, 4, 5, 3, 8, 2, 1, 7], [3, 1, 7, 2, 6, 5, 9, 4, 8], [5, 4, 8, 3, 9, 1, 6, 7, 2], [9, 6, 2, 8, 4, 7, 5, 3, 1]] ;
Rows = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [2, 3, 1, 6, 7, 4, 8, 9, 5], [8, 7, 5, 9, 1, 2, 3, 6, 4], [6, 9, 4, 5, 3, 8, 2, 1, 7], [3, 1, 7, 2, 6, 5, 9, 4, 8], [9, 6, 2, 8, 4, 7, 5, 3, 1], [5, 4, 8, 3, 9, 1, 6, 7, 2]] ;
(etc)
-->

```logtalk
queens::n_queens(8, Qs), labeling([ff], Qs).
```

<!--
Qs = [1, 5, 8, 6, 3, 7, 2, 4] ;
Qs = [1, 6, 8, 3, 7, 4, 2, 5] ;
Qs = [1, 7, 4, 6, 8, 2, 5, 3] ;
(etc)
-->

The following queries require that Ghostscript is available from the command-line (skip if running as a notebook):

```logtalk
(current_object(jupyter) -> true; queens::show(8, [ff], Qs)).
```

<!--
Qs = [1, 5, 8, 6, 3, 7, 2, 4] ;
Qs = [1, 6, 8, 3, 7, 4, 2, 5] ;
Qs = [1, 7, 4, 6, 8, 2, 5, 3] ;
(etc)
-->

```logtalk
(current_object(jupyter) -> true; queens::show(N, [ff], Qs)).
```

<!--
N = 1, Qs = [1] ;
N = 4, Qs = [2, 4, 1, 3] ;
N = 4, Qs = [3, 1, 4, 2] ;
N = 5, Qs = [1, 3, 5, 2, 4] ;
(etc)
-->

```logtalk
oneground::oneground(X, Y, Z), Y = 5.
```

<!--
Y = 5, Z = 1, X in inf..sup.
-->

```logtalk
knight::n_tour(N, Ts), meta::map(clpfd:label, Ts).
```

<!--
N = 0, Ts = [] ;
N = 6, Ts = [[9, 10, 7, 8, 16, 17], [15, 19, 5, 6, 3, 4], [2, 1, 26, 12, 21, 29], [32, 31, 25, 18, 34, 11], [14, 13, 35, 36, 33, 22], [27, 28, 20, 30, 24, 23]] .
-->

```logtalk
knight::n_tour(6, Ts), meta::map(label, Ts), knight::tour_enumeration(Ts, Es), meta::map(format("~t~w~3|~t~w~6|~t~w~9|~t~w~12|~t~w~15|~t~w~18|\n"), Es).
```

<!--
  1 30 25  6  3 32
 26  7  2 31 24  5
 29 36 27  4 33 16
  8 19 34 15 12 23
 35 28 21 10 17 14
 20  9 18 13 22 11
Ts = [[9, 10, 7, 8, 16, 17], [15, 19, 5, 6, 3, 4], [2, 1, 26, 12, 21, 29], [32, 31, 25, 18, 34, 11], [14, 13, 35, 36, 33, 22], [27, 28, 20, 30, 24, 23]],
Es = [[1, 30, 25, 6, 3, 32], [26, 7, 2, 31, 24, 5], [29, 36, 27, 4, 33, 16], [8, 19, 34, 15, 12, 23], [35, 28, 21, 10, 17, 14], [20, 9, 18, 13, 22, 11]] .
-->

```logtalk
time((knight::n_tour(8, Ts), list::append(Ts, Vs), clpfd:labeling([ff], Vs))).
```

<!--
% 18,438,711 inferences, 4.230 CPU in 4.482 seconds (94% CPU, 4359033 Lips)
Ts = [[11, 12, 18, 10, 15, 21, 13, 14], [3, 20, 17, 27, 23, 31, 30, 6], [2, 1, 29, 5, 4, 37, 8, 7], [19, 9, 33, 38, 35, 36, 16, 22], [43, 28, 50, 51, 52, 32, 24, 46], [26, 25, 53, 59, 55, 61, 62, 54], [34, 60, 57, 58, 63, 64, 40, 39], [42, 41, 49, 45, 44, 56, 48, 47]],
Vs = [11, 12, 18, 10, 15, 21, 13, 14, 3, 20, 17, 27, 23, 31, 30, 6, 2, 1, 29, 5, 4, 37, 8, 7, 19, 9, 33, 38, 35, 36, 16, 22, 43, 28, 50, 51, 52, 32, 24, 46, 26, 25, 53, 59, 55, 61, 62, 54, 34|...].
-->

```logtalk
knight::n_tour(8, Ts), list::append(Ts, Vs), clpfd:labeling([ff], Vs), knight::tour_enumeration(Ts, Es), meta::map(format("~t~w~3|~t~w~6|~t~w~9|~t~w~12|~t~w~15|~t~w~18|~t~w~21|~t~w~24|\n"), Es).
```

<!--
  1  4 63 28 31 26 19 22
 62 29  2  5 20 23 32 25
  3 64 39 30 27 56 21 18
 38 61  6 53 40 33 24 55
  7 52 41 34 57 54 17 46
 60 37  8 49 44 47 14 11
 51 42 35 58  9 12 45 16
 36 59 50 43 48 15 10 13
Ts = [[11, 12, 18, 10, 15, 21, 13, 14], [3, 20, 17, 27, 23, 31, 30, 6], [2, 1, 29, 5, 4, 37, 8, 7], [19, 9, 33, 38, 35, 36, 16, 22], [43, 28, 50, 51, 52, 32, 24, 46], [26, 25, 53, 59, 55, 61, 62, 54], [34, 60, 57, 58, 63, 64, 40, 39], [42, 41, 49, 45, 44, 56, 48, 47]],
Vs = [11, 12, 18, 10, 15, 21, 13, 14, 3, 20, 17, 27, 23, 31, 30, 6, 2, 1, 29, 5, 4, 37, 8, 7, 19, 9, 33, 38, 35, 36, 16, 22, 43, 28, 50, 51, 52, 32, 24, 46, 26, 25, 53, 59, 55, 61, 62, 54, 34|...],
Es = [[1, 4, 63, 28, 31, 26, 19, 22], [62, 29, 2, 5, 20, 23, 32, 25], [3, 64, 39, 30, 27, 56, 21, 18], [38, 61, 6, 53, 40, 33, 24, 55], [7, 52, 41, 34, 57, 54, 17, 46], [60, 37, 8, 49, 44, 47, 14, 11], [51, 42, 35, 58, 9, 12, 45, 16], [36, 59, 50, 43, 48, 15, 10, 13]] .
-->

The following queries require that Ghostscript is available from the command-line (skip if running as a notebook):

```logtalk
(current_object(jupyter) -> true; knight::n_tour(6, Ts), meta::map(clpfd:label, Ts), knight::show(Ts)).
```

```logtalk
(current_object(jupyter) -> true; knight::n_tour(8, Ts), meta::map(clpfd:label, Ts), knight::show(Ts)).
```
