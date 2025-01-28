---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
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
Triska's CLP(FD) library distributed with YAP with Logtalk. These examples 
are adapted with permission from the original author, Markus Triska.

The CLP(FD) library is loaded from the "loader.lgt" auxiliary loader file.
This library must always be loaded prior to compilation of the individual 
example files.

Start by loading the example:

```logtalk
logtalk_load(clp_yap(loader)).
```

Change the default term writing depth:

```logtalk
set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(50), priority(699)]).
```

<!--
true.
-->


```logtalk
puzzle::solve(Sum=Rs), label(Rs).
```

<!--
[9, 5, 6, 7]+[1, 0, 8, 5]=[1, 0, 6, 5, 2]
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

The following two queries implies that Ghostscript is available from the command-line:

```logtalk
soduku::(problem(1, Rows), show([ff], Rows)).
```

<!--
Rows = [[1, 5, 6, 8, 9, 4, 3, 2, 7], [9, 2, 8, 7, 3, 1, 4, 5, 6], [4, 7, 3, 2, 6, 5, 9, 1, 8], [3, 6, 2, 4, 1, 7, 8, 9, 5], [7, 8, 9, 3, 5, 2, 6, 4, 1], [5, 1, 4, 9, 8, 6, 2, 7, 3], [8, 3, 1, 5, 4, 9, 7, 6, 2], [6, 9, 7, 1, 2, 3, 5, 8, 4], [2, 4, 5, 6, 7, 8, 1, 3, 9]] .
-->

```logtalk
soduku::show([ff], Rows).
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

The following query implies that Ghostscript is available from the command-line:

```logtalk
queens::show(8, [ff], Qs).
```

<!--
Qs = [1, 5, 8, 6, 3, 7, 2, 4] ;
Qs = [1, 6, 8, 3, 7, 4, 2, 5] ;
Qs = [1, 7, 4, 6, 8, 2, 5, 3] ;
(etc)
-->

```logtalk
queens::show(N, [ff], Qs).
```

<!--
N = 1,
Qs = [1] ;
N = 4,
Qs = [2, 4, 1, 3] ;
N = 4,
Qs = [3, 1, 4, 2] ;
N = 5,
Qs = [1, 3, 5, 2, 4] ;
(etc)
-->

```logtalk
oneground::oneground(X, Y, Z), Y = 5.
```

<!--
Y = 5, Z = 1, X in inf..sup.
-->
