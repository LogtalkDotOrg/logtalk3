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

# operators

This folder contains examples of using operators inside objects and
categories:

- `double.lgt`  
	Simple object containing clauses using an infix operator.

- `triple.lgt`  
	Simple object reading from a file, `triple.txt`, and asserting into 
	self, clauses that use an infix operator.

- `reverse.lgt`  
	Simple object reading from a file, `next.txt`, and writing to 
	another file, `previous.txt`, clauses that use infix operators.

- `local.lgt`  
	Simple example of defining an operator local to a source file.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(operators(loader)).
```

Operators declared inside an object are not visible outside:

```logtalk
double::(I double J).
```

<!--
Syntax error: Operator expected
-->

You must use instead functor notation:

```logtalk
%%table
double::double(I, J).
```

<!--
I = 1, J = 2 ;
I = 2, J = 4 ;
I = 3, J = 6 ;
false.
-->

Operators also affect inputting of terms, enabling this example to work:

```logtalk
triple::read_from_file.
```

<!--
true.
-->

```logtalk
%%table
triple::triple(I, J).
```

<!--
I = 1, J = 3 ;
I = 2, J = 6 ;
I = 3, J = 9 ;
false.
-->

Local operators are used when reading terms (check the file `previous.txt`
generated from the file `next.txt` by the object `reverse` by opening the
files on a text editor):

```logtalk
reverse::reverse_file.
```

<!--
true.
-->

The `edge` operator on the `local.lgt` source file is not globally visible:

```logtalk
graph1::(N1 edge N2).
```

<!--
uncaught exception: error(syntax_error('user_input:10 (char:13) ) or operator expected'),read_term/3)
-->

You must use instead functor notation:

```logtalk
%%table
graph1::edge(N1, N2).
```

<!--
N1 = a, N2 = b ;
N1 = a, N2 = c ;
N1 = b, N2 = d ;
N1 = c, N2 = d ;
false.
-->

```logtalk
%%table
graph1::path(a, d, Path).
```

<!--
Path = [a, b, d] ;
Path = [a, c, d] ;
false.
-->

Confirm that the `edge` operator have not became global:

```logtalk
current_op(P, T, edge).
```

<!--
false.
-->
