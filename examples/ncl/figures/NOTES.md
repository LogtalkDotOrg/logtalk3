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

This folder contains Logtalk versions of two examples of network modeling
for recognizing polyhedra represented as graphs as described in the following
paper:

```text
@inproceedings{Markov1989AFF,
	title={A Framework for Network Modeling in Prolog},
	author={Z. I. Markov},
	booktitle={IJCAI},
	year={1989}
}
```

A copy of the paper can be downloaded from:

https://www.ijcai.org/Proceedings/89-1/Papers/013.pdf

The Logtalk version uses parametric objects to represent the concept of
"net-variables" described in the paper as "global logical variables" and
the coroutining library to approximate the semantics of the original
examples. The object parameter variables provide object "global logical
variables". The implicit use of the `dif/2` constraint is mentioned in
the paper third page: "(The use of a special Prolog extension, ensuring
all different variables to be bound to different objects, is essential
in this example. Such an extension is also available in Prolog III.)".

Note that the use of the `dif` library limits the backend Prolog systems
that can be used to run this example to B-Prolog, ECLiPSe, XVM, SICStus
Prolog, SWI-Prolog, Trealla Prolog, XSB, and YAP.

Load the example:

```logtalk
logtalk_load(figures(loader)).
```

Recognize a parallelogram (the vertices rotation results in four solutions per class):

```logtalk
%%table
figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(1, 2,  0, 20),
		edge(2, 3, 45, 30),
		edge(3, 4,  0, 20),
		edge(4, 1, 45, 30),
		class(Name, X, Y, Z, P)
	).
```

<!--
Name = parallelogram, X = 1, Y = 2, Z = 3, P = 4 ;
Name = four_side_figure, X = 1, Y = 2, Z = 3, P = 4 ;
Name = parallelogram, X = 4, Y = 1, Z = 2, P = 3 ;
Name = four_side_figure, X = 4, Y = 1, Z = 2, P = 3 ;
Name = parallelogram, X = 3, Y = 4, Z = 1, P = 2 ;
Name = parallelogram, X = 2, Y = 3, Z = 4, P = 1 ;
Name = four_side_figure, X = 3, Y = 4, Z = 1, P = 2 ;
Name = four_side_figure, X = 2, Y = 3, Z = 4, P = 1 ;
false.
-->

```logtalk
%%table
figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(1, 2,  0, 20),
		edge(2, 3, 45, 30),
		edge(3, 4,  0, 20),
		edge(4, 1, 45, 30),
		class(Name)
	).
```

<!--
Name = parallelogram ;
Name = four_side_figure ;
Name = parallelogram ;
Name = four_side_figure ;
Name = parallelogram ;
Name = parallelogram ;
Name = four_side_figure ;
Name = four_side_figure ;
false.
-->

Recognize a rhombus (the vertices rotation results in four solutions per class):

```logtalk
%%table
figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(a, b, 45, 10),
		edge(b, c,  0, 10),
		edge(c, d, 45, 10),
		edge(d, a,  0, 10),
		class(Name, X, Y, Z, P)
	).
```

<!--
Name = rhombus, X = a, Y = b, Z = c, P = d ;
Name = parallelogram, X = a, Y = b, Z = c, P = d ;
Name = four_side_figure, X = a, Y = b, Z = c, P = d ;
Name = rhombus, X = d, Y = a, Z = b, P = c ;
Name = rhombus, X = c, Y = d, Z = a, P = b ;
Name = rhombus, X = b, Y = c, Z = d, P = a ;
Name = parallelogram, X = d, Y = a, Z = b, P = c ;
Name = four_side_figure, X = d, Y = a, Z = b, P = c ;
Name = parallelogram, X = c, Y = d, Z = a, P = b ;
Name = parallelogram, X = b, Y = c, Z = d, P = a ;
Name = four_side_figure, X = c, Y = d, Z = a, P = b ;
Name = four_side_figure, X = b, Y = c, Z = d, P = a ;
false.
-->

```logtalk
%%table
figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(a, b, 45, 10),
		edge(b, c,  0, 10),
		edge(c, d, 45, 10),
		edge(d, a,  0, 10),
		class(Name)
	).
```

<!--
Name = rhombus ;
Name = parallelogram ;
Name = four_side_figure ;
Name = rhombus ;
Name = rhombus ;
Name = rhombus ;
Name = parallelogram ;
Name = four_side_figure ;
Name = parallelogram ;
Name = parallelogram ;
Name = four_side_figure ;
Name = four_side_figure ;
false.
-->

Take perpendicularity into account when checking the class of a figure:

```logtalk
%%table
figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(1, 2,  0, 20),
		edge(2, 3, 90, 20),
		edge(3, 4,  0, 20),
		edge(4, 1, 90, 20),
		perpendicular,
		class(Name)
	).
```

<!--
Name = square ;
Name = rectangular ;
Name = four_side_figure ;
Name = square ;
Name = square ;
Name = square ;
Name = rectangular ;
Name = four_side_figure ;
Name = rectangular ;
Name = rectangular ;
Name = four_side_figure ;
Name = four_side_figure ;
false.
-->

```logtalk
%%table
figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
		edge(a, b, 45, 10),
		edge(b, c,  0, 10),
		edge(c, d, 45, 10),
		edge(d, a,  0, 10),
		perpendicular,
		class(Name)
	).
```

<!--
Name = rhombus ;
Name = parallelogram ;
Name = four_side_figure ;
Name = rhombus ;
Name = rhombus ;
Name = rhombus ;
Name = parallelogram ;
Name = four_side_figure ;
Name = parallelogram ;
Name = parallelogram ;
Name = four_side_figure ;
Name = four_side_figure ;
false.
-->
