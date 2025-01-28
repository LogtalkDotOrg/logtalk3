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

# points

You can find the original description of this example (and a solution using 
SICStus Objects) at the URL:

	http://www.sics.se/ps/sicstus/sicstus_32.html#SEC254

Suppose you wish to represent points in a two-dimensional space. The
protocol you will have to define consists on the operation `move/2`, which
allows you to move one point to a new position, and on the operation
`print/0`, which prints the point position. From the base class `point`,
which contains the indicated operations, we wish to build three
variants. One class, `bounded_point`, in which one point can only move
in a restricted area in space. One class, `history_point`, characterized
from the particularity that each point recalls its previous positions.
Finally, a class `bounded_history_point` combining the functionality of
classes `bounded_point` and `history_point`.

At first sight, this looks like the kind of ideal problem to illustrate
the advantages of the multiple inheritance mechanisms. However, this type 
of solution holds several problems. If the methods `move/2` and `print/0`
are inherited by `bounded_history_point` of classes `history_point` and
`bounded_point` simultaneously, then one point will be moved and shown
twice. If the inheritance is carried out, for each method, only from one
of the superclasses (assuming that it is possible to do so, only by
breaking the apparent problem symmetry), then the interfaces of classes
`history_point` and `bounded_point` will have to contain separately the
necessary operations to verify the limits (in the case of `bounded_point`),
or to recall the previous positions (in the case of `history_point`). This
way, the class `bounded_history_point` could build its own versions of
methods `move/2` and `print/0`, adding to the inherited definitions of one
of the superclasses the calling of the operation missing in the other
superclass. This is the solution adapted in the SICStus Objects. However,
this solution also implies a few problems. Let's suppose that method
`move/2` is inherited from class `history_point`. Then, any changing
operated in the definition of the same method in class `bounded_point` is
ignored by `bounded_history_point`. The problem can be unnoticed, once
the symmetry suggested by the use of multiple inheritance does not reflect
on the present implementation.

The solution just suggested is, in short, a generalization of the problem
previously described. Instead of using multiple inheritance, let's use
composition mechanisms. In order to do so, let's separate the operations
on one point, while an object state, of the classes representing each one
of the point types. This can be achieved through the definition of two new
categories, `bounded_coordinate` and `point_history`, that will define the
operations associated both to the memorization of previous values, and to
the verification of feasible limits for a coordinate value. Each one of
the `point`, `bounded_point`, `history_point`, and `bounded_history_point`
classes will import this category, using his operations to define the
methods affecting the solutions that use multiple inheritance.

Start by loading the example and the required library files:

```logtalk
logtalk_load(points(loader)).
```

Let's start with a simple point:

```logtalk
point::new(Point, [position-(1, 3)]), Point::(print, move(7, 4), print).
```

<!--
p1 @ (1, 3)
p1 @ (7, 4)

Point = p1.
-->

Same problem but with bounds on coordinate values:

```logtalk
bounded_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]), Point::(print, move(7, 4), print).
```

<!--
bounds(x) : 0,13
bounds(y) : -7,7
bp2 @ (1, 3)
bounds(x) : 0,13
bounds(y) : -7,7
bp2 @ (7, 4)

Point = bp2.
-->

Same problem but storing the history of coordinate values:

```logtalk
history_point::new(Point, [position-(1, 3)]), Point::(print, move(7, 4), print).
```

<!--
location history: []
hp3 @ (1, 3)
location history: [(1,3)]
hp3 @ (7, 4)

Point = hp3.
-->

Same problem but with bounds on coordinate values and storing past values:

```logtalk
bounded_history_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]), Point::(print, move(7, 4), print).
```

<!--
bounds(x) : 0,13
bounds(y) : -7,7
location history: []
bhp4 @ (1, 3)
bounds(x) : 0,13
bounds(y) : -7,7
location history: [(1,3)]
bhp4 @ (7, 4)

Point = bhp4.
-->

Clean up instances:

```logtalk
point::delete_all, bounded_point::delete_all, history_point::delete_all, bounded_history_point::delete_all.
```

<!--
true.
-->
