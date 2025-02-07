---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# mi

There are two examples in this folder. The first one is an adoption of a 
multi-inheritance C++ example found on the D. M. Capper book "Introducing 
C++ for Scientists, Engineers and Mathematicians" published by 
Springer-Verlag. It uses dynamic predicates for storing state. The second 
example is a variant of the first one using parametric objects.

This example defines the following objects:

- `xyz`  
	this object space stores spatial coordinates using a dynamic 
	predicate

- `t`  
	this object stores a time stamp using a dynamic predicate

- `xyzt`  
	this object inherits from both the objects `xyz` and `t`


- `xyz(_,_,_)`  
	similar to object space but using parameters instead of dynamic 
	predicates

- `t(_)`  
	similar to object space but using a parameter instead of a dynamic 
	predicate

- `xyzt(_,_,_,_)`  
	this object inherits from both the objects `xyz(_,_,_)` and `t(_)`

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(mi(loader)).
```

Set a point in the space-time:

```logtalk
xyzt::rotate(1, 2, 3).
```

<!--
true.
-->

```logtalk
xyzt::translate(4).
```

<!--
true.
-->

Verify it:

```logtalk
xyzt::xyzt(X, Y, Z, T).
```

<!--
T = 4, X = 1, Y = 2, Z = 3.
-->

Enumerate `xyzt` public predicates:

```logtalk
%%table
xyzt::current_predicate(Functor/Arity), functor(Pred, Functor, Arity), xyzt::predicate_property(Pred, declared_in(Object)).
```

<!--
Pred = xyzt(_A,_B,_C,_D), Arity = 4, Object = space_time, Functor = xyzt ? ;
Pred = xyz(_A,_B,_C), Arity = 3, Object = xyz, Functor = xyz ? ;
Pred = rotate(_A,_B,_C), Arity = 3, Object = xyz, Functor = rotate ? ;
Pred = t(_A), Arity = 1, Object = t, Functor = t ? ;
Pred = translate(_A), Arity = 1, Object = t, Functor = translate ? ;
false.
-->

Query the origin distance from a point in the `space-time(_, _, _, _)`:

```logtalk
xyzt(2,3,4,7)::distance(D).
```

<!--
D = 5.385164807134504.
-->

Query the time:

```logtalk
xyzt(2,3,4,7)::time(T).
```

<!--
T = 7.
-->

Enumerate `xyzt(_, _, _, _)` public predicates:

```logtalk
%%table
xyzt(2,3,4,7)::current_predicate(Functor/Arity), functor(Pred, Functor, Arity), xyzt(2,3,4,7)::predicate_property(Pred, declared_in(Object)).
```

<!--
Pred = distance(_A), Arity = 1, Object = xyz(_B,_C,_D), Functor = distance ? ;
Pred = time(_A), Arity = 1, Object = t(_B), Functor = time ? ;
false.
-->
