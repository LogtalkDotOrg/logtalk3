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

# proxies

A parametric object may be used to represent objects whose "state" is static 
and set when the object is defined. However, there can be only one parametric 
object with a given functor and arity. For example, if we define the following
parametric object:

```logtalk
%%highlight
:- object(circle(_Radius, _Color)).
	...
:- end_object.
```

then the following terms may be interpreted as references to the object above:

```logtalk
%%highlight
circle(1, blue)
circle(2, yellow)
```

In the context of parametric objects, the above terms are known as "parametric
object proxies". Proxies represent different instantiations of a parametric 
object parameters. Proxy terms may be stored on the database as Prolog facts 
or as Prolog rules (parameter instantiation can be deduced instead of being 
fixed). This results in a very compact representation, which can be an 
advantage when dealing with a large number of objects with immutable state. 
In addition, all the predicates managing these compact representation are 
encapsulated in a parametric object. This can be, however, a fragile solution 
as changes on the parametric object ancestors may imply changes to the number 
and meaning of the parametric object parameters which, in turn, may imply 
changes to all the Prolog facts used to represent the individual objects.

Note that parametric objects can co-exist with "normal" objects. For example, 
when using a class-based design, we may use "normal" instances together with
a parametric instance of the same class.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(proxies(loader)).
```

Print the area and the perimeter for all circle proxies:

```logtalk
forall(circle(Id, R, C), circle(Id, R, C)::print).
```

<!--
id: #1, area: 4.75291, perimeter: 7.72831, color: blue
id: #2, area: 43.2412, perimeter: 23.3106, color: yellow
id: #3, area: 0.477836, perimeter: 2.45044, color: green
id: #4, area: 103.508, perimeter: 36.0655, color: black
id: #5, area: 217.468, perimeter: 52.2761, color: cyan
true.
-->

Logtalk provides a convenient notation for accessing proxies
represented as Prolog facts when sending a message:

```logtalk
{circle(_, _, _)}::print, fail; true.
```

<!--
id: #1, area: 4.75291, perimeter: 7.72831, color: blue
id: #2, area: 43.2412, perimeter: 23.3106, color: yellow
id: #3, area: 0.477836, perimeter: 2.45044, color: green
id: #4, area: 103.508, perimeter: 36.0655, color: black
id: #5, area: 217.468, perimeter: 52.2761, color: cyan
true.
-->

Print the area and the perimeter for the circle #2:

```logtalk
{circle('#2', R, C)}::print.
```

<!--
id: #2, area: 43.2412, perimeter: 23.3106, color: yellow
R = 3.71, C = yellow.
-->

Construct a list with the areas of all circles:

```logtalk
findall(Area, {circle(_, _, _)}::area(Area), Areas).
```

<!--
Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468].
-->

Some example queries for backend Prolog compilers implementing the 
`time/1` timing predicate (e.g., SWI-Prolog or YAP; the adapter files
for these two systems ensure that a ::/2 goal in the argument of the
`time/1` predicate is compiled prior to calling it so that we benchmark
the code instead of the compiler).

Recompile the example in optimal mode:

```logtalk
logtalk_make(optimal).
```

<!--
true.
-->

Confirm that the `time/1` predicate is available:

```logtalk
time(true).
```

<!--
% 2 inferences, 0.000 CPU in 0.000 seconds (67% CPU, 250000 Lips)
true.
-->

Benchmark some queries:

```logtalk
time(circle(one, 7, red)::id(Id)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (56% CPU, 76923 Lips)
Id = one.
-->

```logtalk
time(circle(one, 7, red)::radius(Radius)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (61% CPU, 83333 Lips)
Radius = 7.
-->

```logtalk
time(circle(one, 7, red)::color(Color)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (56% CPU, 71429 Lips)
Color = red.
-->

```logtalk
Id0 = one, time(circle(Id0, 7, red)::id(Id)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (56% CPU, 76923 Lips)
Id0 = Id, Id = one.
-->

```logtalk
Radius0 = 7, time(circle(one, Radius0, red)::radius(Radius)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (61% CPU, 83333 Lips)
Radius0 = Radius, Radius = 7.
-->

```logtalk
Color0 = red, time(circle(one, 7, Color0)::color(Color)).
```

<!--
% 1 inferences, 0.000 CPU in 0.000 seconds (56% CPU, 71429 Lips)
Color0 = Color, Color = red.
-->

```logtalk
time({circle('#2', Radius, Color)}::id(Id)).
```

<!--
% 2 inferences, 0.000 CPU in 0.000 seconds (71% CPU, 90909 Lips)
Radius = 3.71, Color = yellow, Id = '#2'.
-->