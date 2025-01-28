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

Design pattern:
	Builder

Description:
	"Separate the construction of a complex object from its
	representation so that the same construction process can
	create different representations."

This pattern can be used with both classes and prototypes.

Two sample implementations are provided, both using prototypes. The first
one uses parametric objects for a more declarative solution. The second
one uses objects with dynamic state to represent builders and products.

Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('creational/builder/loader')).
```

Parametric object version:

```logtalk
builder(_)::(set_wheels(3), set_seats(2), set_color(red), get_result(Car)).
```

<!--
Car = car(3, 2, red).
-->

```logtalk
car(3, 2, red)::seats(Seats).
```

<!--
Seats = 2.
-->

Objects with dynamic state version:

```logtalk
builder::new(Builder).
```

<!--
Builder = o2.
-->

```logtalk
o2::(set_wheels(3), set_seats(2), set_color(red), get_result(Car)).
```

<!--
Car = o1.
-->

```logtalk
o1::seats(Seats).
```

<!--
Seats = 2.
-->
