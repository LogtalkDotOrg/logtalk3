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
	Visitor

Description:
	"Represent an operation to be performed on the elements of an
	object structure. Visitor lets you define a new operation without
	changing the classes of the elements on which it operates."

This pattern can be used with both classes and prototypes.

This design pattern can be trivially implemented by defining a suitable
meta-predicate that walks the structure and applies a user defined
closure to each element. When the object defining the structure doesn't
provide such a meta-predicate, a complementing category can define and
add it to the object. Both cases are illustrated in the sample code.
Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('behavioral/visitor/loader')).
```

Print all car components by calling the visitor/1 meta-predicate
with a closure (use a lambda expression so that we can print a
new-line after each component):

```logtalk
sedan::visitor([Component]>>(write(Component),nl)).
```

<!--
engine(diesel)
wheel(front_left)
wheel(front_right)
wheel(rear_right)
wheel(rear_left)
wheel(left_door)
wheel(right_door)
body(station_wagon)

true.
-->

Use the visitor meta-predicate that is added using hot-patching:

```logtalk
sedan::alt_visitor([Component]>>(write(Component),nl)).
```

<!--
engine(diesel)
wheel(front_left)
wheel(front_right)
wheel(rear_right)
wheel(rear_left)
wheel(left_door)
wheel(right_door)
body(station_wagon)

true.
-->

Use the standard `setof/3` meta-predicate to construct a list of
all component types:

```logtalk
setof(
		Type,
		Component^Arity^(sedan::component(Component), functor(Component,Type,Arity)),
		Types
     ).
```

<!--
Types = [body, engine, wheel].
-->
