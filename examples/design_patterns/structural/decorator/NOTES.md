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

Design pattern:
	Decorator

Description:
	"Attach additional responsibilities to an object dynamically.
	Decorators provide a flexible alternative to subclassing for
	extending functionality."

This pattern can be used with both classes and prototypes.

The sample code uses a parametric object to represent the decorator object
with a parameter being used to pass the decorated object. This is a clean,
declarative solution given that object parameters are logical variables.
It also supports defining decorations dynamically by simply constructing
identifier terms for the parametric object. In those cases where we need
to persist across backtracking the association between a decorator object
and its decorated object, we can use in alternative a dynamic predicate
in the decorator object.

As a decorator object should accept all messages that the decorated object
accepts, we use the `forward/1` handler for unknown messages to forward to
the decorated objects all messages that are not defined in the decorator
itself. By using this handler, it is simple to define pipelines of
decorators where a decorator acts as the decorated object for another
decorator.

Another possible implementation of this pattern is to use a *complementing
category* (i.e., hot patching) to decorate an object. This can be a good
alternative solution when we need only to decorated a few specific objects
(although a complementing category can complement multiple objects). The
advantage of using a decorator object is that it can be used to decorate
any object at compilation time or at runtime while with a complementing
category we need to know in advance which objects we will be decorating.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/decorator/loader')).
```

Send the `string/0` message to the decorator object:

```logtalk
colored_shape(circle, red)::string.
```

<!--
A circle of radius 10.0
which is colored red

true.
-->

The `diameter/1` predicate in not defined for `shape` or for
`colored_shape`; thus our decorator forwards the message
to the decorated circle:

```logtalk
colored_shape(circle, red)::diameter(Diameter).
```

<!--
Diameter = 20.0.
-->

Same queries but with a dynamically created object:

```logtalk
create_object(Circle, [extends(circle)], [], [radius(7.0)]).
```

<!--
Circle = o1.
-->

```logtalk
colored_shape(o1, blue)::string.
```

<!--
A circle of radius 7.0
which is colored blue

true.
-->

```logtalk
colored_shape(o1, blue)::diameter(Diameter).
```

<!--
Diameter = 14.0.
-->

We can define a pipeline of decorators; e.g. a colored, named shape:

```logtalk
create_object(NamedShape, [extends(named_shape)], [], [shape(colored_shape(o1,blue)), name(thingy)]).
```

<!--
NamedShape = o2.
-->

```logtalk
o2::string.
```

<!--
A circle of radius 7.0
which is colored blue
which is named thingy

true.
-->

```logtalk
o2::diameter(Diameter).
```

<!--
Diameter = 14.0.
-->

Same queries using the decorator defined in the source file:

```logtalk
my_named_shape::string.
```

<!--
A circle of radius 10.0
which is colored green
which is named Mr. Round

true.
-->

```logtalk
my_named_shape::diameter(Diameter).
```

<!--
Diameter = 20.0.
-->
