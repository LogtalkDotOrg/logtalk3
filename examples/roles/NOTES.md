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

# roles

Logtalk provides _objects_, _protocols_, and _categories_ as first-class
entities. Relations between entities define _patterns of code reuse_ and
the roles played by the entities. For example, when an object instantiates
another object, the first object plays the role of an instance and the
second object plays the role of a class. An extends relation between two
objects implies that both objects play the role of prototypes, with one
of them extending the other, its parent prototype.

This simple example illustrates the different roles an object can play.
See the comments in the `roles.lgt` file for detailed explanations.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(roles(loader)).
```

Prototypes can declare and defined their own predicates:

```logtalk
prototype::foo(Foo).
```

<!--
Foo = 1.
-->

Derived prototypes inherit predicates from their parents:

```logtalk
descendant::foo(Foo).
```

<!--
Foo = 2.
-->

```logtalk
descendant::bar(X, Y).
```

<!--
X = 1, Y = 2.
-->

A class that is its own metaclass can access its own public predicates
using message-sending:

```logtalk
superclass::foo(Foo).
```

<!--
Foo = 1
-->

A class that doesn't have a metaclass cannot receive any message as
the predicate declaration lookup to answer to validate the message
would start in the metaclass:

```logtalk
\+ subclass::current_predicate(_).
```

<!--
true.
-->

An instance can receive messages for predicates declared in its
class(es) and in its class(es) superclass(es):

```logtalk
instance::current_predicate(P).
```

<!--
P = bar/2 ;
P = foo/1 ;
false.
-->

```logtalk
instance::foo(Foo).
```

<!--
Foo = 2.
-->

```logtalk
instance::bar(X, Y).
```

<!--
X = 1, Y = 2.
-->

```logtalk
empty_instance::foo(Foo).
```

<!--
Foo = 1.
-->
