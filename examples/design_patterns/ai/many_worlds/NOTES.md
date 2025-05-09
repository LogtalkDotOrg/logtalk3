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

Design pattern:
	Many worlds

Description:
	Allow reasoning about different worlds, where a world can be e.g.
	a dataset, a knowledge base, a set of examples.

Related examples:
	The Metagol port, an inductive logic programming (ILP) system is
	a good example. In this case, each world is composed by examples
	and background language and reasoning results on learning a set
	of rules. See the `ports/metagol` directory for details. Another,
	simpler, example of this design pattern is `examples/family`.

This is a pattern that is trivial in Logtalk but cumbersome at best
using Prolog modules. There are two sensible implementations of this
design pattern: using inheritance or using parametric objects. Both
solutions are illustrated. Load the `loader.lgt` file to load the
sample implementations and look into the `NOTES.md` file for sample
queries.

In the parametric solution, we use a parametric object for the reasoning
code and pass the dataset or knowledge base to reason about as a parameter.
A simple implementation of this solution is provided in the `parametric.lgt`
file.

In the inheritance solution, the key Logtalk feature (of the reasoning
code that we want to apply to many worlds) are messages to self (which
allow calling from the reasoning code the predicates that describe a
specific world). A simple implementation of this solution is provided
in the `inheritance.lgt` file.

A common scenario is porting an original Prolog module application
that assumes a single world defining in "user". Porting consist mainly
of applying the following steps:

1. Replacing Prolog module directives with Logtalk directives.
2. Changing calls and updates to predicates in "user" into messages to self.
3. Declaring those predicates as public or protected as needed.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('ai/many_worlds/loader')).
```

Try the inheritance approach to the design pattern:

```logtalk
world1::lowest(Lowest).
```

<!--
Lowest = 17.8.
-->

```logtalk
world2::average(Average).
```

<!--
Average = 11.566666666666668.
-->
