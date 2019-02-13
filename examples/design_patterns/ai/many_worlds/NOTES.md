________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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
using Prolog modules. The key Logtalk features are inheritance (of
the reasoning code that we want to apply to many worlds) and messages
to self (which allow calling from the reasoning code the predicates
that describe a specific world).

A common scenario is porting an original Prolog module application
that assumes a single world defining in "user". Porting consist mainly
of applying the following steps:

1. Replacing Prolog module directives with Logtalk directives.
2. Changing calls and updates to predicates in "user" into messages to self.
3. Declaring those predicates as public or protected as needed.
