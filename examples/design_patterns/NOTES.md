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


This directory contains sample implementations of design patterns,
originating from the OOP and AI communities. The OOP patterns are
split between behavioral, creational, and structural patterns. Most
of them are described in the book:

Design Patterns - Elements of Reusable Object-Oriented Software
Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides
Addison-Wesley Professional Computing Series
ISBN 0-201-63361-2

If you don't have access to a copy of the book, see freely available
Internet resources such as:

https://en.wikipedia.org/wiki/Design_Patterns

Note that the design patterns described in the book (aka the "Gang of Four"
or "GoF" book) assume, in general, a class-based object-oriented language.
Logtalk features, notably the support for prototypes, categories, parametric
entities, and event-driven programming, allow alternative, and sometimes
preferable, solutions for some of the design patterns. When applicable, the
design patterns are generalized to apply to both classes and prototypes.

It is also important to note that basic Logtalk (and Prolog) features,
for example, meta-predicates and multifile predicates, can complement
or provide alternative solutions for the problems targeted by some of
the patterns. Thus, these sample implementations of design patterns
should be taken as examples to be mined for ideas rather than canonical
advice on how to solve the described problems.

Given that Logtalk is a *declarative* language, the creational patterns
are usually reinterpreted as structural patterns as objects, including
those playing the role of instances, are often defined in source files
instead of being forcefully dynamically created.

The descriptions of the design patterns are citations from the book
when applicable.
