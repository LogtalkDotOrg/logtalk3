________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


This directory contains sample implementations of design patterns, most of
them described in the book:

Design Patterns - Elements of Reusable Object-Oriented Software
Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides
Addison-Wesley Professional Computing Series
ISBN 0-201-63361-2

Note that the design patterns described in the book (aka the "Gang of Four"
or "GoF" book) assume, in general, a class-based object-oriented language.
Logtalk features, including the support for prototypes, categories, and
event-driven programming, allow alternative, and sometimes preferable,
solutions for some of the design patterns. When applicable, the design
patterns are generalized to apply to both classes and prototypes.

Given that Logtalk is a *declarative* language, the creational patterns
are usually reinterpreted as structural patterns as objects, including
those playing the role of instances, are often defined in source files
instead of being forcefully dynamically created.

The descriptions of the design patterns are citations from the book.
