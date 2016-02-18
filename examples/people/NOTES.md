________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.
 
This example illustrates how to define object constructors for a simple 
hierarchy of objects representing persons, students, and teachers. For
simplicity, prototypes are used instead of classes. Logtalk provides a 
low-level, built-in predicate, `create_object/4`, for dynamically creating 
new objects. This predicate can be used to define object constructors, 
similar to those used in other OOP languages.

This example also illustrates how to efficiently represent objects with 
immutable state using parametric objects and object proxies (Prolog facts).
