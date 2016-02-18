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

This folder contains examples of using some of the built-in database 
handling methods with object and categories. Two object hierarchies are
provided, one prototype-based, and the other class-based, in order to
illustrate the differences between asserting predicates in a class and
in a prototype:

The following objects are defined:

- `root`  
	root of the prototype hierarchy; declares and defines a public,
	dynamic predicate
- `descendant`  
	simple prototype extending the root prototype

- `class`  
	root of the class hierarchy; declares and defines a public predicate
- `metaclass`  
	class metaclass
- `instance`  
	simple instance of class class

- `prototype`  
	simple prototype used to illustrate how the scope of asserted 
	predicates depends on the target object (this, self, or an explicit 
	object)

In addition, the file `categories.lgt` illustrates how to define category
predicates that handle dynamic predicates in the context of "this" and in
the context of "self".
