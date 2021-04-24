________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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


To load this example and for sample queries, please see the `SCRIPT.txt`
file. Run this example with no other examples loaded at the same time.

This folder contains an example that shows how to implement a reflective
class-based system. There are three main classes:

- `object`  
	root of the inheritance graph
- `class`  
	default metaclass for all instantiable classes
- `abstract_class`  
	default metaclass for all abstract classes

Each class inherit all the methods form the other two classes and from 
itself (without any inheritance loops of course ;-).

You can find more sophisticated versions of these classes in the `roots`
example. If you are not familiar with the concept of metaclass used in
this example, see the `metaclasses` example first.
