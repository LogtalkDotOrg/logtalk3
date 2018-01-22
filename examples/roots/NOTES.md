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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains rewritten versions of some of the objects provided 
with previous, 1.x versions, of the Logtalk system. They are intended to 
help the conversion of applications from Logtalk 1.x to 2.x and to
support most of the other examples provided with the current Logtalk
distribution.

Short description of each example entity:

- `class`  
	default metaclass for all classes
- `classp`  
	protocol of class `class`

- `abstract_class`  
	default metaclass for all abstract classes
- `abstract_classp`  
	protocol of class `abstract_class`

- `object`  
	root class for class-based hierarchies
- `objectp`  
	protocol of class `object`

- `initialization`  
	category defining methods for object initialization

- `proto`  
	root prototype for prototype-based hierarchies
- `protop`  
	protocol for prototype `proto`

- `nil`  
	object used to represent a void reference

Please note that the entities above are just example definitions. There is 
nothing fundamental about any of them; they can and should be replaced by 
definitions better fitted to the requirements of specific applications.
