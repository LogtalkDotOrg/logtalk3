________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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

This folder contains an example of instance defined methods. When using 
classes and instances, methods must be declared in a class but the method 
definitions may be stored in the instances, either overriding or specializing 
the class definitions.

This example defines the following objects:

- `root`  
	root class defining a method named `method/0`

- `instance1`  
	simple instance of root inheriting `method/0`

- `instance2`  
	instance of root overriding the inherited method `method/0`

- `instance3`  
	instance of root specializing the inherited method `method/0`
