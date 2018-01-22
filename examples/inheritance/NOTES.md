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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This folder contains examples of public, protected, and private inheritance,
for both prototype-based and class-based hierarchies.

This example defines a category named `predicates` which specifies three 
predicates, one public, one protected, and one private. This category is 
imported by the root objects: `parent` for the prototype hierarchy and 
`root` for the class hierarchy. Each root object have a set of three 
descendants, each one using one of the inheritance types.

The two object hierarchies are organized as follows:

	parent
		prototype1				% public inheritance
			descendant1
		prototype2				% protected inheritance
			descendant2
		prototype3				% private inheritance
			descendant3

	root
		subclass1				% public inheritance
			instance1
		subclass2				% protected inheritance
			instance2
		subclass3				% private inheritance
			instance3

A second category named `interface`, imported by all objects except the 
sub-class instances, allows us to query the objects about their interfaces.
