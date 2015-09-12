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

This example illustrates how to define "class methods" as found on class-
based object-oriented programming languages such as Java or Objective-C.
Logtalk classes are objects. Therefore, class methods are simply instance
methods defined in the class of the class, i.e. in its meta-class.

This example defines the following objects:

- `circle`  
	class representing common features of geometric circles
	such as radius and position

- `metacircle`  
	meta-class of class circle defining "class methods" for
	creating instances and calculating areas

- `c42`  
	static instance of class `circle`
