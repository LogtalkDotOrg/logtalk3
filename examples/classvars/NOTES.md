________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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

This folder contains an example that shows how to implement class variables 
as defined in Smalltalk. The name shared instance variables is however much
more accurate. In systems like Logtalk, which enables the use of explicit 
metaclasses, true class variables are just the class (as an object) own 
instance variables!

This example defines a root class, `root` and three instances, `instance1`, 
`instance2`, and `instance3`. The root class defines a shared instance variable 
(using a dynamic predicate) and the setter and getter methods which implement 
the variable sharing behavior.
