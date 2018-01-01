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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an example that shows how to implement instance
variables as defined in class-based object-oriented programming languages.

This example defines a root class, `root`, and three instances, `instance1`, 
`instance2`, and `instance3`. The root class defines an instance variable 
(using a dynamic predicate) and the corresponding setter and getter methods.
The root class is used to store a default value for the instance variable.
