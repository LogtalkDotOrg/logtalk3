________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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

There are two examples in this folder. The first one is an adoption of a 
multi-inheritance C++ example found on the D. M. Capper book "Introducing 
C++ for Scientists, Engineers and Mathematicians" published by 
Springer-Verlag. It uses dynamic predicates for storing state. The second 
example is a variant of the first one using parametric objects.

This example defines the following objects:

- `xyz`  
	this object space stores spatial coordinates using a dynamic 
	predicate

- `t`  
	this object stores a time stamp using a dynamic predicate

- `xyzt`  
	this object inherits from both the objects `xyz` and `t`


- `xyz(_,_,_)`  
	similar to object space but using parameters instead of dynamic 
	predicates

- `t(_)`  
	similar to object space but using a parameter instead of a dynamic 
	predicate

- `xyzt(_,_,_,_)`  
	this object inherits from both the objects `xyz(_,_,_)` and `t(_)`
