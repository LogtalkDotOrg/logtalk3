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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example shows the use of meta-predicates in Logtalk. Meta-predicates are 
predicates whose head includes arguments that will be called as goals in the 
body of the predicate definition.

This example defines the following objects:

- `company`  
    usage example of the `map_reduce/5` meta-predicate

- `fibonacci`  
    example of calculating Fibonacci numbers using the `fold_left/4`
    meta-predicate

- `sort(_)`  
	this is a parametric object containing a method that implements the
	quicksort sorting algorithm; the parameter is interpreted as the type
	of the elements being sorted

- `tracer`  
	this object implements a meta-predicate that is used by `sort(_)` to 
	trace the sorting algorithm steps

- `metapreds`, `descendant`, and `test`  
	objects used for illustrating the use of closures as meta-arguments

- `predicates`  
	object defining some predicates for testing meta-predicates defined 
	in the Logtalk library
