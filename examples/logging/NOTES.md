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

This folder contains an example of using a category to define a simple 
logging support for objects. This example illustrates how to define in 
a category a set of predicates that handle a dynamic predicate in the 
context of "this". Although the database built-in methods, such as 
`assertz/1` or `retractall/1`, are straightforward to use, calling the 
dynamic predicate must be performed using the message sending `::/2` 
control construct (note that a direct call to the dynamic predicate 
would result in a compilation error as it would be interpreted as a 
call to a local category dynamic predicate: categories cannot contain
clauses for dynamic predicates).
