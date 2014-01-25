________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example contains simple meta-interpreters for pure Prolog encapsulated 
in Logtalk categories:

- `solver`  
	simple meta-interpreter for pure Prolog

- `proof_tree`  
	simple meta-interpreter for pure Prolog returning the proof 
	tree for successful queries

- `tracer`  
	simple meta-interpreter for pure Prolog that traces proof 
	construction

- `counter`  
	simple meta-interpreter for counting the number of resolution
	steps when proving a goal

To use a meta-interpreter with an object, simply import the corresponding 
category.

Defining meta-interpreters as categories allows the use of the built-in 
predicate `clause/2` to access the clauses of object predicates without 
forcing these predicates to be declared public or protected. Within a 
category, calls to the built-in predicate `clause/2` retrieve clauses in 
"this", i.e. in the database of the object importing the category. 
