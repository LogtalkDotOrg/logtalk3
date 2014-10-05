________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example is inspired by a StackOverflow question for which a sketch of a
Logtalk-based solution was provided:

	http://stackoverflow.com/questions/26119110/knowledge-representation-in-prolog-how-to-store-data

It illustrates a possible solution for representing structured data using
objects and also hot patching of running code. It also shows when a mixed
representation using both data objects and Prolog facts can be managed.

The main files defined in this example are:

- `specs.lgt`  
	defines the recipe protocol, `recipep`, declaring recipe description
	predicates and a prototype object, `proto_recipe`, defining handy
	predicates for summarizing recipe information
- `recipes.lgt`  
	defines some recipes represented as objects
- `parametric.lgt`  
	defines some recipes represented as facts for a `recipe/3` predicate
	plus a `recipe/3` parametric object and a `recipe/1` predicate for
	enumerating recipes while abstracting their representation

- `patch_1.lgt` and `patch_2.lgt`  
	categories used to illustrate hot patching of the `proto_recipe` object
