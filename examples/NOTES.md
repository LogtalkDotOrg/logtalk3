________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Additional licensing terms apply per Section 7 of the GNU General
Public License 3. Consult the `LICENSE.txt` file for details.
________________________________________________________________________


This folder contains several examples of Logtalk programs. A brief 
description of each example is included below.

Each example folder contains a `NOTES.md` file and a loader helper file 
(usually `loader.lgt`) that may be used to load all the example entities. 
Most examples also contain a `SCRIPT.txt` file with instructions on how 
to load the example and sample queries for you to try.

Most of these examples require objects, protocols, and categories that 
are defined in the Logtalk standard library or in other examples. See 
the `NOTES.md` files inside the library folder, plus the `NOTES.md` 
and `SCRIPT.txt` files inside each example folder.

Some examples may redefine objects already loaded from other examples.
It might be necessary to restart Logtalk after playing with some examples.

Some of the examples have been adapted from public available Prolog code 
or from known Prolog text books and are copyrighted by the respective 
authors.

These are programming examples, meaning that you should study the source 
files to fully understand them. However, note that some examples purpose 
is to illustrate general principles rather than being adequate, efficient 
solutions for deployment code.

All examples are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).

The example folders may contain two files, `tests.lgt` and `tester.lgt`.
The file `tests.lgt` contains unit tests for the example. These unit tests
are based on the sample queries found on the example `SCRIPT.txt` file.
The file `tester.lgt` is a loader file that, when loaded, will automatically
run all the example unit tests. You can automate running all these tests by
calling the `logtalk_tester` shell script from the command-line (see the
`scripts/NOTES.md` file for details on how to use with POSIX and Windows
operating-systems). Type `man logtalk_tester` or `logtalk_tester -h` for
usage details, including how to select the back-end Prolog compiler.

Follows a short description of each included example (in alphabetical order):

- `ack`  
	implementation of the Ackermann arithmetic function

- `adventure`  
    some simple examples of text adventures

- `aliases`  
	example of using the `alias/2` predicate directive to provide 
	alternative names to inherited predicates in order to improve 
	readability or to solve multi-inheritance conflicts

- `assignvars`  
	example of using assignable variables in the context of parametric 
	objects in order to represent object state

- `attvars`  
	experimental example of of using attributed variables within
	Logtalk objects and categories
	(requires Logtalk to be run with B-Prolog, SWI-Prolog, XSB, or YAP)

- `benchmarks`  
	simple benchmarks for helping measuring performance of Logtalk 
	message sending between Prolog compilers and for comparing 
	performance of message sending calls with predicate calls in 
	plain Prolog

- `birds`  
	bird identification expert system
	(example adapted from the Adventure in Prolog Amzi! book)

- `bottles`  
	99 bottles of beer on the wall! Sing along!

- `bricks`  
	example of representation and handling of relations using events;
	illustrates how to use events to avoid breaking object encapsulation

- `cc`  
	example of using conditional compilation directives to write code
	portable across several back-end Prolog compilers

- `classmethods`  
	example of defining "class methods" as found on some class-based
	object-oriented programming languages

- `classvars`  
	example of implementation of class variables
	(as found in Smalltalk; i.e. shared instance variables)

- `coinduction`  
	experimental example of coinductive predicates
	(requires Logtalk to be run with CxProlog, ECLiPSE, SICStus Prolog,
	SWI-Prolog, or YAP)

- `complements`  
	example of using a category to explicitly complement an existing 
	object

- `constraints`  
	several examples of using constraints within objects and categories
	when using constraint domain solvers found in back-end Prolog compilers

- `dcgs`  
	examples of using DCG rules inside objects and categories

- `debug_hooks`  
	simple example of using compilation hooks and term expansion for 
	conditional compilation of debug statements

- `delegates`  
	simple implementation of the delegation design pattern

- `diamonds`  
	examples of problems and solutions for the "diamond problem"
	(multi-inheritance conflicts and ambiguities)

- `dynpred`  
	example of using some of the built-in database handling methods 
	in order to implement dynamic object state

- `encodings`  
	very simple example of using the new, experimental `encoding/1` 
	directive (requires Logtalk to be run with YAP, SWI-Prolog,
	CxProlog, K-Prolog, SICStus Prolog, or Lean Prolog)

- `engines`  
	example of category composition (extension of categories by 
	other categories) using car engines

- `errors`  
	example showing the Logtalk compiler warning and error reporting
	for common programming errors

- `expansion`  
	example illustrating the term and goal expansion mechanisms

- `hailstone`  
	example of computing Hailstone sequences

- `hello_world`  
	the unavoidable "hello world" programming example

- `hooks`  
	simple example of using compiler hook objects and predicates

- `includes`  
	simple example of using the `include/1` directive as both a
	file directive and an entity directive

- `inheritance`  
	examples of public, protected, and private inheritance using both 
	prototypes and classes/instances

- `instmethods`  
	example of instance defined methods; also illustrates the use of 
	"super calls" to call overridden method definitions

- `instvars`  
	example of defining instance variables, default variable values,
	and setter and getter methods

- `jpl`  
	example of defining a minimal abstraction of the JPL API for
	calling Java from Logtalk using familiar message sending syntax

- `lambdas`  
	example of using lambda expressions

- `lambdas_compiled`  
	pseudo-example for testing compilation of calls to library
	meta-predicates with lambda expressions as meta-arguments

- `lo`  
	examples adapted from the Francis G. McCabe L&O system

- `logging`  
	example of using a category to define a simple logging support for 
	objects

- `logic`  
	example of a translator of first-order predicate logic propositions 
	to conjunctive normal form and to clausal form

- `lpa`  
	examples adapted from the LPA Prolog++ system

- `metaclasses`
	example of using classes and metaclasses

- `metapredicates`  
	example of using meta-predicates in Logtalk objects

- `metapredicates_compiled`  
	pseudo-example for testing compilation of calls to library
	meta-predicates

- `metainterpreters`  
	some examples of simple meta-interpreters defined as categories 
	that can be imported by "database" objects

- `mi`  
	simple multi-inheritance examples

- `miscellaneous`  
	unsorted examples

- `modules`  
	simple example of compiling Prolog module files as objects

- `msglog`  
	example of using events and monitors for recording, replaying, 
	and printing user messages

- `multifile`  
	example illustrates how to use multifile predicates within
	Logtalk objects and categories

- `named_databases`
    example of an implementation of Lean Prolog API for named
    databases for Prolog compilers with a module system

- `operators`  
	example of using operators local to objects and categories

- `patching`  
	example of using complementing categories to patch broken
	object code

- `parametric`  
	simple example of parametric objects

- `pardicts`  
	simple SWI-Prolog only example of using a dictionary term
	for representing object parameters

- `people`  
	simple example of defining object constructors

- `poem`  
	examples adapted from the Ben Staveley-Taylor POEM system

- `points`  
	example adapted from SICStus Objects documentation; defines 
	a simple class hierarchy of points illustrating how to use 
	categories as object components

- `polygons`  
	example of representation and handling of relations using events

- `profiling`  
	examples of using of events and monitors to implement profilers

- `prototypes`  
	example illustrating the concept of prototypes

- `proxies`  
	example of using parametric object proxies for an efficient 
	representation of objects with read-only state

- `puzzles`  
	several examples of solving logical puzzles

- `reflection`  
	example of a simple class-based reflective system

- `relations`  
	objects implementing predicates for dealing with relations and 
	constrained relations between objects; used by other examples

- `roots`  
	objects, protocols, and categories used by some of the other 
	examples; illustrates how you can define object creation and 
	abolishing methods, complete with initialization and termination 
	options

- `searching`  
	state-space searching framework
	(this example includes some code adapted from Ivan Bratko's "Prolog
	Programming for Artificial Intelligence" book)

- `securemp`  
	a set of source files for testing Logtalk secure
	implementation of meta-predicates

- `shapes`  
	simple geometric shapes implemented as both a prototype hierarchy 
	and a class hierarchy

- `sicstus`  
	examples adapted from SICStus Objects documentation

- `symbiosis`
	examples of using Prolog built-in meta-predicates and module
	meta-predicates that take closures as arguments

- `symdiff`  
	example of using parametric objects to implement symbolic 
	expression differentiation and simplification

- `tabling`  
	simple example of using tabling directives within objects
	(requires Logtalk to be run with B-Prolog, XSB, or YAP)

- `testing`  
	some examples of writing unit tests

- `threads`  
	several simple examples of multi-threading programming, some of 
	them intended only for benchmarking multi-threading Prolog compilers
	(requires Logtalk to be run with YAP, SWI-Prolog, or XSB)

- `viewpoints`  
	example on how to implement property sharing and value sharing 
	with prototypes

- `xpce`  
	SWI-Prolog only example of using XPCE from Logtalk
