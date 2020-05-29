________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


This folder contains several examples of Logtalk programs. A brief
description of each example is included below.

Each example folder contains a `NOTES.md` file and a loader helper file
(usually named `loader.lgt`) that can be used to compile and load the
example source code. Most examples also contain a `SCRIPT.txt` file with
instructions on how to load the example and sample queries for you to try.

Most of these examples require objects, protocols, and categories that
are defined in the Logtalk standard library or in other examples. See
the `NOTES.md` files inside the library folder, plus the `NOTES.md`
and `SCRIPT.txt` files inside each example folder.

Some examples may redefine objects already loaded from other examples.
It might be necessary to restart Logtalk after playing with some examples.

Some examples have been adapted from public Prolog code or from Prolog
text book and tutorials and are copyrighted by the respective authors.

These are programming examples, meaning that you should study the source
files to fully understand them. Note, however, that some examples main
purpose is to illustrate general principles rather than being adequate,
efficient solutions for deployment code.

All examples are indented using tabs (a common setting is a tab width
equivalent to 4 spaces).

Most example folders contain two files, `tests.lgt` and `tester.lgt`. The
file `tests.lgt` contains unit tests for the example. These unit tests
are based on the sample queries found on the example `SCRIPT.txt` file.
The file `tester.lgt` is a loader file that, when loaded, will automatically
run all the example unit tests. You can automate running all these tests by
calling the `logtalk_tester` shell script from the command-line (see the
`scripts/NOTES.md` file for details on how to use with POSIX and Windows
operating-systems). Type `man logtalk_tester` or `logtalk_tester -h` for
usage details, including how to select the backend Prolog compiler.

Follows a short description of each included example (in alphabetical order):

- `ack`  
	implementation of the Ackermann arithmetic function

- `adventure`  
	some simple examples of text adventures

- `aliases`  
	example of using the `alias/2` predicate directive to provide
	alternative names to inherited predicates in order to improve
	readability or to solve multi-inheritance conflicts

- `around_methods`  
	example of defining a complementing category that uses the
	experimental `@/1` goal annotation to define an "around method"

- `aspects`  
	example of defining aspects (as in Aspect-Oriented Programming)

- `assign_parameters`  
	example of using assignable variables in the context of parametric
	objects in order to represent object state

- `assumptions`  
	simple example of implementation of ground linear and intuitionistic
	assumptions

- `attvars`  
	experimental example of using attributed variables within
	Logtalk objects and categories
	(requires Logtalk to be run with B-Prolog, SWI-Prolog, XSB, or YAP)

- `bench`  
	classic set of plain Prolog benchmark programs and Logtalk wrappers
	for those programs

- `benchmarks`  
	simple benchmarks for helping to compare the performance of Logtalk
	message sending when using different backend Prolog compilers and
	for comparing performance of message sending calls with predicate
	calls in plain Prolog and explicitly-qualified Prolog module calls
	(when applicable)

- `birds`  
	bird identification expert system
	(example adapted from the "Adventure in Prolog" Amzi! book)

- `blocks`  
	simpler version of the `bricks` example; illustrates how to use events
	to avoid breaking object encapsulation when handling object relations

- `books`  
	illustrates using the optional terms library to decouple data
	acquisition, which must be able to represent optional values,
	from data processing, which decides how to handle those values
	and their absence

- `bottles`  
	99 bottles of beer on the wall! Sing along!

- `bricks`  
	example of representation and handling of relations using events;
	illustrates how to use events to avoid breaking object encapsulation

- `carengines`  
	example of extending categories using car engines

- `cascade`  
	example of using expected terms to call a conjunction of goals
	where any of them may cause an error condition without using the
	traditional catch/throw mechanism

- `cc`  
	example of using conditional compilation directives to write code
	portable across several backend Prolog compilers

- `classmethods`  
	example of defining "class methods" as found on some class-based
	object-oriented programming languages

- `classvars`  
	example of implementation of class variables
	(as found in Smalltalk; i.e. shared instance variables)

- `closed_world_assumption`  
	example illustrating the difference between *declaring* a predicate
	and *defining* a predicate and the Closed World Assumption (CWA)
	semantics as implemented in Logtalk when calling predicates and
	sending messages

- `clustering`  
	example of using a Java library for performing clustering
	of a set of numbers

- `coinduction`  
	experimental example of coinductive predicates
	(requires Logtalk to be run with CxProlog, ECLiPSe, SICStus Prolog,
	SWI-Prolog, or YAP)

- `complements`  
	examples of using a category to explicitly complement an existing
	object, either for hot patching or for adding new functionality

- `constraints`  
	several examples of using constraints within objects and categories
	when using constraint domain solvers found in backend Prolog compilers

- `dcgs`  
	examples of using DCG rules inside objects and categories

- `debug_hooks`  
	simple example of using compilation hooks and term expansion for
	conditional compilation of debug statements

- `defaulty`  
	example comparing defaulty and tagged data representations

- `delegates`  
	simple implementation of the delegation design pattern

- `design_patterns`
	sample implementation of common object-oriented behavioral,
	creational, and structural design patterns

- `diamonds`  
	examples of problems and solutions for the "diamond problem"
	(multi-inheritance conflicts and ambiguities)

- `document_converter`  
	example of using a Java library for converting documents to text

- `dynpred`  
	example of using the built-in database handling methods to
	implement dynamic object state

- `edcgs`  
	several examples of Extended Definite Clause Grammars (EDCGs)

- `encodings`  
	very simple example of using the experimental `encoding/1`
	directive (requires Logtalk to be run with YAP, SWI-Prolog,
	CxProlog, JIProlog, SICStus Prolog, or Lean Prolog)

- `engines`  
	examples of using threaded engines to implement fluents, lazy
	meta-predicates, interactors, and simple agents (requires Logtalk
	to be run with ECLiPSe, SWI-Prolog, XSB, or YAP)

- `errors`  
	example showing the Logtalk compiler warning and error reporting
	for common programming errors

- `expansion`  
	example illustrating the term and goal expansion mechanisms

- `family`  
	classical family relations example

- `family_alt`  
	alternative version of the `family` example using a family registry
	implemented using multifile predicates to avoid dynamic binding

- `figures`  
	an example of network modeling for recognizing polyhedra represented
	as graphs

- `futures`  
	simple example of working with futures, a common concurrent programming
	idiom, using the high-level multi-threading predicates

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

- `inlining`  
	simple example for illustrating and testing inlining of predicate
	definitions

- `instmethods`  
	example of instance defined methods; also illustrates the use of
	"super calls" to call overridden method definitions

- `instvars`  
	example of defining instance variables, default variable values,
	and setter and getter methods

- `jpl`  
	examples adapted from SWI-Prolog/YAP JPL library; illustrates how
	to use the `java` library minimal abstraction for calling Java
	from Logtalk using familiar message sending syntax (requires Logtalk
	to be run with SWI-Prolog, YAP, or JIProlog as the backend compiler)

- `lambdas`  
	example of using lambda expressions

- `lambdas_compiled`  
	pseudo-example for testing compilation of calls to library
	meta-predicates with lambda expressions as meta-arguments

- `laptops`  
	example of defining an object as a composition of other objects
	in order to contrast with category-based composition

- `lo`  
	examples adapted from the Francis G. McCabe L&O system

- `localizations`  
	simple example of supporting application localization in multiple
	languages

- `logic`  
	example of a translator of first-order predicate logic propositions
	to conjunctive normal form and to clausal form

- `logs`  
	example of using a category to define a simple log support for objects

- `lpa`  
	examples adapted from the LPA Prolog++ system (an expert system for
	automobile fault diagnosis and a timetables example)

- `metaclasses`
	example of using classes and metaclasses

- `metainterpreters`  
	some examples of simple meta-interpreters defined as categories
	that can be imported by "database" objects

- `metapredicates`  
	example of using meta-predicates in Logtalk objects

- `metapredicates_compiled`  
	pseudo-example for testing compilation of calls to library
	meta-predicates

- `mi`  
	simple multi-inheritance examples

- `miscellaneous`  
	unsorted examples

- `missing_data`  
	illustrates using the expected terms library to decouple data
	acquisition, which must be resilient to unexpected events,
	from data processing, which decides how to handle those events

- `modules`  
	simple example of compiling Prolog modules as objects

- `msglog`  
	example of using events and monitors for recording, replaying,
	and printing user messages

- `multifile`  
	example illustrates how to use multifile predicates within
	Logtalk objects and categories

- `my_types`  
	example of defining new types using the user-extensible `type`
	library object

- `named_databases`  
	example of an implementation of the Lean Prolog API for named
	databases for Prolog compilers with a module system

- `now_you_see_me`  
	example illustrating requirements dictated by inheritance semantics
	for the implementation of dynamic predicates

- `object_aliases`  
	simple examples illustrating the use of object aliases

- `operators`  
	example of using operators local to objects and categories

- `parametric`  
	simple examples of parametric objects

- `pardicts`  
	simple SWI-Prolog only example of using a dictionary term
	for representing object parameters

- `parvars`  
	variant of the `parametric` example using parameter variables

- `patches`  
	example of using complementing categories to patch broken
	object code

- `patching`  
	another example of using complementing categories to patch
	broken object code

- `pengines`  
	simple example of using SWI-Prolog pengines from objects

- `people`  
	simple example of defining object constructors

- `persistency`  
	illustrates a very simple solution for persisting an object
	dynamic state across sessions

- `planets`  
	simple example illustrating the concepts of protocol and category

- `poem`  
	examples adapted from the Ben Staveley-Taylor POEM system

- `points`  
	example adapted from SICStus Objects documentation; defines
	a simple class hierarchy of points illustrating how to use
	categories as object components

- `polygons`  
	example of representation and handling of relations using events

- `predicate_lookups`  
	example illustrating the predicate declaration and predicate definition
	lookup algorithms used when sending a message to an object

- `process_modeling`  
	example of using parametric objects to represent and restore
	shared variables between sets of constraints that are stored
	in different objects

- `profiling`  
	examples of using of events and monitors to implement profilers

- `prototypes`  
	example illustrating the concept of prototypes

- `proxies`  
	example of using parametric object proxies for an efficient
	representation of objects with read-only state

- `puzzles`  
	several examples of solving logical puzzles

- `quick_check`  
	example of using the QuickCheck support provided by the `lgtunit`
	tool both for interactive testing at the top-level interpreter and
	for defining unit tests

- `recipes`  
	example of a possible solution for representing structured data
	and also of hot patching of running code

- `reflection`  
	example of a simple class-based reflective system

- `relations`  
	objects implementing predicates for dealing with relations and
	constrained relations between objects; used by other examples

- `roles`  
	simple example illustrating the different roles that can be
	played by an object

- `roots`  
	objects, protocols, and categories used by other examples;
	illustrates how you can define object creation and abolishing
	methods, complete with initialization and termination options

- `scopes`  
	simple example illustrating predicate scope semantics

- `searching`  
	state-space searching framework
	(this example includes some code adapted from Ivan Bratko's "Prolog
	Programming for Artificial Intelligence" book)

- `securemp`  
	a set of source files for testing Logtalk secure
	implementation of meta-predicates

- `self_messages`  
	simple example illustrating the semantics of messages to "self"

- `serialization`  
	simple example of serializing objects to a file

- `shapes`  
	simple geometric shapes implemented as both a prototype hierarchy 
	and a class hierarchy for comparing both approaches

- `sicstus`  
	examples adapted from SICStus Objects documentation

- `slides`  
	example of using the library list zippers

- `super_calls`  
	simple example illustrating that "super" calls preserve the value
	of "self"

- `symbiosis`
	examples of using Prolog non-standard built-in meta-predicates
	and module meta-predicates that take closures as arguments

- `symdiff`  
	example of using parametric objects to implement symbolic
	expression differentiation and simplification

- `tabling`  
	simple example of using tabling directives within objects
	(requires Logtalk to be run with B-Prolog, SWI-Prolog, XSB, or YAP)

- `testing`  
	some examples of writing unit tests

- `threads`  
	several simple examples of multi-threading programming, some of
	them intended only for benchmarking multi-threading Prolog compilers
	(requires Logtalk to be run with YAP, SWI-Prolog, or XSB)

- `viewpoints`  
	example on how to implement property sharing and value sharing
	with prototypes

- `wrappers`  
	example of using the `begin_of_file` term generated when compiling
	a source file to define object wrappers for plain Prolog code

- `xpce`  
	SWI-Prolog only example of using XPCE from Logtalk
