________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2016 Barry Evans <barryevans@kyndi.com> and  
Paulo Moura <pmoura@logtalk.org>

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


`dead_code_scanner`
===================

This tool detects *likely* dead code in Logtalk entities and in Prolog modules
compiled as objects. Predicates (and non-terminals) are classified as dead code
when:

- There is no scope directive for them and they are not called, directly or
indirectly, by any predicate with a (local or inherited) scope directive.
- They are listed in `uses/2` and `use_module/2` directives but not called.

Besides dead code, this tool can also help detect other problems in the code
that often result in reporting false positives. For example, typos in `alias/2`
directives, missing scope directives, and missing `meta_non_terminal/1` and
`meta_predicate/1` directives.

Given the possibility of false positives, care must be taken before deleting
reported dead code to ensure that it's, in fact, code that is not used.
A common cause of false positives is the use of conditional compilation
directives to provide implementations for predicates missing in some systems.

The `dead_code_scanner.lgt` source file implements the scanning predicates for
finding dead code in entities, libraries, and directories. The source file
`dead_code_scanner_messages.lgt` defines the default translations for the
messages printed when scanning for dead code. These messages can be intercepted
to customize output, e.g. to make it less verbose, or for integration with e.g.
GUI IDEs and continuous integration servers.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#dead-code-scanner](../../docs/library_index.html#dead-code-scanner)

For sample queries, please see the `SCRIPT.txt` file in the tool directory.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(dead_code_scanner(loader)).


Usage
-----

This tool provides a set of predicates that allows scanning entities,
libraries, files, and directories. See the tool API documentation for
details. The source code to be analyzed should be loaded with the
`source_data` and `optimize` flags turned on (possibly set in a loader
file).

As an example, assume that we want to scan an application with a library
alias `my_app`. The following goals could be used:

	| ?- set_logtalk_flag(source_data, on),
	     set_logtalk_flag(optimize, on).
	yes

	| ?- logtalk_load(my_app(loader)).
	...
	yes

	| ?- dead_code_scanner::library(my_app).
	...

For complex applications that make use of sub-libraries, there is also a
`rlibrary/1` predicate that performs a recursive scan of a library and all
its sub-libraries. Conversely, we may be interested in scanning a single
entity:

	| ?- dead_code_scanner::entity(some_object).
	...

For other usage examples, see the `SCRIPT.txt` file in the tool directory.


Integration with the `make` tool
--------------------------------

The `loader.lgt` file sets a make target action that will call the
`dead_code_scanner::all` goal whenever the `logtalk_make(check)` goal
(or its top-level abbreviation, `{?}`) is called.


Caveats
-------

Use of local meta-calls with goal arguments only known at runtime can result
in false positives. When using library or user-defined meta-predicates,
compilation of the source files with the `optimize` flag turned on may allow
meta-calls to be resolved at compile time and thus allow calling information
for the meta-arguments to be recorded, avoiding false positives for predicates
that are only meta-called.


Scanning Prolog modules
-----------------------

This tool can also be applied to Prolog modules that Logtalk is able to compile
as objects. For example, if the Prolog module file is named `module.pl`, try:

	| ?- logtalk_load(module, [source_data(on)]).

Due to the lack of standardization of module systems and the abundance of
proprietary extensions, this solution is not expected to work for all cases.


Scanning plain Prolog files
---------------------------

This tool can also be applied to plain Prolog code. For example, if the Prolog
file is named `code.pl`, simply define an object including its code:

	:- object(code).
		:- include('code.pl').
	:- end_object.

Save the object to an e.g. `code.lgt` file in the same directory as the
Prolog file and then load it in debug mode:

	| ?- logtalk_load(code, [source_data(on), optimize(on)]).

In alternative, use the `object_wrapper_hook` provided by the `hook_objects`
library:

	| ?- logtalk_load(hook_objects(object_wrapper_hook)).
	...

	| ?- logtalk_load(code, [hook(object_wrapper_hook), source_data(on), optimize(on)]).

With either wrapping solution, pay special attention to any compilation
warnings that may signal issues that could prevent the plain Prolog from
being fully analyzed when wrapped by an object.
