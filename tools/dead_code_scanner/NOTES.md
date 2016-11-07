________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 2016 Barry Evans <barryevans@kyndi.com>  
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


Overview
--------

This tool detects *likely* dead code in Logtalk entities and in Prolog modules
compiled as objects. Predicates (and non-terminals) are classified as dead code
when (1) there is no scope directive for them and (2) are not called, directly
or indirectly, by any predicate with a (local or inherited) scope directive.
Predicates (and non-terminals) listed in `uses/2` and `use_module/2` directives
but that are not used are also classified as dead code.

Basides dead code, this tool can also help detect other problems in the code
than often result in reporting false positives. For example, typos in `alias/2`
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

To consult this tool API documentation, open in a web browser the link:

[docs/directory_index.html#tools/dead_code_scanner/](http://logtalk.org/docs/directory_index.html#tools/dead_code_scanner/)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(dead_code_scanner(loader)).


Known issues
------------

Use of local meta-calls with goal arguments only know at runtime can result
in false positives. When using library or user-defined meta-predicates, it
can be helpful to compile the source files with the `optimize` flag turned
on so that the meta-calls may be resolved at compile time and thus allow
calling information for the meta-arguments to be recorded.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
