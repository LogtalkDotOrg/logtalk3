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


About
-----

This folder contains libraries of useful objects, categories, and protocols.
Specific notes about individual libraries can be found in the corresponding
library directory `NOTES.md` files.

A plain Prolog version of the Unicode 6.2 standard is also included in the
`unicode_data` folder. See its `README.md` file for details.

A `parallel_logtalk_processes_setup.pl` Prolog file is also provided with
sample code for selected backend Prolog compilers for initializing Logtalk
processes such that each process uses a unique scratch directory therefore
allowing parallel process execution (e.g. for usage at continuous integration
servers).


Documentation
-------------

For full documentation see:

[../docs/index.html](https://logtalk.org/docs/index.html)

The documentation can be regenerated using the shell scripts
`../scripts/update_html_docs.sh` and `../scripts/update_svg_diagrams.sh`.


Loading libraries
-----------------

All the individual libraries can be loaded using the `<library name>(loader)`
notation as argument for the compiling and loading predicates. For example:

	| ?- logtalk_load(random(loader)).

For existing applications still relying on the old library `*_loader.lgt`
files, these loader files are still provided but are considered deprecated.

There is a file named `all_loader.lgt` that will load all libraries. Simply
type the goal:

	| ?- logtalk_load(library(all_loader)).

As a general rule, always use the corresponding loader file to load a
library. Most library entities are part of small hierarchies or depend on
other libraries and thus cannot be loaded and compiled separately (e.g. the
`list` object implements the `listp` protocol and is part of a basic types
hierarchy). Using the loader files takes care of all dependencies and also
ensures compilation in optimized mode.


Credits
-------

Some code in this library is based on public domain Prolog code, in particular,
code adopted from the Edinburgh Prolog library. The definition  of predicate
`reverse/2` in object list is from Richard O'Keefe and can be found in its book
"The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library proposal
available at:

	http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm


Other notes
-----------

Some files contained in this directory represent work in progress and are
not loaded by default by any loader utility file.

All source files are indented using tabs (a common setting is a tab width
equivalent to 4 spaces).
