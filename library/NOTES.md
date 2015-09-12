________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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


This folder contains some useful objects, categories, and protocols. 

To load a group of objects, protocols, and categories in this library 
either change your Prolog working directory to this folder and then 
compile and load the corresponding loader utility file or simply use 
the notation `library(<loader file>)` as argument for the compiling
and loading predicates. For example:

	| ?- logtalk_load(library(random_loader)).

Currently, there are eleven groups of entities defined, each one with
its own loader and notes files:

* assignvars  
	`assignvars_loader.lgt`
	`assignvars.txt`

* dependents  
	`dependents_loader.lgt`
	`dependents.txt`

* events  
	`events_loader.lgt`
	`events.txt`

* hierarchies  
	`hierarchies_loader.lgt`
	`hierarchies.txt`

* metapredicates  
	`metapredicates_loader.lgt`
	`metapredicates.txt`

* os  
	`os_loader.lgt`
	`os.txt`

* random  
	`random_loader.lgt`
	`random.txt`

* statistics  
	`statistics_loader.lgt`
	`statistics.txt`

* types  
	`basic_types_loader.lgt`
	`types_loader.lgt`
	`types.txt`

There is also a file named `all_loader.lgt` that will load all entities in the 
groups listed above. Simply type the goal:

	| ?- logtalk_load(library(all_loader)).

Specific notes about each group of objects, categories, and protocols can be 
found in the corresponding `*.txt` files.

Some of the files contained in this directory represent work in progress and 
are not loaded by default by any loader utility file.

Some of the code in this library is based on public domain Prolog code, in 
particular, code adopted from the Edinburgh Prolog library. The definition 
of predicate `reverse/2` in object list is from Richard O'Keefe and can be
found in its book "The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library proposal
available at:

	http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

All source files are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).

The `$LOGTALKUSER/docs` directory includes a XHTML version of the library
documentation. To regenerate documentation of the Logtalk libraries, start
Logtalk with your favorite back-end Prolog compiler and follow these steps:

(1) If the `source_data` flag is not `on` by default, type the query:

	| ?- set_logtalk_flag(source_data, on).

(2) Load all library entities using the query:

	| ?- {library(all_loader)}.

(3) Load the `lgtdoc` tool and generate the XML documenting files for all
library entities using the queries:

	| ?- {lgtdoc(loader)}.
	...
	| ?- lgtdoc::rlibrary(library, [xmldir('$LOGTALKUSER/docs/tmp')]).

(4) Run the command `lgt2html` on the `$LOGTALKUSER/docs/tmp` directory
to generate (X)HTML documentation or the command `lgt2pdf` to generate PDF
documentation. For example:

	$ cd $LOGTALKUSER/docs/tmp
	$ lgt2html -i library.html -t "Library documentation index" && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ rm -rf $LOGTALKUSER/docs/tmp
