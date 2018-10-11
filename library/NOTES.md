________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
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


This folder contains useful objects, categories, and protocols. For full
documentation see:

[../docs/index.html](https://logtalk.org/docs/index.html)

A plain Prolog version of the Unicode 6.2 standard is also included in the
`unicode_data` folder. See its `README.md` file for details.

As a general rule, **always** use the corresponding loader file to load
library entities. The available loader files are described below. Most
library entities are part of small hierarchies or depend on other entities
and thus cannot be loaded and compiled separately (e.g. the `list` object
implements the `listp` protocol and is part of a basic types hierarchy).
Using the loader files takes care of all dependencies and also ensures
compilation in optimized mode.

The library loader files can be loaded using the `library(<loader file>)`
notation as argument for the compiling and loading predicates. For example:

	| ?- logtalk_load(library(random_loader)).

Currently, there are fourteen groups of entities defined, each one with
its own loader and notes files:

* assignvars  
	`assignvars_loader.lgt`
	`assignvars.md`

* dependents  
	`dependents_loader.lgt`
	`dependents.md`

* edcg  
	`edcg_loader.lgt`
	`edcg.md`

* events  
	`events_loader.lgt`
	`events.md`

* expected  
	`expected_loader.lgt`
	`expected.md`

* hierarchies  
	`hierarchies_loader.lgt`
	`hierarchies.md`

* java  
	`java_loader.lgt`
	`java.md`

* metapredicates  
	`metapredicates_loader.lgt`
	`metapredicates.md`

* optional  
	`optional_loader.lgt`
	`optional.md`

* os  
	`os_loader.lgt`
	`os.md`

* random  
	`random_loader.lgt`
	`random.md`

* statistics  
	`statistics_loader.lgt`
	`statistics.md`

* types  
	`basic_types_loader.lgt`
	`types_loader.lgt`
	`types.md`

* redis  
	`redis_loader.lgt`
	`redis.md`

For helping when embedding Logtalk and Logtalk applications:

* expand library alias paths
	`expand_library_alias_paths_loader.lgt`

There is a file named `all_loader.lgt` that will load all entities in the 
groups listed above. Simply type the goal:

	| ?- logtalk_load(library(all_loader)).

Specific notes about each group of objects, categories, and protocols can be 
found in the corresponding `*.md` files.

A `parallel_logtalk_processes_setup.pl` Prolog file is also provided with
sample code for selected backend Prolog compilers for initializing Logtalk
processes such that each process uses a unique scratch directory therefore
allowing parallel process execution (e.g. for usage at continuous integration
servers).

Some files contained in this directory represent work in progress and  are
not loaded by default by any loader utility file.

Some code in this library is based on public domain Prolog code, in particular,
code adopted from the Edinburgh Prolog library. The definition  of predicate
`reverse/2` in object list is from Richard O'Keefe and can be found in its book
"The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library proposal
available at:

	http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

HTML documentation for the library APIs can be found on the `docs`
directory (open the `docs/index.html` file with your web browser).
The documentation can be regenerated using the shell scripts
`../scripts/update_html_docs.sh` and `../scripts/update_svg_diagrams.sh`.

All source files are formatted using tabs (the recommended setting is a tab
width equivalent to 4 spaces).
