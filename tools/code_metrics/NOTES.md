________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2017-2025 Paulo Moura <pmoura@logtalk.org>
SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
SPDX-License-Identifier: Apache-2.0

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


`code_metrics`
==============

The purpose of this tool is to assess qualities of source code that may
predict negative aspects such as entity coupling, cohesion, complexity,
error-proneness, and overall maintainability. It is meant to be extensible
via the addition of objects implementing new metrics.

This tool provides predicates for computing metrics for source files,
entities, libraries, files, and directories. The actual availability
of a particular predicate depends on the specific metric. A set of
predicates prints, by default, the computed metric values to the standard
output. A second set of predicates computes and returns a score (usually
a compound term with the computed metric values as arguments).


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#code-metrics](../../docs/library_index.html#code-metrics)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(code_metrics(loader)).


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(code_metrics(tester)).


Available metrics
-----------------

Currently, the following metrics are provided:

- Number of Clauses (`noc_metric`)
- Number of Rules (`nor_metric`)
- Unique Predicate Nodes (`upn_metric`)
- Cyclomatic Complexity (`cc_metric`)
- Depth of Inheritance (`dit_metric`)
- Efferent coupling, afferent coupling, instability, and abstractness (`coupling_metric`)
- Documentation (`doc_metric`)
- Source code size (`size_metric`)
- Halstead complexity (`halstead_metric` and `halstead_metric(Stroud)`)

A helper object, `code_metrics`, is also provided, allowing running all
loaded individual metrics. For code coverage metrics, see the `lgtunit`
tool documentation.


Coupling metrics
----------------

- Efferent coupling (`Ce`): Number of entities that an entity depends on.
These include objects receiving messages from the entity, plus the implemented
protocols, imported categories, and extended/instantiated/specialized objects.

- Afferent coupling (`Ca`): Number of entities that depend on an entity. For
a protocol, the number of protocols that extend it, plus the number of objects
and categories that implement it. For a category, the number of objects that
import it. For an object, the number of categories and objects that send
messages to it plus the number of objects that extend/instantiate/specialize
it.

- Instability: Computed as `Ce / (Ce + Ca)`. Measures an entity resilience
to change. Ranging from 0.0 to 1.0, with 0.0 indicating a maximally stable
entity and 1.0 indicating a maximally unstable entity. Ideally, an entity
is either maximally stable or maximally unstable.

- Abstractness: Computed as the ratio between the number of static predicates
with scope directives without a local definition and the number of static
predicates with scope directives. Measures the rigidity of an entity. Ranging
from 0.0 to 1.0, with 0.0 indicating a fully concrete entity and 1.0
indicating a fully abstract entity.

The dependencies count includes direct entity relations plus predicate calls
or dynamic updates to predicates in external objects or categories.

For more information on the interpretation of the coupling metric scores,
see, e.g., the original paper by Robert Martin:

	@inproceedings{citeulike:1579528,
		author = "Martin, Robert",
		booktitle = "Workshop Pragmatic and Theoretical Directions in Object-Oriented Software Metrics",
		citeulike-article-id = 1579528,
		citeulike-linkout-0 = "http://www.objectmentor.com/resources/articles/oodmetrc.pdf",
		keywords = "diplomarbeit",
		organization = "OOPSLA'94",
		posted-at = "2007-08-21 11:08:44",
		priority = 0,
		title = "OO Design Quality Metrics - An Analysis of Dependencies",
		url = "http://www.objectmentor.com/resources/articles/oodmetrc.pdf",
		year = 1994
	}

The coupling metric was also influenced by the metrics rating system in
Microsoft Visual Studio and aims to eventually emulate the functionality
of a maintainability index score.


Halstead metric
---------------

Predicates declared, user-defined, and called are interpreted as _operators_.
Built-in predicates and built-in control constructs are ignored. Predicate
arguments are abstracted, assumed distinct, and interpreted as _operands_.
Note that this definition of operands is a significant deviation from the
original definition, which used syntactic literals. A computation closer to
the original definition of the metric would require switching to use the
parser to collect information on syntactic literals, which would imply a
much larger computation cost. The number of predicate calls doesn't include
calls to built-in predicates and can underestimate recursive calls.

The computation of this metric is parameterized by the _Stroud_ coefficient
for computing the time required to program (default is 18). The following
individual measures are computed:

- Number of distinct predicates (declared, defined, called, or updated; `Pn`).
- Number of predicate arguments (assumed distinct; `PAn`).
- Number of predicate calls/updates + number of clauses (`Cn`).
- Number of predicate call/update arguments + number of clause head arguments (`CAn`).
- Entity vocabulary (`EV`). Computed as `EV = Pn + PAn`.
- Entity length (`EL`). Computed as `EL = Cn + CAn`.
- Volume (`V`). Computed as `V = EL * log2(EV)`.
- Difficulty (`D`). Computed as `D = (Pn/2) * (CAn/An)`.
- Effort (`E`). Computed as `E = D * V`.
- Time required to program (`T`). Computed as `T = E/k` seconds (where `k` is the Stroud number; defaults to 18).
- Number of delivered bugs (`B`). Computed as `B = V/3000`.


UPN metric
----------

The Unique Predicate Nodes (UPN) metric is described in the following
paper:

	@article{MOORES199845,
		title = "Applying Complexity Measures to Rule-Based Prolog Programs",
		journal = "Journal of Systems and Software",
		volume = "44",
		number = "1",
		pages = "45 - 52",
		year = "1998",
		issn = "0164-1212",
		doi = "https://doi.org/10.1016/S0164-1212(98)10042-0",
		url = "http://www.sciencedirect.com/science/article/pii/S0164121298100420",
		author = "Trevor T Moores"
	}

The nodes include called and updated predicates independently of where
they are defined. It also includes multifile predicates contributed to
other entities.


Cyclomatic complexity metric
----------------------------

The cyclomatic complexity metric evaluates an entity code complexity by
measuring the number of linearly independent paths through the code. In
its current implementation, all defined predicates that are not called
or updated are counted as graph-connected components (the reasoning being
that these predicates can be considered entry points). The implementation
uses the same predicate abstraction as the UPN metric. The defined predicates
include multifile predicate definitions contributed by the entity to other
entities.

For more details on this metric, see the original paper by Thomas J. McCabe:

	@inproceedings{McCabe:1976:CM:800253.807712,
		author = "McCabe, Thomas J.",
		title = "A Complexity Measure",
		booktitle = "Proceedings of the 2Nd International Conference on Software Engineering",
		series = "ICSE '76",
		year = 1976,
		location = "San Francisco, California, USA",
		pages = "407--",
		url = "http://dl.acm.org/citation.cfm?id=800253.807712",
		acmid = 807712,
		publisher = "IEEE Computer Society Press",
		address = "Los Alamitos, CA, USA",
		keywords = "Basis, Complexity measure, Control flow, Decomposition, Graph theory, Independence, Linear, Modularization, Programming, Reduction, Software, Testing",
	}


Usage
-----

All metrics require the source code to be analyzed to be loaded with the
`source_data` flag turned on. For usage examples, see the `SCRIPT.txt`
file in the tool directory.

Be sure to fully understand the metrics individual meanings and any
implementation limitations before using them to support any evaluation
or decision process.


Excluding code from analysis
----------------------------

A set of options is available to specify code that should be excluded when
applying code metrics:

- `exclude_directories(Directories)`  
	list of directories to exclude (default is `[]`); all sub-directories of the excluded directories are also excluded; directories may be listed by full or relative path

- `exclude_files(Files)`  
	list of source files to exclude (default is `[]`); files may be listed by full path or basename, with or without extension

- `exclude_libraries(Libraries)`  
	list of libraries to exclude (default is `[startup, scratch_directory]`)

- `exclude_entities(Entities)`  
	list of entities to exclude (default is `[]`)


Defining new metrics
--------------------

New metrics can be implemented by defining an object that imports the
`code_metric` category and implements its score predicates. There is
also a `code_metrics_utilities` category that defines useful predicates
for the definition of metrics.


Third-party tools
-----------------

The following open-source command-line programs can count blank lines,
comment lines, and lines of source code in many programming languages,
including Logtalk:

- `cloc` - https://github.com/AlDanial/cloc

- `ohcount` - https://github.com/blackducksoftware/ohcount

- `tokei` - https://github.com/XAMPPRocky/tokei


Applying metrics to Prolog modules
----------------------------------

Some of the metrics can also be applied to Prolog modules that Logtalk is
able to compile as objects. For example, if the Prolog module file is named
`module.pl`, try:

	| ?- logtalk_load(module, [source_data(on)]).

Due to the lack of standardization of module systems and the abundance of
proprietary extensions, this solution is not expected to work for all cases.


Applying metrics to plain Prolog code
-------------------------------------

Some of the metrics can also be applied to plain Prolog code. For example,
if the Prolog file is named `code.pl`, simply define an object including
its code:

	:- object(code).
		:- include('code.pl').
	:- end_object.

Save the object to an e.g. `code.lgt` file in the same directory as the
Prolog file and then load it in debug mode:

	| ?- logtalk_load(code, [source_data(on)]).

In alternative, use the `object_wrapper_hook` provided by the `hook_objects`
library:

	| ?- logtalk_load(hook_objects(loader)).
	...

	| ?- logtalk_load(code, [hook(object_wrapper_hook), source_data(on)]).

With either wrapping solution, pay special attention to any compilation
warnings that may signal issues that could prevent the plain Prolog code
from working when wrapped by an object.
