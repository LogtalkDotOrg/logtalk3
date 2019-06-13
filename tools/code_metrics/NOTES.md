________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and  
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


`code_metrics`
==============

The purpose of this tool is to assess qualities of source code that may
predict negative aspects such as entity coupling, cohesion, complexity,
error-proneness, and overall maintainability. It is meant to be extensible
via the addition of objects implementing new metrics.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/library_index.html#code-metrics](https://logtalk.org/docs/library_index.html#code-metrics)

For sample queries, please see the [SCRIPT.txt](SCRIPT.txt) file.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(code_metrics(loader)).


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
- Halstead complexity (`halstead_metric`)

All metrics require the source code to be analyzed to be loaded with the
`source_data` flag turned on.

A helper object, `code_metrics`, is also provided allowing running all
loaded individual metrics.

For usage examples, see the `SCRIPT.txt` file.

For code coverage metrics, see the `lgtunit` tool documentation.

For interpretation of the coupling metric scores, see e.g. the original
paper by Robert Martin, "OO Design Quality Metrics":

	@inproceedings{citeulike:1579528,
		author = {Martin, Robert},
		booktitle = {Workshop Pragmatic and Theoretical Directions in Object-Oriented Software Metrics},
		citeulike-article-id = {1579528},
		citeulike-linkout-0 = {http://www.objectmentor.com/resources/articles/oodmetrc.pdf},
		keywords = {diplomarbeit},
		organization = {OOPSLA'94},
		posted-at = {2007-08-21 11:08:44},
		priority = {0},
		title = {{OO} Design Quality Metrics - An Analysis of Dependencies},
		url = {http://www.objectmentor.com/resources/articles/oodmetrc.pdf},
		year = {1994}
	}

The Halstead metric computation uses the reflection API for performance.
The main consequence of this choice is that we abstract all predicate
arguments. A computation closer to the original definition of the metric
would require switching to use the parser to collect information on
syntactic literals, which would imply a much large computation cost.

The coupling metric was also influenced by the metrics rating system in
Microsoft Visual Studio and aims to eventually emulate the functionality
of a maintainability index score.

The unique predicate nodes (UPN) metric is described in the following
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

The cyclomatic complexity metric uses the same predicate abstraction as the
UPN metric and it is also described in the above paper besides the original
paper by Thomas J. McCabe:

	@inproceedings{McCabe:1976:CM:800253.807712,
		author = {McCabe, Thomas J.},
		title = {A Complexity Measure},
		booktitle = {Proceedings of the 2Nd International Conference on Software Engineering},
		series = {ICSE '76},
		year = {1976},
		location = {San Francisco, California, USA},
		pages = {407--},
		url = {http://dl.acm.org/citation.cfm?id=800253.807712},
		acmid = {807712},
		publisher = {IEEE Computer Society Press},
		address = {Los Alamitos, CA, USA},
		keywords = {Basis, Complexity measure, Control flow, Decomposition, Graph theory, Independence, Linear, Modularization, Programming, Reduction, Software, Testing},
	} 


Defining new metrics
--------------------

New metrics can be implemented by defining an object that imports the
`code_metric` category and implements its score predicates. There is
also a `code_metrics_utilities` category that defines useful predicates
for the definition of metrics.


Third-party tools
-----------------

`cloc` is an open-source command-line program that counts blank lines,
comment lines, and lines of source code in many programming languages
including Logtalk. Available at https://github.com/AlDanial/cloc

`ohcount` is an open-source command-line program that counts blank lines,
comment lines, and lines of source code in many programming languages
including Logtalk. Available at https://github.com/blackducksoftware/ohcount

`tokei` is an open-source command-line program that counts blank lines,
comment lines, and lines of source code in many programming languages
including Logtalk. Available at https://github.com/Aaronepower/tokei
