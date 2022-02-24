``code_metrics``
================

The purpose of this tool is to assess qualities of source code that may
predict negative aspects such as entity coupling, cohesion, complexity,
error-proneness, and overall maintainability. It is meant to be
extensible via the addition of objects implementing new metrics.

This tool provides predicates for computing metrics for source files,
entities, libraries, files, and directories. The actual availability of
a particular predicate depends on the specific metric. One set of
predicates prints, by default, the computed metric values to the
standard output. A second set of predicates computes and returns a score
(usually a compound term with the computed metric values as arguments).

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#code-metrics <../../docs/library_index.html#code-metrics>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(code_metrics(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(code_metrics(tester)).

Available metrics
-----------------

Currently, the following metrics are provided:

-  Number of Clauses (``noc_metric``)
-  Number of Rules (``nor_metric``)
-  Unique Predicate Nodes (``upn_metric``)
-  Cyclomatic Complexity (``cc_metric``)
-  Depth of Inheritance (``dit_metric``)
-  Efferent coupling, afferent coupling, instability, and abstractness
   (``coupling_metric``)
-  Documentation (``doc_metric``)
-  Source code size (``size_metric``)
-  Halstead complexity (``halstead_metric`` and
   ``halstead_metric(Stroud)``)

A helper object, ``code_metrics``, is also provided allowing running all
loaded individual metrics. For code coverage metrics, see the
``lgtunit`` tool documentation.

For interpretation of the coupling metric scores, see e.g. the original
paper by Robert Martin, "OO Design Quality Metrics":

::

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

::

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

The cyclomatic complexity metric uses the same predicate abstraction as
the UPN metric and it is also described in the above paper besides the
original paper by Thomas J. McCabe:

::

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

Be sure to fully understand the metrics individual meanings and any
implementation limitations before using them to support any evaluation
or decision process.

Usage
-----

All metrics require the source code to be analyzed to be loaded with the
``source_data`` flag turned on. For usage examples, see the
``SCRIPT.txt`` file in the tool directory.

Defining new metrics
--------------------

New metrics can be implemented by defining an object that imports the
``code_metric`` category and implements its score predicates. There is
also a ``code_metrics_utilities`` category that defines useful
predicates for the definition of metrics.

Third-party tools
-----------------

``cloc`` is an open-source command-line program that counts blank lines,
comment lines, and lines of source code in many programming languages
including Logtalk. Available at https://github.com/AlDanial/cloc

``ohcount`` is an open-source command-line program that counts blank
lines, comment lines, and lines of source code in many programming
languages including Logtalk. Available at
https://github.com/blackducksoftware/ohcount

``tokei`` is an open-source command-line program that counts blank
lines, comment lines, and lines of source code in many programming
languages including Logtalk. Available at
https://github.com/Aaronepower/tokei

Applying metrics to Prolog modules
----------------------------------

Some of the metrics can also be applied to Prolog modules that Logtalk
is able to compile as objects. For example, if the Prolog module file is
named ``module.pl``, try:

::

   | ?- logtalk_load(module, [source_data(on)]).

Due to the lack of standardization of module systems and the abundance
of proprietary extensions, this solution is not expected to work for all
cases.

Applying metrics to plain Prolog code
-------------------------------------

Some of the metrics can also be applied to plain Prolog code. For
example, if the Prolog file is named ``code.pl``, simply define an
object including its code:

::

   :- object(code).
       :- include('code.pl').
   :- end_object.

Save the object to an e.g. ``code.lgt`` file in the same directory as
the Prolog file and then load it in debug mode:

::

   | ?- logtalk_load(code, [source_data(on)]).

In alternative, use the ``object_wrapper_hook`` provided by the
``hook_objects`` library:

::

   | ?- logtalk_load([os(loader), hook_objects(object_wrapper_hook)]).
   ...

   | ?- logtalk_load(code, [hook(object_wrapper_hook), source_data(on)]).

With either wrapping solution, pay special attention to any compilation
warnings that may signal issues that could prevent the plain Prolog code
of working when wrapped by an object.
