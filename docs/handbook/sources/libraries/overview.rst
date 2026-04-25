Overview
========

This folder contains libraries of useful objects, categories, and
protocols. The currently available libraries can be grouped by scope as
follows (the grouping is thematic only, as some libraries naturally span
multiple areas):

- Application metadata, configuration., and logging: ``application``,
  ``expand_library_alias_paths``, ``command_line_options``, ``logging``,
  and ``options``.
- Types, collections, combinatorics, and generic data processing:
  ``basic_types``, ``types``, ``assignvars``, ``combinations``,
  ``deques``, ``dictionaries``, ``nested_dictionaries``, ``graphs``,
  ``heaps``, ``hierarchies``, ``intervals``, ``permutations``,
  ``queues``, ``sets``, ``subsequences``, ``union_find``, and
  ``zippers``.
- Meta-programming: ``meta`` and ``meta_compiler``
- Monoid implementations: ``expecteds``, ``optionals``, and
  ``validations``.
- Control, state, and developer support: ``dependents``, ``edcg``,
  ``events``, ``listing``, ``reader``, and ``term_io``.
- Term- and goal-expansion: ``hook_flows`` and ``hook_objects``.
- Portability: ``coroutining``, ``dif``, ``format``, ``process``,
  ``recorded_database``, and ``timeout``.
- Dates, time, and geospatial data: ``ccsds``, ``dates``, ``dates_tz``,
  ``geospatial``, ``time_scales``, and ``tzif``.
- Text and NLP: ``character_sets``, ``grammars``, ``stemming``,
  ``string_distance``, and ``strings``.
- Web: ``html``, ``mime_types``, and ``url``.
- Identifiers: ``cuid2``, ``genint``, ``gensym``, ``ids``, ``ksuid``,
  ``nanoid``, ``snowflakeid``, ``ulid``, and ``uuid``.
- Interchange formats and wire protocols: ``amqp``, ``avro``,
  ``base32``, ``base58``, ``base64``, ``base85``, ``cbor``, ``csv``,
  ``json``, ``json_ld``, ``json_lines``, ``json_rpc``, ``json_schema``,
  ``mcp_server``, ``protobuf``, ``stomp``, ``toml``, ``toon``, ``tsv``,
  and ``yaml``.
- Coordination and data stores: ``linda``, ``memcached``, and ``redis``.
- System and external integration: ``git``, ``java``, ``os``, and
  ``sockets``.
- Logic and symbolic computing: ``datalog``.
- Security and integrity: ``hashes`` and ``hmac``.
- Randomness: ``arbitrary``, ``mutations``, and ``random``.
- Mathematics, statistics, and optimization: ``simulated_annealing`` and
  ``statistics``.
- Machine learning:

  - Classification: ``classification_protocols``, ``ada_boost``,
    ``c45``, ``knn``, ``linear_svm``, ``logistic_regression``,
    ``naive_bayes``, ``nearest_centroid``, and ``random_forest``.
  - Anomaly detection: ``anomaly_detection_protocols``,
    ``isolation_forest``, ``knn_distance``, and ``lof``.
  - Regression: ``regression_protocols``,
    ``gradient_boosting_regression``, ``knn_regression``,
    ``linear_regression``, ``random_forest_regression``, and
    ``regression_tree``.
  - Ranking: ``ranking_protocols``, ``borda``, ``bradley_terry``,
    ``copeland``, and ``rank_centrality``.
  - Clustering: ``clustering_protocols``, ``agglomerative``, ``dbscan``,
    ``gaussian_mixture``, ``hdbscan``, ``hierarchical_clustering``,
    ``kcenters``, ``kmeans``, ``kmedians``, ``kmedoids``, ``kmodes``,
    ``kprototypes``, and ``optics``.
  - Dimension reduction: ``dimension_reduction_protocols``,
    ``lda_projection``, ``pca``, and ``random_projection``.
  - Pattern mining: ``pattern_mining_protocols``,
    ``frequent_pattern_mining_protocols``,
    ``sequential_pattern_mining_protocols``, ``apriori``, ``clo_span``,
    ``eclat``, ``fp_growth``, ``gsp``, ``prefix_span``, and ``spade``.

In addition to the loader-based libraries, this directory also contains
a small number of standalone reusable entities, namely ``attributes``,
``cloning``, ``counters``, and ``streamvars``.

Specific notes about individual libraries can be found in the
corresponding library directory ``NOTES.md`` files.

A plain Prolog version of the Unicode 6.2 standard is also included in
the ``unicode_data`` folder. See its ``README.md`` file for details.

A ``parallel_logtalk_processes_setup.pl`` Prolog file is also provided
with sample code for selected backend Prolog compilers for initializing
Logtalk processes such that each process uses a unique scratch
directory, therefore allowing parallel process execution (e.g., for
usage at continuous integration servers). Starting with Logtalk 3.48.0,
this setup is only required in general when running with the ``clean``
flag turned off. See the comments in the file itself for usage
instructions.

Library documentation
---------------------

Specific notes about each library can be found in the corresponding
``NOTES.md`` files. HTML documentation for each library API can be found
on the ``docs`` directory (open the ``../docs/handbook/index.html`` file
with your web browser).

Loading libraries
-----------------

All the individual libraries can be loaded using the
``<library name>(loader)`` notation as argument for the compiling and
loading predicates. For example:

::

   | ?- logtalk_load(random(loader)).

There is also a file named ``all_loader.lgt`` that will load all
libraries. Simply type the goal:

::

   | ?- logtalk_load(library(all_loader)).

As a general rule, always use the corresponding loader file to load a
library. Most library entities are part of small hierarchies or depend
on other libraries and thus cannot be loaded and compiled separately
(e.g., the ``list`` object implements the ``listp`` protocol and is part
of a basic types hierarchy). Using the loader files takes care of all
dependencies and also ensures compilation in optimized mode.

Testing libraries
-----------------

Most of the libraries include unit tests in their directory, together
with a ``tester.lgt`` file for running them. For example, to run the
tests for the ``random`` library, we can use the goal:

::

   | ?- logtalk_load(random(tester)).

To run all library tests, we can use the ``logtalk_tester`` automation
script from the ``library`` directory at the root of the Logtalk
distribution. For example, assuming the Logtalk user directory is
``~/logtalk`` and that we want to run the tests using ECLiPSe as the
backend Prolog compiler:

::

   $ cd ~/logtalk/library
   $ logtalk_tester -p eclipse

Credits
-------

Some code in this library is based on public domain Prolog code, in
particular, code adopted from the Edinburgh Prolog library. The
definition of the predicate ``reverse/2`` in the ``list`` object is from
Richard O'Keefe and can be found in its book "The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library
proposal available at:

http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

Some libraries, or parts of libraries, are either ports of Prolog system
libraries or inspired by Prolog system libraries. See the individual
library notes for details. See also the ``NOTICE.txt`` file at the root
of the Logtalk distribution for copyright information on third-party
source code.

Other notes
-----------

Some files contained in this directory represent work in progress and
are not loaded by default by any loader utility file.
