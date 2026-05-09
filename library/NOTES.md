________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


Overview
========

This folder contains libraries of useful objects, categories, and protocols.
The currently available libraries can be grouped by scope as follows (the
grouping is thematic only, as some libraries naturally span multiple areas):

- Application metadata, configuration., and logging:
	`application`, `expand_library_alias_paths`, `command_line_options`, `logging`, and `options`.
- Types, collections, combinatorics, and generic data processing:
	`basic_types`, `types`, `assignvars`, `combinations`, `deques`, `dictionaries`,
	`nested_dictionaries`, `graphs`, `heaps`, `hierarchies`, `intervals`,
	`permutations`, `queues`, `sets`, `subsequences`, `union_find`, and `zippers`.
- Meta-programming:
    `meta` and `meta_compiler`
- Monoid implementations:
	`expecteds`, `optionals`, and `validations`.
- Control, state, and developer support:
	`dependents`, `edcg`, `events`,	`listing`, `reader`, and `term_io`.
- Term- and goal-expansion:
	`hook_flows` and `hook_objects`.
- Portability:
	`coroutining`, `dif`, `format`, `process`, `recorded_database`, and `timeout`.
- Dates and time:
    `dates`, `dates_tz`, `time_scales`, and `tzif`.
- Geospatial data:
	`geo_json` and `geospatial`
- Space communications and telemetry:
	`ccsds_frames`, `ccsds_link_profiles`, `ccsds_packet_services`, `ccsds_packetization`,
	`ccsds_packets`, `ccsds_tc_services`, `ccsds_time_codes`, `ccsds_time_fields`,
- Text and NLP:
	`character_sets`, `grammars`, `stemming`, `string_distance`, and `strings`.
- Web:
	`html`, `mime_types`, and `url`.
- Identifiers:
	`cuid2`, `genint`, `gensym`, `ids`, `ksuid`, `nanoid`, `snowflakeid`,
	`ulid`, and `uuid`.
- Interchange formats and wire protocols:
	`amqp`, `avro`, `base32`, `base58`, `base64`, `base85`, `cbor`,
	`csv`, `json`, `json_ld`, `json_lines`, `json_rpc`, `json_schema`,
	`message_pack`, `mcp_server`, `protobuf`, `stomp`, `toml`, `toon`,
	`tsv`, and `yaml`.
- Coordination and data stores:
	`linda`, `memcached`, and `redis`.
- System and external integration:
	`git`, `java`, `os`, and `sockets`.
- Logic and symbolic computing:
	`datalog`.
- Security and integrity:
	`hashes` and `hmac`.
- Randomness:
	`arbitrary`, `mutations`, and `random`.
- Mathematics, statistics, and optimization:
	`linear_algebra`, `simulated_annealing`, and `statistics`.
- Machine learning:
	- Classification:
		`classification_protocols`, `adaptive_boosting_classifier`, `c45_classifier`, `gradient_boosting_classifier`,
        `kernel_svm_classifier`, `knn_classifier`, `lda_classifier`, `linear_svm_classifier`,
		`logistic_regression_classifier`, `naive_bayes_classifier`, `nearest_centroid_classifier`, `qda_classifier`,
		`random_forest_classifier`, and `sgd_classifier`.
	- Anomaly detection:
		`anomaly_detection_protocols`, `cusum_anomaly_detector`, `ewma_anomaly_detector`, `iqr_anomaly_detector`, `isolation_forest_anomaly_detector`,
		`knn_distance_anomaly_detector`, `lof_anomaly_detector`, `modified_z_score_anomaly_detector`, and `z_score_anomaly_detector`.
	- Regression:
		`regression_protocols`, `bayesian_ridge_regression`, `elastic_net_regression`,
        `gradient_boosting_regression`, `gaussian_process_regression`, `knn_regression`,
        `lasso_regression`, `linear_regression`, `random_forest_regression`,
        `regression_tree`, and `ridge_regression`.
	- Ranking:
		`ranking_protocols`, `borda_ranker`, `bradley_terry_ranker`, `colley_ranker`, `copeland_ranker`,
	    `elo_ranker`, `glicko2_ranker`, `glicko2_periodic_ranker`, `hodge_rank`, `kemeny_young_ranker`,
        `massey_ranker`, `plackett_luce_ranker`, `plackett_luce_last_ranker`, `rank_centrality`,
        `ranked_pairs`, `regularized_bradley_terry_ranker`, `schulze_ranker`, and
        `thurstone_mosteller_ranker`.
	- Clustering:
		`clustering_protocols`, `agglomerative_clusterer`, `dbscan_clusterer`,
		`gaussian_mixture_clusterer`, `hdbscan_clusterer`, `hierarchical_clustering`,
		`kcenters_clusterer`, `kmeans_clusterer`, `kmedians_clusterer`, `kmedoids_clusterer`, `kmodes_clusterer`,
		`kprototypes_clusterer`, and `optics_clusterer`.
	- Dimension reduction:
		`dimension_reduction_protocols`, `ica_projection`, `kernel_pca_projection`, `lda_projection`,
	    `nmf_projection`, `pca_projection`, `pls_projection`, `probabilistic_pca_projection`, and
	    `random_projection`.
	- Pattern mining:
		`pattern_mining_protocols`, `frequent_pattern_mining_protocols`,
		`sequential_pattern_mining_protocols`, `apriori_pattern_miner`, `clo_span_pattern_miner`,
		`eclat_pattern_miner`, `fp_growth_pattern_miner`, `gsp_pattern_miner`, `prefix_span_pattern_miner`, and `spade_pattern_miner`.

In addition to the loader-based libraries, this directory also contains a
small number of standalone reusable entities, namely `attributes`, `cloning`,
`counters`, and `streamvars`.

Specific notes about individual libraries can be found in the corresponding
library directory `NOTES.md` files.

A plain Prolog version of the Unicode 6.2 standard is also included in the
`unicode_data` folder. See its `README.md` file for details.

A `parallel_logtalk_processes_setup.pl` Prolog file is also provided with
sample code for selected backend Prolog compilers for initializing Logtalk
processes such that each process uses a unique scratch directory, therefore
allowing parallel process execution (e.g., for usage at continuous integration
servers). Starting with Logtalk 3.48.0, this setup is only required in general
when running with the `clean` flag turned off. See the comments in the file
itself for usage instructions.


Library documentation
---------------------

Specific notes about each library can be found in the corresponding `NOTES.md`
files. HTML documentation for each library API can be found on the `docs`
directory (open the `../docs/handbook/index.html` file with your web browser).


Loading libraries
-----------------

All the individual libraries can be loaded using the `<library name>(loader)`
notation as argument for the compiling and loading predicates. For example:

	| ?- logtalk_load(random(loader)).

There is also a file named `all_loader.lgt` that will load all libraries.
Simply type the goal:

	| ?- logtalk_load(library(all_loader)).

As a general rule, always use the corresponding loader file to load a
library. Most library entities are part of small hierarchies or depend on
other libraries and thus cannot be loaded and compiled separately (e.g., the
`list` object implements the `listp` protocol and is part of a basic types
hierarchy). Using the loader files takes care of all dependencies and also
ensures compilation in optimized mode.


Testing libraries
-----------------

Most of the libraries include unit tests in their directory, together with
a `tester.lgt` file for running them. For example, to run the tests for the
`random` library, we can use the goal:

	| ?- logtalk_load(random(tester)).

To run all library tests, we can use the `logtalk_tester` automation script
from the `library` directory at the root of the Logtalk distribution. For
example, assuming the Logtalk user directory is `~/logtalk` and that we want
to run the tests using ECLiPSe as the backend Prolog compiler:

	$ cd ~/logtalk/library
	$ logtalk_tester -p eclipse


Credits
-------

Some code in this library is based on public domain Prolog code, in particular,
code adopted from the Edinburgh Prolog library. The definition of the predicate
`reverse/2` in the `list` object is from Richard O'Keefe and can be found in its
book "The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library proposal
available at:

http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

Some libraries, or parts of libraries, are either ports of Prolog system
libraries or inspired by Prolog system libraries. See the individual library
notes for details. See also the `NOTICE.txt` file at the root of the Logtalk
distribution for copyright information on third-party source code.


Other notes
-----------

Some files contained in this directory represent work in progress and are
not loaded by default by any loader utility file.
