%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths
%  Last updated on May 9, 2026
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% logtalk_library_path(Library, Path)
%
% paths must always end with a "/"

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

% Logtalk startup directory
:- initialization((
	(	'$lgt_environment_variable'('LOGTALK_STARTUP_DIRECTORY', _) ->
		LOGTALK_STARTUP_DIRECTORY = '$LOGTALK_STARTUP_DIRECTORY/'
	;	'$lgt_current_directory'(LOGTALK_STARTUP_DIRECTORY0),
		(	sub_atom(LOGTALK_STARTUP_DIRECTORY0, _, _, 0, '/') ->
			LOGTALK_STARTUP_DIRECTORY = LOGTALK_STARTUP_DIRECTORY0
		;	atom_concat(LOGTALK_STARTUP_DIRECTORY0, '/', LOGTALK_STARTUP_DIRECTORY)
		)
	),
	asserta(logtalk_library_path(startup, LOGTALK_STARTUP_DIRECTORY))
)).

% Logtalk installation directory
logtalk_library_path(logtalk_home, '$LOGTALKHOME/').

% Logtalk user directory
logtalk_library_path(logtalk_user, '$LOGTALKUSER/').

% user home directory
logtalk_library_path(home, HOME) :-
	(	'$lgt_environment_variable'('HOME', _) ->
		% likely a POSIX system but Windows users
		% may also define this environment variable
		HOME = '$HOME/'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		% Windows systems define this environment variable
		HOME = '$USERPROFILE/'
	;	fail
	).

% core library, required for Logtalk startup
logtalk_library_path(core, logtalk_home('core/')).

% main directories in the Logtalk distribution
logtalk_library_path(coding, logtalk_user('coding/')).
logtalk_library_path(contributions, logtalk_user('contributions/')).
logtalk_library_path(examples, logtalk_user('examples/')).
logtalk_library_path(library, logtalk_user('library/')).
logtalk_library_path(ports, logtalk_user('ports/')).
logtalk_library_path(tools, logtalk_user('tools/')).
logtalk_library_path(tests, logtalk_user('tests/')).

% third-party contributions
logtalk_library_path(flags, contributions('flags/')).
logtalk_library_path(iso8601, contributions('iso8601/')).
logtalk_library_path(pddl_parser, contributions('pddl_parser/')).
logtalk_library_path(verdi_neruda, contributions('verdi_neruda/')).
logtalk_library_path(xml_parser, contributions('xml_parser/')).

% ports of third-party libraries and applications
logtalk_library_path(fcube, ports('fcube/')).
logtalk_library_path(metagol, ports('metagol/')).
logtalk_library_path(toychr, ports('toychr/')).

% developer tools
logtalk_library_path(assertions, tools('assertions/')).
logtalk_library_path(code_metrics, tools('code_metrics/')).
logtalk_library_path(dead_code_scanner, tools('dead_code_scanner/')).
logtalk_library_path(debug_messages, tools('debug_messages/')).
logtalk_library_path(debugger, tools('debugger/')).
logtalk_library_path(diagrams, tools('diagrams/')).
logtalk_library_path(doclet, tools('doclet/')).
logtalk_library_path(help, tools('help/')).
logtalk_library_path(issue_creator, tools('issue_creator/')).
logtalk_library_path(lgtdoc, tools('lgtdoc/')).
logtalk_library_path(lgtunit, tools('lgtunit/')).
logtalk_library_path(linter_reporter, tools('linter_reporter/')).
logtalk_library_path(mutation_testing, tools('mutation_testing/')).
logtalk_library_path(packs, tools('packs/')).
logtalk_library_path(ports_profiler, tools('ports_profiler/')).
logtalk_library_path(profiler, tools('profiler/')).
logtalk_library_path(sarif, tools('sarif/')).
logtalk_library_path(sbom, tools('sbom/')).
logtalk_library_path(tool_diagnostics, tools('tool_diagnostics/')).
logtalk_library_path(tutor, tools('tutor/')).
logtalk_library_path(wrapper, tools('wrapper/')).

% libraries
logtalk_library_path(adaptive_boosting_classifier, library('adaptive_boosting_classifier/')).
logtalk_library_path(agglomerative_clusterer, library('agglomerative_clusterer/')).
logtalk_library_path(amqp, library('amqp/')).
logtalk_library_path(anomaly_detection_protocols, library('anomaly_detection_protocols/')).
logtalk_library_path(application, library('application/')).
logtalk_library_path(apriori_pattern_miner, library('apriori_pattern_miner/')).
logtalk_library_path(arbitrary, library('arbitrary/')).
logtalk_library_path(assignvars, library('assignvars/')).
logtalk_library_path(avro, library('avro/')).
logtalk_library_path(base32, library('base32/')).
logtalk_library_path(base58, library('base58/')).
logtalk_library_path(base64, library('base64/')).
logtalk_library_path(base85, library('base85/')).
logtalk_library_path(basic_types, library('basic_types/')).
logtalk_library_path(bayesian_ridge_regression, library('bayesian_ridge_regression/')).
logtalk_library_path(borda_ranker, library('borda_ranker/')).
logtalk_library_path(bradley_terry_ranker, library('bradley_terry_ranker/')).
logtalk_library_path(c45_classifier, library('c45_classifier/')).
logtalk_library_path(cbor, library('cbor/')).
logtalk_library_path(character_sets, library('character_sets/')).
logtalk_library_path(classification_protocols, library('classification_protocols/')).
logtalk_library_path(clo_span_pattern_miner, library('clo_span_pattern_miner/')).
logtalk_library_path(clustering_protocols, library('clustering_protocols/')).
logtalk_library_path(combinations, library('combinations/')).
logtalk_library_path(command_line_options, library('command_line_options/')).
logtalk_library_path(colley_ranker, library('colley_ranker/')).
logtalk_library_path(copeland_ranker, library('copeland_ranker/')).
logtalk_library_path(coroutining, library('coroutining/')).
logtalk_library_path(ccsds_frames, library('ccsds_frames/')).
logtalk_library_path(ccsds_link_profiles, library('ccsds_link_profiles/')).
logtalk_library_path(ccsds_packet_services, library('ccsds_packet_services/')).
logtalk_library_path(ccsds_packetization, library('ccsds_packetization/')).
logtalk_library_path(ccsds_packets, library('ccsds_packets/')).
logtalk_library_path(ccsds_tc_services, library('ccsds_tc_services/')).
logtalk_library_path(ccsds_time_codes, library('ccsds_time_codes/')).
logtalk_library_path(ccsds_time_fields, library('ccsds_time_fields/')).
logtalk_library_path(csv, library('csv/')).
logtalk_library_path(cuid2, library('cuid2/')).
logtalk_library_path(cusum_anomaly_detector, library('cusum_anomaly_detector/')).
logtalk_library_path(datalog, library('datalog/')).
logtalk_library_path(dates, library('dates/')).
logtalk_library_path(dates_tz, library('dates_tz/')).
logtalk_library_path(dbscan_clusterer, library('dbscan_clusterer/')).
logtalk_library_path(dependents, library('dependents/')).
logtalk_library_path(deques, library('deques/')).
logtalk_library_path(dictionaries, library('dictionaries/')).
logtalk_library_path(dif, library('dif/')).
logtalk_library_path(dimension_reduction_protocols, library('dimension_reduction_protocols/')).
logtalk_library_path(eclat_pattern_miner, library('eclat_pattern_miner/')).
logtalk_library_path(edcg, library('edcg/')).
logtalk_library_path(elastic_net_regression, library('elastic_net_regression/')).
logtalk_library_path(elo_ranker, library('elo_ranker/')).
logtalk_library_path(events, library('events/')).
logtalk_library_path(ewma_anomaly_detector, library('ewma_anomaly_detector/')).
logtalk_library_path(expand_library_alias_paths, library('expand_library_alias_paths/')).
logtalk_library_path(expecteds, library('expecteds/')).
logtalk_library_path(format, library('format/')).
logtalk_library_path(fp_growth_pattern_miner, library('fp_growth_pattern_miner/')).
logtalk_library_path(frequent_pattern_mining_protocols, library('frequent_pattern_mining_protocols/')).
logtalk_library_path(gaussian_mixture_clusterer, library('gaussian_mixture_clusterer/')).
logtalk_library_path(gaussian_process_regression, library('gaussian_process_regression/')).
logtalk_library_path(genint, library('genint/')).
logtalk_library_path(gensym, library('gensym/')).
logtalk_library_path(geo_json, library('geo_json/')).
logtalk_library_path(geospatial, library('geospatial/')).
logtalk_library_path(git, library('git/')).
logtalk_library_path(glicko2_ranker, library('glicko2_ranker/')).
logtalk_library_path(glicko2_periodic_ranker, library('glicko2_periodic_ranker/')).
logtalk_library_path(gradient_boosting_classifier, library('gradient_boosting_classifier/')).
logtalk_library_path(gradient_boosting_regression, library('gradient_boosting_regression/')).
logtalk_library_path(grammars, library('grammars/')).
logtalk_library_path(graphs, library('graphs/')).
logtalk_library_path(gsp_pattern_miner, library('gsp_pattern_miner/')).
logtalk_library_path(hashes, library('hashes/')).
logtalk_library_path(hdbscan_clusterer, library('hdbscan_clusterer/')).
logtalk_library_path(heaps, library('heaps/')).
logtalk_library_path(hierarchical_clustering, library('hierarchical_clustering/')).
logtalk_library_path(hierarchies, library('hierarchies/')).
logtalk_library_path(hmac, library('hmac/')).
logtalk_library_path(hodge_rank, library('hodge_rank/')).
logtalk_library_path(hook_flows, library('hook_flows/')).
logtalk_library_path(hook_objects, library('hook_objects/')).
logtalk_library_path(html, library('html/')).
logtalk_library_path(ica_projection, library('ica_projection/')).
logtalk_library_path(ids, library('ids/')).
logtalk_library_path(ieee_754, library('ieee_754/')).
logtalk_library_path(intervals, library('intervals/')).
logtalk_library_path(iqr_anomaly_detector, library('iqr_anomaly_detector/')).
logtalk_library_path(iso_639, library('iso_639/')).
logtalk_library_path(iso_3166, library('iso_3166/')).
logtalk_library_path(iso_4217, library('iso_4217/')).
logtalk_library_path(iso_9362, library('iso_9362/')).
logtalk_library_path(iso_13616, library('iso_13616/')).
logtalk_library_path(isolation_forest_anomaly_detector, library('isolation_forest_anomaly_detector/')).
logtalk_library_path(java, library('java/')).
logtalk_library_path(json, library('json/')).
logtalk_library_path(json_ld, library('json_ld/')).
logtalk_library_path(json_lines, library('json_lines/')).
logtalk_library_path(json_rpc, library('json_rpc/')).
logtalk_library_path(json_schema, library('json_schema/')).
logtalk_library_path(kcenters_clusterer, library('kcenters_clusterer/')).
logtalk_library_path(kemeny_young_ranker, library('kemeny_young_ranker/')).
logtalk_library_path(kernel_svm_classifier, library('kernel_svm_classifier/')).
logtalk_library_path(kernel_pca_projection, library('kernel_pca_projection/')).
logtalk_library_path(kmeans_clusterer, library('kmeans_clusterer/')).
logtalk_library_path(kmedians_clusterer, library('kmedians_clusterer/')).
logtalk_library_path(kmedoids_clusterer, library('kmedoids_clusterer/')).
logtalk_library_path(kmodes_clusterer, library('kmodes_clusterer/')).
logtalk_library_path(knn_classifier, library('knn_classifier/')).
logtalk_library_path(knn_distance_anomaly_detector, library('knn_distance_anomaly_detector/')).
logtalk_library_path(knn_regression, library('knn_regression/')).
logtalk_library_path(kprototypes_clusterer, library('kprototypes_clusterer/')).
logtalk_library_path(ksuid, library('ksuid/')).
logtalk_library_path(lasso_regression, library('lasso_regression/')).
logtalk_library_path(lda_classifier, library('lda_classifier/')).
logtalk_library_path(lda_projection, library('lda_projection/')).
logtalk_library_path(linda, library('linda/')).
logtalk_library_path(linear_algebra, library('linear_algebra/')).
logtalk_library_path(linear_regression, library('linear_regression/')).
logtalk_library_path(linear_svm_classifier, library('linear_svm_classifier/')).
logtalk_library_path(listing, library('listing/')).
logtalk_library_path(lof_anomaly_detector, library('lof_anomaly_detector/')).
logtalk_library_path(logging, library('logging/')).
logtalk_library_path(logistic_regression_classifier, library('logistic_regression_classifier/')).
logtalk_library_path(loops, library('loops/')).
logtalk_library_path(massey_ranker, library('massey_ranker/')).
logtalk_library_path(mcp_server, library('mcp_server/')).
logtalk_library_path(memcached, library('memcached/')).
logtalk_library_path(message_pack, library('message_pack/')).
logtalk_library_path(meta, library('meta/')).
logtalk_library_path(meta_compiler, library('meta_compiler/')).
logtalk_library_path(mime_types, library('mime_types/')).
logtalk_library_path(modified_z_score_anomaly_detector, library('modified_z_score_anomaly_detector/')).
logtalk_library_path(mutations, library('mutations/')).
logtalk_library_path(naive_bayes_classifier, library('naive_bayes_classifier/')).
logtalk_library_path(nanoid, library('nanoid/')).
logtalk_library_path(nearest_centroid_classifier, library('nearest_centroid_classifier/')).
logtalk_library_path(nested_dictionaries, library('nested_dictionaries/')).
logtalk_library_path(nmf_projection, library('nmf_projection/')).
logtalk_library_path(optics_clusterer, library('optics_clusterer/')).
logtalk_library_path(optionals, library('optionals/')).
logtalk_library_path(options, library('options/')).
logtalk_library_path(os, library('os/')).
logtalk_library_path(pattern_mining_protocols, library('pattern_mining_protocols/')).
logtalk_library_path(pca_projection, library('pca_projection/')).
logtalk_library_path(pls_projection, library('pls_projection/')).
logtalk_library_path(permutations, library('permutations/')).
logtalk_library_path(plackett_luce_ranker, library('plackett_luce_ranker/')).
logtalk_library_path(plackett_luce_last_ranker, library('plackett_luce_last_ranker/')).
logtalk_library_path(prefix_span_pattern_miner, library('prefix_span_pattern_miner/')).
logtalk_library_path(probabilistic_pca_projection, library('probabilistic_pca_projection/')).
logtalk_library_path(process, library('process/')).
logtalk_library_path(protobuf, library('protobuf/')).
logtalk_library_path(qda_classifier, library('qda_classifier/')).
logtalk_library_path(queues, library('queues/')).
logtalk_library_path(random, library('random/')).
logtalk_library_path(random_forest_classifier, library('random_forest_classifier/')).
logtalk_library_path(random_forest_regression, library('random_forest_regression/')).
logtalk_library_path(random_projection, library('random_projection/')).
logtalk_library_path(rank_centrality, library('rank_centrality/')).
logtalk_library_path(ranked_pairs, library('ranked_pairs/')).
logtalk_library_path(ranking_protocols, library('ranking_protocols/')).
logtalk_library_path(reader, library('reader/')).
logtalk_library_path(recorded_database, library('recorded_database/')).
logtalk_library_path(redis, library('redis/')).
logtalk_library_path(regression_protocols, library('regression_protocols/')).
logtalk_library_path(regression_tree, library('regression_tree/')).
logtalk_library_path(regularized_bradley_terry_ranker, library('regularized_bradley_terry_ranker/')).
logtalk_library_path(ridge_regression, library('ridge_regression/')).
logtalk_library_path(schulze_ranker, library('schulze_ranker/')).
logtalk_library_path(sequential_pattern_mining_protocols, library('sequential_pattern_mining_protocols/')).
logtalk_library_path(sets, library('sets/')).
logtalk_library_path(sgd_classifier, library('sgd_classifier/')).
logtalk_library_path(simulated_annealing, library('simulated_annealing/')).
logtalk_library_path(snowflakeid, library('snowflakeid/')).
logtalk_library_path(sockets, library('sockets/')).
logtalk_library_path(spade_pattern_miner, library('spade_pattern_miner/')).
logtalk_library_path(statistics, library('statistics/')).
logtalk_library_path(stemming, library('stemming/')).
logtalk_library_path(stomp, library('stomp/')).
logtalk_library_path(string_distance, library('string_distance/')).
logtalk_library_path(strings, library('strings/')).
logtalk_library_path(subsequences, library('subsequences/')).
logtalk_library_path(term_io, library('term_io/')).
logtalk_library_path(thurstone_mosteller_ranker, library('thurstone_mosteller_ranker/')).
logtalk_library_path(time_scales, library('time_scales/')).
logtalk_library_path(timeout, library('timeout/')).
logtalk_library_path(toml, library('toml/')).
logtalk_library_path(toon, library('toon/')).
logtalk_library_path(truncated_svd_projection, library('truncated_svd_projection/')).
logtalk_library_path(tsv, library('tsv/')).
logtalk_library_path(types, library('types/')).
logtalk_library_path(tzif, library('tzif/')).
logtalk_library_path(ulid, library('ulid/')).
logtalk_library_path(union_find, library('union_find/')).
logtalk_library_path(url, library('url/')).
logtalk_library_path(uuid, library('uuid/')).
logtalk_library_path(validations, library('validations/')).
logtalk_library_path(wkt_wkb, library('wkt_wkb/')).
logtalk_library_path(yaml, library('yaml/')).
logtalk_library_path(z_score_anomaly_detector, library('z_score_anomaly_detector/')).
logtalk_library_path(zippers, library('zippers/')).

% programming examples
logtalk_library_path(ack, examples('ack/')).
logtalk_library_path(activation_nodes, ncl('activation_nodes/')).
logtalk_library_path(adventure, examples('adventure/')).
logtalk_library_path(aliases, examples('aliases/')).
logtalk_library_path(apache_poi, examples('apache_poi/')).
logtalk_library_path(around_methods, examples('around_methods/')).
logtalk_library_path(aspects, examples('aspects/')).
logtalk_library_path(assign_parameters, examples('assign_parameters/')).
logtalk_library_path(assumptions, examples('assumptions/')).
logtalk_library_path(attvars, examples('attvars/')).
logtalk_library_path(backtracking_nodes, ncl('backtracking_nodes/')).
logtalk_library_path(barriers, threads('barriers/')).
logtalk_library_path(bench, examples('bench/')).
logtalk_library_path(benchmarks, examples('benchmarks/')).
logtalk_library_path(birds, examples('birds/')).
logtalk_library_path(birds_mcp, examples('birds_mcp/')).
logtalk_library_path(birthdays, threads('birthdays/')).
logtalk_library_path(blackboard, threads('blackboard/')).
logtalk_library_path(blocks, examples('blocks/')).
logtalk_library_path(books, examples('books/')).
logtalk_library_path(bottles, examples('bottles/')).
logtalk_library_path(bricks, examples('bricks/')).
logtalk_library_path(buckets, threads('buckets/')).
logtalk_library_path(buffer, threads('buffer/')).
logtalk_library_path(carengines, examples('carengines/')).
logtalk_library_path(cascade, examples('cascade/')).
logtalk_library_path(checkpoint, threads('checkpoint/')).
logtalk_library_path(classmethods, examples('classmethods/')).
logtalk_library_path(classvars, examples('classvars/')).
logtalk_library_path(closed_world_assumption, examples('closed_world_assumption/')).
logtalk_library_path(clp_bp, constraints('bp/')).
logtalk_library_path(clp_eclipse, constraints('eclipse/')).
logtalk_library_path(clp_gprolog, constraints('gprolog/')).
logtalk_library_path(clp_sicstus, constraints('sicstus/')).
logtalk_library_path(clp_swipl, constraints('swipl/')).
logtalk_library_path(clp_yap, constraints('yap/')).
logtalk_library_path(clustering, examples('clustering/')).
logtalk_library_path(coinduction, examples('coinduction/')).
logtalk_library_path(complements_allow, examples('complements/allow/')).
logtalk_library_path(complements_restrict, examples('complements/restrict/')).
logtalk_library_path(constraints, examples('constraints/')).
logtalk_library_path(dcgs, examples('dcgs/')).
logtalk_library_path(debug_hooks, examples('debug_hooks/')).
logtalk_library_path(defaulty, examples('defaulty/')).
logtalk_library_path(delegates, examples('delegates/')).
logtalk_library_path(design_patterns, examples('design_patterns/')).
logtalk_library_path(document_converter, examples('document_converter/')).
logtalk_library_path(diamonds, examples('diamonds/')).
logtalk_library_path(dynpred, examples('dynpred/')).
logtalk_library_path(ebench, engines('ebench/')).
logtalk_library_path(eclipse_tests_dsl, examples('eclipse_tests_dsl/')).
logtalk_library_path(edcgs, examples('edcgs/')).
logtalk_library_path(elephants, examples('elephants/')).
logtalk_library_path(emetas, engines('emetas/')).
logtalk_library_path(encodings, examples('encodings/')).
logtalk_library_path(engines, examples('engines/')).
logtalk_library_path(errors, examples('errors/')).
logtalk_library_path(expansion, examples('expansion/')).
logtalk_library_path(family, examples('family/')).
logtalk_library_path(family_alt, examples('family_alt/')).
logtalk_library_path(fft, threads('fft/')).
logtalk_library_path(fibonacci, threads('fibonacci/')).
logtalk_library_path(figures, ncl('figures/')).
logtalk_library_path(fluents, engines('fluents/')).
logtalk_library_path(free_nodes, ncl('free_nodes/')).
logtalk_library_path(functions, threads('functions/')).
logtalk_library_path(futures, examples('futures/')).
logtalk_library_path(hailstone, examples('hailstone/')).
logtalk_library_path(hanoi, threads('hanoi/')).
logtalk_library_path(haunted_wasteland, examples('haunted_wasteland/')).
logtalk_library_path(hello_world, examples('hello_world/')).
logtalk_library_path(hooks, examples('hooks/')).
logtalk_library_path(includes, examples('includes/')).
logtalk_library_path(inheritance, examples('inheritance/')).
logtalk_library_path(inlining, examples('inlining/')).
logtalk_library_path(instmethods, examples('instmethods/')).
logtalk_library_path(instvars, examples('instvars/')).
logtalk_library_path(integration, threads('integration/')).
logtalk_library_path(integration2d, threads('integration2d/')).
logtalk_library_path(interactors, engines('interactors/')).
logtalk_library_path(jpl, examples('jpl/')).
logtalk_library_path(lambdas, examples('lambdas/')).
logtalk_library_path(lambdas_compiled, examples('lambdas_compiled/')).
logtalk_library_path(laptops, examples('laptops/')).
logtalk_library_path(lazy, engines('lazy/')).
logtalk_library_path(lips, examples('lips/')).
logtalk_library_path(list_permutations, examples('list_permutations/')).
logtalk_library_path(lo_planner, examples('lo/planner/')).
logtalk_library_path(lo_travellers, examples('lo/travellers/')).
logtalk_library_path(localizations, examples('localizations/')).
logtalk_library_path(logic, examples('logic/')).
logtalk_library_path(logs, examples('logs/')).
logtalk_library_path(lpa_faults, examples('lpa/faults/')).
logtalk_library_path(lpa_timetables, examples('lpa/timetables/')).
logtalk_library_path(metaclasses, examples('metaclasses/')).
logtalk_library_path(metainterpreters, examples('metainterpreters/')).
logtalk_library_path(metapredicates, examples('metapredicates/')).
logtalk_library_path(metapredicates_compiled, examples('metapredicates_compiled/')).
logtalk_library_path(metered_concurrency, threads('metered_concurrency/')).
logtalk_library_path(mi, examples('mi/')).
logtalk_library_path(miscellaneous, examples('miscellaneous/')).
logtalk_library_path(missing_data, examples('missing_data/')).
logtalk_library_path(modules, examples('modules/')).
logtalk_library_path(module_aliases, examples('module_aliases/')).
logtalk_library_path(msglog, examples('msglog/')).
logtalk_library_path(mtbatch, threads('mtbatch/')).
logtalk_library_path(multifile, examples('multifile/')).
logtalk_library_path(my_types, examples('my_types/')).
logtalk_library_path(named_databases, examples('named_databases/')).
logtalk_library_path(ncl, examples('ncl/')).
logtalk_library_path(nondet, threads('nondet/')).
logtalk_library_path(neo4j, examples('neo4j/')).
logtalk_library_path(now_you_see_me, examples('now_you_see_me/')).
logtalk_library_path(object_aliases, examples('object_aliases/')).
logtalk_library_path(operators, examples('operators/')).
logtalk_library_path(patches, examples('patches/')).
logtalk_library_path(patching, examples('patching/')).
logtalk_library_path(parametric, examples('parametric/')).
logtalk_library_path(pardicts, examples('pardicts/')).
logtalk_library_path(parvars, examples('parvars/')).
logtalk_library_path(pengines, examples('pengines/')).
logtalk_library_path(people, examples('people/')).
logtalk_library_path(persistency, examples('persistency/')).
logtalk_library_path(philosophers, threads('philosophers/')).
logtalk_library_path(ping_pong, threads('ping_pong/')).
logtalk_library_path(planets, examples('planets/')).
logtalk_library_path(pmq, engines('pmq/')).
logtalk_library_path(poem, examples('poem/')).
logtalk_library_path(points, examples('points/')).
logtalk_library_path(polygons, examples('polygons/')).
logtalk_library_path(predicate_lookups, examples('predicate_lookups/')).
logtalk_library_path(primes, threads('primes/')).
logtalk_library_path(process_modeling, examples('process_modeling/')).
logtalk_library_path(profiling, examples('profiling/')).
logtalk_library_path(prototypes, examples('prototypes/')).
logtalk_library_path(proxies, examples('proxies/')).
logtalk_library_path(puzzles, examples('puzzles/')).
logtalk_library_path(questions, examples('questions/')).
logtalk_library_path(quick_check, examples('quick_check/')).
logtalk_library_path(recipes, examples('recipes/')).
logtalk_library_path(reflection, examples('reflection/')).
logtalk_library_path(relations, examples('relations/')).
logtalk_library_path(roles, examples('roles/')).
logtalk_library_path(roots, examples('roots/')).
logtalk_library_path(scopes, examples('scopes/')).
logtalk_library_path(scratchcards, examples('scratchcards/')).
logtalk_library_path(searching, examples('searching/')).
logtalk_library_path(securemp, examples('securemp/')).
logtalk_library_path(self_messages, examples('self_messages/')).
logtalk_library_path(self_vs_super, examples('self_vs_super/')).
logtalk_library_path(self_vs_this, examples('self_vs_this/')).
logtalk_library_path(serialization, examples('serialization/')).
logtalk_library_path(shapes_ch, examples('shapes/ch/')).
logtalk_library_path(shapes_ph, examples('shapes/ph/')).
logtalk_library_path(sicstus, examples('sicstus/')).
logtalk_library_path(slides, examples('slides/')).
logtalk_library_path(sorting, threads('sorting/')).
logtalk_library_path(sums, engines('sums/')).
logtalk_library_path(super_calls, examples('super_calls/')).
logtalk_library_path(symbiosis, examples('symbiosis/')).
logtalk_library_path(sync, threads('sync/')).
logtalk_library_path(symdiff, examples('symdiff/')).
logtalk_library_path(tabling, examples('tabling/')).
logtalk_library_path(tcltk, examples('tcltk/')).
logtalk_library_path(tak, threads('tak/')).
logtalk_library_path(tbbt, engines('tbbt/')).
logtalk_library_path(team, threads('team/')).
logtalk_library_path(testing, examples('testing/')).
logtalk_library_path(tests_dsl, examples('tests_dsl/')).
logtalk_library_path(threads, examples('threads/')).
logtalk_library_path(trebuchet, examples('trebuchet/')).
logtalk_library_path(units, examples('units/')).
logtalk_library_path(viewpoints, examples('viewpoints/')).
logtalk_library_path(whisper, engines('whisper/')).
logtalk_library_path(wrappers, examples('wrappers/')).
logtalk_library_path(xpce, examples('xpce/')).
logtalk_library_path(yield, engines('yield/')).

% packs
logtalk_library_path(Pack, PackPath) :-
	(	var(Pack) ->
		true
	;	Pack \== logtalk_packs
	),
	(	'$lgt_expand_library_alias'(logtalk_packs, LogtalkPacks) ->
		atom_concat(LogtalkPacks, '/packs', PathsPacks)
	;	'$lgt_environment_variable'('LOGTALKPACKS', _) ->
		PathsPacks = '$LOGTALKPACKS/packs'
	;	'$lgt_environment_variable'('HOME', _) ->
		PathsPacks = '$HOME/logtalk_packs/packs'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		PathsPacks = '$USERPROFILE/logtalk_packs/packs'
	;	fail
	),
	'$lgt_expand_path'(PathsPacks, ExpandedPath),
	'$lgt_directory_exists'(ExpandedPath),
	(	var(Pack) ->
		'$lgt_directory_files'(ExpandedPath, Files),
		'$lgt_member'(Pack, Files),
		\+ sub_atom(Pack, 0, _, _, '.')
	;	true
	),
	atomic_list_concat([ExpandedPath, '/', Pack, '/'], PackPath),
	'$lgt_directory_exists'(PackPath).
