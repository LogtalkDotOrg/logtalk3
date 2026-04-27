%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default standard library paths
%  Last updated on April 27, 2026
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

% libraries

logtalk_library_path(library, logtalk_user('library/')).

logtalk_library_path(ada_boost, library('ada_boost/')).
logtalk_library_path(agglomerative, library('agglomerative/')).
logtalk_library_path(amqp, library('amqp/')).
logtalk_library_path(anomaly_detection_protocols, library('anomaly_detection_protocols/')).
logtalk_library_path(application, library('application/')).
logtalk_library_path(apriori, library('apriori/')).
logtalk_library_path(arbitrary, library('arbitrary/')).
logtalk_library_path(assignvars, library('assignvars/')).
logtalk_library_path(avro, library('avro/')).
logtalk_library_path(base32, library('base32/')).
logtalk_library_path(base58, library('base58/')).
logtalk_library_path(base64, library('base64/')).
logtalk_library_path(base85, library('base85/')).
logtalk_library_path(basic_types, library('basic_types/')).
logtalk_library_path(borda, library('borda/')).
logtalk_library_path(bradley_terry, library('bradley_terry/')).
logtalk_library_path(c45, library('c45/')).
logtalk_library_path(cbor, library('cbor/')).
logtalk_library_path(character_sets, library('character_sets/')).
logtalk_library_path(classification_protocols, library('classification_protocols/')).
logtalk_library_path(clo_span, library('clo_span')).
logtalk_library_path(clustering_protocols, library('clustering_protocols/')).
logtalk_library_path(combinations, library('combinations/')).
logtalk_library_path(command_line_options, library('command_line_options/')).
logtalk_library_path(colley, library('colley/')).
logtalk_library_path(copeland, library('copeland/')).
logtalk_library_path(coroutining, library('coroutining/')).
logtalk_library_path(ccsds, library('ccsds/')).
logtalk_library_path(csv, library('csv/')).
logtalk_library_path(cuid2, library('cuid2/')).
logtalk_library_path(datalog, library('datalog/')).
logtalk_library_path(dates, library('dates/')).
logtalk_library_path(dates_tz, library('dates_tz/')).
logtalk_library_path(dbscan, library('dbscan/')).
logtalk_library_path(dependents, library('dependents/')).
logtalk_library_path(deques, library('deques/')).
logtalk_library_path(dictionaries, library('dictionaries/')).
logtalk_library_path(dif, library('dif/')).
logtalk_library_path(dimension_reduction_protocols, library('dimension_reduction_protocols/')).
logtalk_library_path(eclat, library('eclat/')).
logtalk_library_path(edcg, library('edcg/')).
logtalk_library_path(elo, library('elo/')).
logtalk_library_path(events, library('events/')).
logtalk_library_path(expand_library_alias_paths, library('expand_library_alias_paths/')).
logtalk_library_path(expecteds, library('expecteds/')).
logtalk_library_path(format, library('format/')).
logtalk_library_path(fp_growth, library('fp_growth/')).
logtalk_library_path(frequent_pattern_mining_protocols, library('frequent_pattern_mining_protocols/')).
logtalk_library_path(gaussian_mixture, library('gaussian_mixture/')).
logtalk_library_path(genint, library('genint/')).
logtalk_library_path(gensym, library('gensym/')).
logtalk_library_path(geospatial, library('geospatial/')).
logtalk_library_path(git, library('git/')).
logtalk_library_path(glicko2, library('glicko2/')).
logtalk_library_path(glicko2_periodic, library('glicko2_periodic/')).
logtalk_library_path(gradient_boosting_regression, library('gradient_boosting_regression/')).
logtalk_library_path(grammars, library('grammars/')).
logtalk_library_path(graphs, library('graphs/')).
logtalk_library_path(gsp, library('gsp/')).
logtalk_library_path(hashes, library('hashes/')).
logtalk_library_path(hdbscan, library('hdbscan/')).
logtalk_library_path(heaps, library('heaps/')).
logtalk_library_path(hierarchical_clustering, library('hierarchical_clustering/')).
logtalk_library_path(hierarchies, library('hierarchies/')).
logtalk_library_path(hmac, library('hmac/')).
logtalk_library_path(hodge_rank, library('hodge_rank/')).
logtalk_library_path(hook_flows, library('hook_flows/')).
logtalk_library_path(hook_objects, library('hook_objects/')).
logtalk_library_path(html, library('html/')).
logtalk_library_path(ids, library('ids/')).
logtalk_library_path(intervals, library('intervals/')).
logtalk_library_path(isolation_forest, library('isolation_forest/')).
logtalk_library_path(java, library('java/')).
logtalk_library_path(json, library('json/')).
logtalk_library_path(json_ld, library('json_ld/')).
logtalk_library_path(json_lines, library('json_lines/')).
logtalk_library_path(json_rpc, library('json_rpc/')).
logtalk_library_path(json_schema, library('json_schema/')).
logtalk_library_path(kcenters, library('kcenters/')).
logtalk_library_path(kmeans, library('kmeans/')).
logtalk_library_path(kmedians, library('kmedians/')).
logtalk_library_path(kmedoids, library('kmedoids/')).
logtalk_library_path(kmodes, library('kmodes/')).
logtalk_library_path(knn, library('knn/')).
logtalk_library_path(knn_distance, library('knn_distance/')).
logtalk_library_path(knn_regression, library('knn_regression/')).
logtalk_library_path(kprototypes, library('kprototypes/')).
logtalk_library_path(ksuid, library('ksuid/')).
logtalk_library_path(lda_projection, library('lda_projection/')).
logtalk_library_path(linda, library('linda/')).
logtalk_library_path(linear_regression, library('linear_regression/')).
logtalk_library_path(linear_svm, library('linear_svm/')).
logtalk_library_path(listing, library('listing/')).
logtalk_library_path(lof, library('lof/')).
logtalk_library_path(logging, library('logging/')).
logtalk_library_path(logistic_regression, library('logistic_regression/')).
logtalk_library_path(loops, library('loops/')).
logtalk_library_path(massey, library('massey/')).
logtalk_library_path(mcp_server, library('mcp_server/')).
logtalk_library_path(memcached, library('memcached/')).
logtalk_library_path(meta, library('meta/')).
logtalk_library_path(meta_compiler, library('meta_compiler/')).
logtalk_library_path(mime_types, library('mime_types/')).
logtalk_library_path(mutations, library('mutations/')).
logtalk_library_path(naive_bayes, library('naive_bayes/')).
logtalk_library_path(nanoid, library('nanoid/')).
logtalk_library_path(nearest_centroid, library('nearest_centroid/')).
logtalk_library_path(nested_dictionaries, library('nested_dictionaries/')).
logtalk_library_path(optics, library('optics/')).
logtalk_library_path(optionals, library('optionals/')).
logtalk_library_path(options, library('options/')).
logtalk_library_path(os, library('os/')).
logtalk_library_path(pattern_mining_protocols, library('pattern_mining_protocols/')).
logtalk_library_path(pca, library('pca/')).
logtalk_library_path(permutations, library('permutations/')).
logtalk_library_path(plackett_luce, library('plackett_luce/')).
logtalk_library_path(plackett_luce_last, library('plackett_luce_last/')).
logtalk_library_path(prefix_span, library('prefix_span/')).
logtalk_library_path(process, library('process/')).
logtalk_library_path(protobuf, library('protobuf/')).
logtalk_library_path(queues, library('queues/')).
logtalk_library_path(random, library('random/')).
logtalk_library_path(random_forest, library('random_forest/')).
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
logtalk_library_path(regularized_bradley_terry, library('regularized_bradley_terry/')).
logtalk_library_path(schulze, library('schulze/')).
logtalk_library_path(sequential_pattern_mining_protocols, library('sequential_pattern_mining_protocols/')).
logtalk_library_path(sets, library('sets/')).
logtalk_library_path(simulated_annealing, library('simulated_annealing/')).
logtalk_library_path(snowflakeid, library('snowflakeid/')).
logtalk_library_path(sockets, library('sockets/')).
logtalk_library_path(spade, library('spade/')).
logtalk_library_path(statistics, library('statistics/')).
logtalk_library_path(stemming, library('stemming/')).
logtalk_library_path(stomp, library('stomp/')).
logtalk_library_path(string_distance, library('string_distance/')).
logtalk_library_path(strings, library('strings/')).
logtalk_library_path(subsequences, library('subsequences/')).
logtalk_library_path(term_io, library('term_io/')).
logtalk_library_path(thurstone_mosteller, library('thurstone_mosteller/')).
logtalk_library_path(time_scales, library('time_scales/')).
logtalk_library_path(timeout, library('timeout/')).
logtalk_library_path(toml, library('toml/')).
logtalk_library_path(toon, library('toon/')).
logtalk_library_path(tsv, library('tsv/')).
logtalk_library_path(types, library('types/')).
logtalk_library_path(tzif, library('tzif/')).
logtalk_library_path(ulid, library('ulid/')).
logtalk_library_path(union_find, library('union_find/')).
logtalk_library_path(url, library('url/')).
logtalk_library_path(uuid, library('uuid/')).
logtalk_library_path(validations, library('validations/')).
logtalk_library_path(yaml, library('yaml/')).
logtalk_library_path(zippers, library('zippers/')).
