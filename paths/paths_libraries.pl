%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default standard library paths
%  Last updated on May 11, 2026
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
logtalk_library_path(crs_projections, library('crs_projections/')).
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
logtalk_library_path(geohash, library('geohash/')).
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
logtalk_library_path(nmea, library('nmea/')).
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
