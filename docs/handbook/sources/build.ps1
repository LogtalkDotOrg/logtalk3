#############################################################################
##
##   Documentation build script
##   Last updated on May 5, 2026
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##
##       http://www.apache.org/licenses/LICENSE-2.0
##
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
##
#############################################################################


$scriptpath = $MyInvocation.MyCommand.Path
$dir = Split-Path $scriptpath
Push-Location $dir

Remove-Item ../TheLogtalkHandbook*.pdf
Remove-Item ../TheLogtalkHandbook*.epub
Remove-Item ../TheLogtalkHandbook*.info
Remove-Item ../TheLogtalkHandbook*.md
Remove-Item ../_sources -Recurse
Remove-Item ../_static -Recurse
Remove-Item ../faq -Recurse
Remove-Item ../refman -Recurse
Remove-Item ../tutorial -Recurse
Remove-Item ../userman -Recurse
Remove-Item ../devtools -Recurse
Remove-Item ../libraries -Recurse
Remove-Item ../ports -Recurse
Remove-Item ../contributions -Recurse
.\make.bat clean

(Get-Content ../../../tools/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/overview.rst
(Get-Content ../../../tools/asdf/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/asdf.rst
(Get-Content ../../../tools/assertions/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/assertions.rst
(Get-Content ../../../tools/code_metrics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/code_metrics.rst
(Get-Content ../../../tools/dead_code_scanner/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/dead_code_scanner.rst
(Get-Content ../../../tools/debug_messages/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/debug_messages.rst
(Get-Content ../../../tools/debugger/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/debugger.rst
(Get-Content ../../../tools/diagrams/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/diagrams.rst
(Get-Content ../../../tools/doclet/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/doclet.rst
(Get-Content ../../../tools/help/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/help.rst
(Get-Content ../../../tools/issue_creator/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/issue_creator.rst
(Get-Content ../../../tools/lgtdoc/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/lgtdoc.rst
(Get-Content ../../../tools/lgtunit/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/lgtunit.rst
(Get-Content ../../../tools/linter/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/linter.rst
(Get-Content ../../../tools/linter_reporter/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/linter_reporter.rst
(Get-Content ../../../tools/make/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/make.rst
(Get-Content ../../../tools/mutation_testing/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/mutation_testing.rst
(Get-Content ../../../tools/packs/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/packs.rst
(Get-Content ../../../tools/ports_profiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/ports_profiler.rst
(Get-Content ../../../tools/profiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/profiler.rst
(Get-Content ../../../tools/sarif/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/sarif.rst
(Get-Content ../../../tools/sbom/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/sbom.rst
(Get-Content ../../../tools/tool_diagnostics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/tool_diagnostics.rst
(Get-Content ../../../tools/tutor/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/tutor.rst
(Get-Content ../../../tools/wrapper/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/wrapper.rst

Get-ChildItem -Path devtools/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../../library/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/overview.rst
(Get-Content ../../../library/ada_boost/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ada_boost.rst
(Get-Content ../../../library/agglomerative/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/agglomerative.rst
(Get-Content ../../../library/amqp/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/amqp.rst
(Get-Content ../../../library/anomaly_detection_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/anomaly_detection_protocols.rst
(Get-Content ../../../library/application/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/application.rst
(Get-Content ../../../library/apriori/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/apriori.rst
(Get-Content ../../../library/arbitrary/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/arbitrary.rst
(Get-Content ../../../library/assignvars/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/assignvars.rst
(Get-Content ../../../library/avro/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/avro.rst
(Get-Content ../../../library/base32/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/base32.rst
(Get-Content ../../../library/base58/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/base58.rst
(Get-Content ../../../library/base64/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/base64.rst
(Get-Content ../../../library/base85/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/base85.rst
(Get-Content ../../../library/basic_types/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/basic_types.rst
(Get-Content ../../../library/bayesian_ridge_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/bayesian_ridge_regression.rst
(Get-Content ../../../library/borda/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/borda.rst
(Get-Content ../../../library/bradley_terry/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/bradley_terry.rst
(Get-Content ../../../library/c45/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/c45.rst
(Get-Content ../../../library/cbor/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/cbor.rst
(Get-Content ../../../library/ccsds/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ccsds.rst
(Get-Content ../../../library/character_sets/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/character_sets.rst
(Get-Content ../../../library/classification_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/classification_protocols.rst
(Get-Content ../../../library/clo_span/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/clo_span.rst
(Get-Content ../../../library/clustering_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/clustering_protocols.rst
(Get-Content ../../../library/colley/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/colley.rst
(Get-Content ../../../library/combinations/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/combinations.rst
(Get-Content ../../../library/command_line_options/NOTES.md | Select-Object -Skip 32) | pandoc -f gfm -t rst -o libraries/command_line_options.rst
(Get-Content ../../../library/copeland/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/copeland.rst
(Get-Content ../../../library/coroutining/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/coroutining.rst
(Get-Content ../../../library/csv/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/csv.rst
(Get-Content ../../../library/cuid2/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/cuid2.rst
(Get-Content ../../../library/cusum/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/cusum.rst
(Get-Content ../../../library/datalog/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/datalog.rst
(Get-Content ../../../library/dates/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dates.rst
(Get-Content ../../../library/dates_tz/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dates_tz.rst
(Get-Content ../../../library/dbscan/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dbscan.rst
(Get-Content ../../../library/dependents/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dependents.rst
(Get-Content ../../../library/deques/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/deques.rst
(Get-Content ../../../library/dictionaries/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dictionaries.rst
(Get-Content ../../../library/dif/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dif.rst
(Get-Content ../../../library/dimension_reduction_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dimension_reduction_protocols.rst
(Get-Content ../../../library/eclat/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/eclat.rst
(Get-Content ../../../library/edcg/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/edcg.rst
(Get-Content ../../../library/elastic_net_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/elastic_net_regression.rst
(Get-Content ../../../library/elo/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/elo.rst
(Get-Content ../../../library/events/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/events.rst
(Get-Content ../../../library/ewma/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ewma.rst
(Get-Content ../../../library/expand_library_alias_paths/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/expand_library_alias_paths.rst
(Get-Content ../../../library/expecteds/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/expecteds.rst
(Get-Content ../../../library/format/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/format.rst
(Get-Content ../../../library/fp_growth/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/fp_growth.rst
(Get-Content ../../../library/frequent_pattern_mining_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/frequent_pattern_mining_protocols.rst
(Get-Content ../../../library/gaussian_mixture/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gaussian_mixture.rst
(Get-Content ../../../library/gaussian_process_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gaussian_process_regression.rst
(Get-Content ../../../library/genint/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/genint.rst
(Get-Content ../../../library/gensym/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gensym.rst
(Get-Content ../../../library/geospatial/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/geospatial.rst
(Get-Content ../../../library/git/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/git.rst
(Get-Content ../../../library/glicko2/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/glicko2.rst
(Get-Content ../../../library/glicko2_periodic/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/glicko2_periodic.rst
(Get-Content ../../../library/gradient_boosting_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gradient_boosting_regression.rst
(Get-Content ../../../library/grammars/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/grammars.rst
(Get-Content ../../../library/graphs/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/graphs.rst
(Get-Content ../../../library/gsp/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gsp.rst
(Get-Content ../../../library/hashes/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hashes.rst
(Get-Content ../../../library/hdbscan/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hdbscan.rst
(Get-Content ../../../library/heaps/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/heaps.rst
(Get-Content ../../../library/hierarchical_clustering/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hierarchical_clustering.rst
(Get-Content ../../../library/hierarchies/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hierarchies.rst
(Get-Content ../../../library/hmac/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hmac.rst
(Get-Content ../../../library/hodge_rank/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hodge_rank.rst
(Get-Content ../../../library/hook_flows/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hook_flows.rst
(Get-Content ../../../library/hook_objects/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hook_objects.rst
(Get-Content ../../../library/html/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/html.rst
(Get-Content ../../../library/ica/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ica.rst
(Get-Content ../../../library/ids/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ids.rst
(Get-Content ../../../library/intervals/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/intervals.rst
(Get-Content ../../../library/iqr/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iqr.rst
(Get-Content ../../../library/iso_639/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iso_639.rst
(Get-Content ../../../library/iso_3166/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iso_3166.rst
(Get-Content ../../../library/iso_4217/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iso_4217.rst
(Get-Content ../../../library/iso_9362/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iso_9362.rst
(Get-Content ../../../library/iso_13616/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/iso_13616.rst
(Get-Content ../../../library/isolation_forest/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/isolation_forest.rst
(Get-Content ../../../library/java/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/java.rst
(Get-Content ../../../library/json/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json.rst
(Get-Content ../../../library/json_ld/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json_ld.rst
(Get-Content ../../../library/json_lines/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json_lines.rst
(Get-Content ../../../library/json_rpc/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json_rpc.rst
(Get-Content ../../../library/json_schema/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json_schema.rst
(Get-Content ../../../library/kcenters/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kcenters.rst
(Get-Content ../../../library/kemeny_young/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kemeny_young.rst
(Get-Content ../../../library/kmeans/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kmeans.rst
(Get-Content ../../../library/kernel_pca/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kernel_pca.rst
(Get-Content ../../../library/kmedians/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kmedians.rst
(Get-Content ../../../library/kmedoids/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kmedoids.rst
(Get-Content ../../../library/kmodes/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kmodes.rst
(Get-Content ../../../library/knn/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/knn.rst
(Get-Content ../../../library/knn_distance/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/knn_distance.rst
(Get-Content ../../../library/knn_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/knn_regression.rst
(Get-Content ../../../library/kprototypes/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/kprototypes.rst
(Get-Content ../../../library/ksuid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ksuid.rst
(Get-Content ../../../library/lasso_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/lasso_regression.rst
(Get-Content ../../../library/lda_projection/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/lda_projection.rst
(Get-Content ../../../library/linda/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/linda.rst
(Get-Content ../../../library/linear_algebra/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/linear_algebra.rst
(Get-Content ../../../library/linear_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/linear_regression.rst
(Get-Content ../../../library/linear_svm/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/linear_svm.rst
(Get-Content ../../../library/listing/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/listing.rst
(Get-Content ../../../library/lof/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/lof.rst
(Get-Content ../../../library/logging/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/logging.rst
(Get-Content ../../../library/logistic_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/logistic_regression.rst
(Get-Content ../../../library/loops/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/loops.rst
(Get-Content ../../../library/massey/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/massey.rst
(Get-Content ../../../library/mcp_server/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/mcp_server.rst
(Get-Content ../../../library/memcached/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/memcached.rst
(Get-Content ../../../library/meta/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/meta.rst
(Get-Content ../../../library/meta_compiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/meta_compiler.rst
(Get-Content ../../../library/mime_types/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/mime_types.rst
(Get-Content ../../../library/modified_z_score/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/modified_z_score.rst
(Get-Content ../../../library/mutations/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/mutations.rst
(Get-Content ../../../library/nanoid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/nanoid.rst
(Get-Content ../../../library/naive_bayes/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/naive_bayes.rst
(Get-Content ../../../library/nearest_centroid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/nearest_centroid.rst
(Get-Content ../../../library/nested_dictionaries/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/nested_dictionaries.rst
(Get-Content ../../../library/nmf/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/nmf.rst
(Get-Content ../../../library/optics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/optics.rst
(Get-Content ../../../library/optionals/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/optionals.rst
(Get-Content ../../../library/options/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/options.rst
(Get-Content ../../../library/os/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/os.rst
(Get-Content ../../../library/pattern_mining_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/pattern_mining_protocols.rst
(Get-Content ../../../library/pca/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/pca.rst
(Get-Content ../../../library/permutations/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/permutations.rst
(Get-Content ../../../library/plackett_luce/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/plackett_luce.rst
(Get-Content ../../../library/plackett_luce_last/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/plackett_luce_last.rst
(Get-Content ../../../library/pls_projection/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/pls_projection.rst
(Get-Content ../../../library/prefix_span/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/prefix_span.rst
(Get-Content ../../../library/probabilistic_pca/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/probabilistic_pca.rst
(Get-Content ../../../library/process/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/process.rst
(Get-Content ../../../library/protobuf/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/protobuf.rst
(Get-Content ../../../library/queues/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/queues.rst
(Get-Content ../../../library/random/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/random.rst
(Get-Content ../../../library/random_forest/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/random_forest.rst
(Get-Content ../../../library/random_forest_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/random_forest_regression.rst
(Get-Content ../../../library/random_projection/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/random_projection.rst
(Get-Content ../../../library/rank_centrality/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/rank_centrality.rst
(Get-Content ../../../library/ranked_pairs/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ranked_pairs.rst
(Get-Content ../../../library/ranking_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ranking_protocols.rst
(Get-Content ../../../library/reader/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/reader.rst
(Get-Content ../../../library/recorded_database/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/recorded_database.rst
(Get-Content ../../../library/redis/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/redis.rst
(Get-Content ../../../library/regression_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/regression_protocols.rst
(Get-Content ../../../library/regression_tree/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/regression_tree.rst
(Get-Content ../../../library/regularized_bradley_terry/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/regularized_bradley_terry.rst
(Get-Content ../../../library/ridge_regression/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ridge_regression.rst
(Get-Content ../../../library/schulze/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/schulze.rst
(Get-Content ../../../library/sequential_pattern_mining_protocols/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/sequential_pattern_mining_protocols.rst
(Get-Content ../../../library/sets/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/sets.rst
(Get-Content ../../../library/simulated_annealing/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/simulated_annealing.rst
(Get-Content ../../../library/snowflakeid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/snowflakeid.rst
(Get-Content ../../../library/sockets/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/sockets.rst
(Get-Content ../../../library/spade/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/spade.rst
(Get-Content ../../../library/statistics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/statistics.rst
(Get-Content ../../../library/stemming/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/stemming.rst
(Get-Content ../../../library/stomp/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/stomp.rst
(Get-Content ../../../library/string_distance/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/string_distance.rst
(Get-Content ../../../library/strings/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/strings.rst
(Get-Content ../../../library/subsequences/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/subsequences.rst
(Get-Content ../../../library/term_io/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/term_io.rst
(Get-Content ../../../library/thurstone_mosteller/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/thurstone_mosteller.rst
(Get-Content ../../../library/time_scales/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/time_scales.rst
(Get-Content ../../../library/timeout/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/timeout.rst
(Get-Content ../../../library/toml/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/toml.rst
(Get-Content ../../../library/toon/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/toon.rst
(Get-Content ../../../library/truncated_svd/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/truncated_svd.rst
(Get-Content ../../../library/tsv/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/tsv.rst
(Get-Content ../../../library/types/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/types.rst
(Get-Content ../../../library/tzif/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/tzif.rst
(Get-Content ../../../library/ulid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/ulid.rst
(Get-Content ../../../library/unicode_data/README.md) | pandoc -f gfm -t rst -o libraries/unicode_data.rst
(Get-Content ../../../library/union_find/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/union_find.rst
(Get-Content ../../../library/url/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/url.rst
(Get-Content ../../../library/uuid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/uuid.rst
(Get-Content ../../../library/validations/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/validations.rst
(Get-Content ../../../library/yaml/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/yaml.rst
(Get-Content ../../../library/z_score/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/z_score.rst
(Get-Content ../../../library/zippers/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/zippers.rst

Get-ChildItem -Path libraries/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" -and $file -ne "core.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../../ports/fcube/NOTES.md | Select-Object -Skip 24) | pandoc -f gfm -t rst -o ports/fcube.rst
(Get-Content ../../../ports/metagol/NOTES.md | Select-Object -Skip 35) | pandoc -f gfm -t rst -o ports/metagol.rst
(Get-Content ../../../ports/toychr/NOTES.md | Select-Object -Skip 22) | pandoc -f gfm -t rst -o ports/toychr.rst

Get-ChildItem -Path ports/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../../contributions/flags/NOTES.md) | pandoc -f gfm -t rst -o contributions/flags.rst
(Get-Content ../../../contributions/iso8601/NOTES.md | Select-Object -Skip 20) | pandoc -f gfm -t rst -o contributions/iso8601.rst
(Get-Content ../../../contributions/pddl_parser/README.txt) | pandoc -f gfm -t rst -o contributions/pddl_parser.rst
(Get-Content ../../../contributions/verdi_neruda/README.txt) | pandoc -f gfm -t rst -o contributions/verdi_neruda.rst
(Get-Content ../../../contributions/xml_parser/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o contributions/xml_parser.rst

Get-ChildItem -Path contributions/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

.\make.bat html
.\make.bat latexpdf
.\make.bat epub
.\make.bat info
.\make.bat singlehtml
#.\make.bat linkcheck

$version = Get-Content $env:LOGTALKUSER/VERSION.txt
$version_base = $version.Split("-")[0]
pandoc _build/singlehtml/index.html --wrap=none -t gfm-raw_html -o _build/singlehtml/TheLogtalkHandbook-$version_base.md

# Remove heading link references from the Markdown file
(Get-Content _build/singlehtml/TheLogtalkHandbook-$version_base.md) -replace '\[.\]\(#[-a-z0-9]+ "Link to this heading"\)', "" | Set-Content _build/singlehtml/TheLogtalkHandbook-$version_base.md
# Remove other links leaving only the text
(Get-Content _build/singlehtml/TheLogtalkHandbook-$version_base.md) -replace '\[([^]]+)\]\([^)]+\)', '$1' | Set-Content _build/singlehtml/TheLogtalkHandbook-$version_base.md

Remove-Item _build/html/index_latexpdf.html
Move-Item -Path _build/html/* -Destination ../ -Force
Remove-Item ../_sources/index_latexpdf.rst.txt
Move-Item -Path _build/latex/TheLogtalkHandbook*.pdf -Destination ../ -Force
Move-Item -Path _build/epub/TheLogtalkHandbook*.epub -Destination ../ -Force
Move-Item -Path _build/texinfo/TheLogtalkHandbook*.info -Destination ../ -Force
Move-Item -Path _build/singlehtml/TheLogtalkHandbook*.md -Destination ../ -Force

.\make.bat clean

Pop-Location
