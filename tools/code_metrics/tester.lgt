%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(os(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load(
		[
			code_metrics_utilities,
			code_metric,
			code_metrics_messages,
			dit_metric,
			coupling_metric,
			noc_metric,
			nor_metric,
			upn_metric,
			doc_metric,
			size_metric,
			cc_metric,
			halstead_metric,
			code_metrics
		],
		[	
			source_data(on),
			debug(on)
		]
	),
	logtalk_load(
		[
			test_entities,
			third_party_test_entities
		], [
			source_data(on)
		]
	),
	logtalk_load(
		[
			coupling_metric_tests,
			dit_metric_tests,
			doc_metric_tests,
			noc_metric_tests,
			nor_metric_tests,
			upn_metric_tests,
			size_metric_tests,
			cc_metric_tests,
			halstead_metric_tests,
			code_metrics_tests
		], [
			hook(lgtunit), optimize(on)
		]
	),
	lgtunit::run_test_sets([
		coupling_metric_tests,
		dit_metric_tests,
		doc_metric_tests,
		noc_metric_tests,
		nor_metric_tests,
		upn_metric_tests,
		size_metric_tests,
		cc_metric_tests,
		halstead_metric_tests,
		code_metrics_tests
	])
)).
