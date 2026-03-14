%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- initialization(
	logtalk::asserta(message_hook(_Message, _Kind, code_metrics, _Tokens))
).

:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(os(loader)),
	logtalk_load(options(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
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
		lcom_metric,
		wmc_metric,
		rfc_metric,
		code_metrics
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_files/test_entities',
		'test_files/third_party_test_entities'
	], [
		source_data(on), optimize(on)
	]),
	logtalk_load([
		'test_files/coupling_metric_tests',
		'test_files/dit_metric_tests',
		'test_files/doc_metric_tests',
		'test_files/noc_metric_tests',
		'test_files/nor_metric_tests',
		'test_files/upn_metric_tests',
		'test_files/size_metric_tests',
		'test_files/cc_metric_tests',
		'test_files/halstead_metric_tests',
		'test_files/lcom_metric_tests',
		'test_files/wmc_metric_tests',
		'test_files/rfc_metric_tests',
		'test_files/code_metrics_tests'
	], [
		hook(lgtunit), optimize(on)
	]),
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
		lcom_metric_tests,
		wmc_metric_tests,
		rfc_metric_tests,
		code_metrics_tests
	])
)).


:- initialization(
	logtalk::retractall(message_hook(_Message, _Kind, code_metrics, _Tokens))
).
