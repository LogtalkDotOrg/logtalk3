%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


% to avoid problems with backend Prolog compilers such as ECLiPSe where
% reloading a file defining clauses for a multifile predicate results in
% the duplication of the clauses, below we load the required libraries
% for the "lgtunit" tool separately so that we can load the "random"
% library under testing in debug mode

:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(basic_types(loader)),
	logtalk_load(
		[random_protocol, pseudo_random_protocol, sampling_protocol, random, backend_random, fast_random],
		[debug(on), source_data(on), clean(on)]
	),
	logtalk_load(types(loader)),
	logtalk_load([arbitrary(arbitrary)], [optimize(on)]),
	logtalk_load(os(loader)),
	logtalk_load([lgtunit(lgtunit_messages), lgtunit(lgtunit)], [optimize(on)]),
	logtalk_load(tests, [hook(lgtunit)]),
	(	current_prolog_flag(bounded, false) ->
		lgtunit::run_test_sets([
			tests(random),
			tests(random(as183)),
			tests(random(xoshiro128pp)),
			tests(random(xoshiro128ss)),
			tests(random(xoshiro256pp)),
			tests(random(xoshiro256ss)),
			tests(random(splitmix64)),
			tests(fast_random),
			tests(fast_random(as183)),
			tests(fast_random(xoshiro128pp)),
			tests(fast_random(xoshiro128ss)),
			tests(fast_random(xoshiro256pp)),
			tests(fast_random(xoshiro256ss)),
			tests(fast_random(splitmix64)),
			tests(backend_random)
		])
	;	lgtunit::run_test_sets([
			tests(random),
			tests(random(as183)),
			tests(random(xoshiro128pp)),
			tests(random(xoshiro128ss)),
			tests(fast_random),
			tests(fast_random(as183)),
			tests(fast_random(xoshiro128pp)),
			tests(fast_random(xoshiro128ss)),
			tests(backend_random)
		])
	)
)).
