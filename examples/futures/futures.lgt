%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(list_math).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-02-14,
		comment is 'Simple futures programming idiom example.'
	]).

	:- threaded.

	:- public([
		product_sum/3, product_sum_engines/3
	]).

	:- uses(numberlist, [sum/2]).

	% the first alternative uses asynchronous calls but incurs a performance
	% penalty due to unification of the lists when retrieving the results

	product_sum(List1, List2, Result) :-
		% asynchronous calls (will run concurrently)
		threaded_once(sum(List1, Sum1)),
		threaded_once(sum(List2, Sum2)),
		% get results (block until available)
		threaded_exit(sum(List1, Sum1)),
		threaded_exit(sum(List2, Sum2)),
		Result is Sum1 * Sum2.

	% the second alternative uses threaded engines and unifies only the
	% resulting sums when retrieving the results

	product_sum_engines(List1, List2, Result) :-
		% engines prove goals concurrently
		threaded_engine_create(Sum1, sum(List1, Sum1), Engine1),
		threaded_engine_create(Sum2, sum(List2, Sum2), Engine2),
		% get results (block until available)
		threaded_engine_next(Engine1, Sum1),
		threaded_engine_next(Engine2, Sum2),
		Result is Sum1 * Sum2,
		% dispose of the engines
		threaded_engine_destroy(Engine1),
		threaded_engine_destroy(Engine2).

:- end_object.
