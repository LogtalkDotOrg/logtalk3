%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(doc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:1,
		author is 'Paulo Moura',
		date is 2020-04-13,
		comment is 'Unit tests for the code documentation metric.'
	]).

	cover(code_metric).
	cover(doc_metric).

	test(doc_expanding) :-
		doc_metric_valid(expanding).

	test(doc_forwarding) :-
		doc_metric_valid(forwarding).

	test(doc_monitoring) :-
		doc_metric_valid(monitoring).

	test(doc_logtalk) :-
		doc_metric_valid(logtalk).

	test(doc_user) :-
		doc_metric_valid(user).

	% auxiliary predicates

	doc_metric_valid(Entity) :-
		doc_metric::entity_score(Entity, Score),
		0 =< Score, Score =< 100.

:- end_object.
