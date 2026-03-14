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


:- object(rfc_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Unit tests for the rfc_metric object.'
	]).

	:- uses(rfc_metric, [
		entity_score/2
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(rfc_metric).

	% rfc_obj_1 defines p/0 and q/0; p calls q (internal only)
	% Response set = {p/0, q/0} => RFC = 2
	test(rfc_metric_01, true(Score == 2)) :-
		entity_score(rfc_obj_1, Score).

	% rfc_obj_2 defines a/0 and b/0; a calls b (internal), b calls rfc_obj_1::p (external)
	% Response set = {a/0, b/0, rfc_obj_1::p/0} => RFC = 3
	test(rfc_metric_02, true(Score == 3)) :-
		entity_score(rfc_obj_2, Score).

	% rfc_cat_1 defines x/0 and y/0; x calls y (internal only)
	% Response set = {x/0, y/0} => RFC = 2
	test(rfc_metric_03, true(Score == 2)) :-
		entity_score(rfc_cat_1, Score).

	% protocol: entity_score/2 must fail
	test(rfc_metric_protocol, fail) :-
		entity_score(prot_a, _).

:- end_object.
