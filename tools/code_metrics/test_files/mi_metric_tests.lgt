%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2018-2026 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_object(lines_metric)).

:- object(mi_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Unit tests for the mi_metric object.'
	]).

	:- uses(mi_metric, [
		entity_score/2
	]).

	cover(code_metric).
	cover(mi_metric).

	test(mi_metric_entity_score_shape, true((number(MI), MI =< 171.0))) :-
		entity_score(lcom_obj_1, mi(MI)).

	test(mi_metric_protocol, fail) :-
		entity_score(prot_a, _).

:- end_object.

:- else.

:- object(mi_metric_tests,
	extends(lgtunit)).
:- end_object.

:- endif.
