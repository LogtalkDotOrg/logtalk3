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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'Unit tests for the "full_adder_diagnosis" example.'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	cover(full_adder_diagnosis).

	cleanup :-
		^^clean_text_output.

	% the multipliers and adders derive the values a correctly
	% functioning circuit would produce
	test(full_adder_diagnosis_1, deterministic(Values == [6, 6, 6, 12, 12])) :-
		full_adder_diagnosis::build(State),
		atms::nodes(State, Nodes),
		findall(
			Value,
			(	member(Wire, [x1, x2, x3, y1, y2]),
				memberchk(_-val(Wire, Value), Nodes)
			),
			Values
		).

	% the Y1 mismatch (predicted 12, observed 10) is recorded as the
	% single minimal nogood {ok(m1), ok(m2), ok(a1)}
	test(full_adder_diagnosis_2, deterministic(Nogoods == [[ok(m1), ok(m2), ok(a1)]])) :-
		full_adder_diagnosis::build(State),
		atms::nodes(State, Nodes),
		findall(
			Data,
			(	atms::nogood(Nogood, State),
				full_adder_diagnosis::node_data(Nogood, Nodes, Data)
			),
			Nogoods
		).

	% with that nogood recorded, val(y1,12) can no longer be derived
	% under any consistent set of health assumptions
	test(full_adder_diagnosis_3, deterministic(Label == [])) :-
		full_adder_diagnosis::build(State),
		atms::nodes(State, Nodes),
		memberchk(Node-val(y1, 12), Nodes),
		atms::label(Node, State, Label).

	% val(y2,12) is unaffected and keeps a non-empty label
	test(full_adder_diagnosis_4, deterministic(Label \== [])) :-
		full_adder_diagnosis::build(State),
		atms::nodes(State, Nodes),
		memberchk(Node-val(y2, 12), Nodes),
		atms::label(Node, State, Label).

	% the single conflict set yields exactly the three classical
	% minimal single-fault diagnoses
	test(full_adder_diagnosis_5, deterministic(Diagnoses == [[ok(a1)], [ok(m1)], [ok(m2)]])) :-
		full_adder_diagnosis::build(State),
		full_adder_diagnosis::diagnoses(State, Diagnoses).

	% report/0 runs the whole example end-to-end without raising an error
	test(full_adder_diagnosis_6, deterministic) :-
		^^set_text_output(''),
		full_adder_diagnosis::report.

:- end_object.
