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


:- object(chi_square_dataset(_Kind_),
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Mixed, missing, degenerate, and malformed dataset fixtures for chi-square selection.'
	]).

	attribute_values(signal, Declaration) :-
		parameter(1, Kind),
		declaration(Kind, Declaration).
	attribute_values(copy, [a, b]).
	attribute_values(constant, [a]).

	example_count(Count) :-
		parameter(1, Kind),
		(	(Kind == missing; Kind == joint_extremes) ->
			Count = 6
		;	(Kind == weak; Kind == unused_categories) ->
			Count = 8
		;	Kind == multiclass ->
			Count = 6
		;	Count = 4
		).

	example(Id, Features, Target) :-
		parameter(1, Kind),
		row(Kind, Id, Features, Target).

	declaration(empty_domain, []).
	declaration(compound_domain, [a, label(b)]).
	declaration(unbound_domain, _).
	declaration(mixed, continuous).
	declaration(missing, continuous).
	declaration(joint_extremes, continuous).
	declaration(all_missing, continuous).
	declaration(bad_number, continuous).
	declaration(bad_target, continuous).
	declaration(outside_domain, [0, 1]).
	declaration(unique, [0, 1, 9, 10]).
	declaration(single_class, continuous).
	declaration(weak, [0, 1]).
	declaration(unused_categories, [0, 1, 2, 3]).
	declaration(multiclass, [0, 1, 2]).

	row(mixed, 1, [signal-0, copy-a, constant-a], x).
	row(mixed, 2, [signal-1, copy-a, constant-a], x).
	row(mixed, 3, [signal-9, copy-b, constant-a], y).
	row(mixed, 4, [signal-10, copy-b, constant-a], y).
	row(missing, 1, [signal-0, copy-a, constant-a], x).
	row(missing, 2, [signal-1, copy-_, constant-a], x).
	row(missing, 3, [signal-9, copy-b, constant-a], y).
	row(missing, 4, [signal-10, constant-a], y).
	row(missing, 5, [signal-_, copy-a, constant-a], x).
	row(missing, 6, [signal-100, copy-b, constant-a], _).
	row(joint_extremes, Id, Features, Target) :-
		row(mixed, Id, Features, Target).
	row(joint_extremes, 5, [signal-100, copy-_, constant-a], x).
	row(joint_extremes, 6, [signal-_, copy-b, constant-a], y).
	row(all_missing, 1, [signal-_, copy-_, constant-_], x).
	row(all_missing, 2, [], x).
	row(all_missing, 3, [signal-_, copy-_, constant-_], y).
	row(all_missing, 4, [], y).
	row(bad_number, 1, [signal-bad, copy-a, constant-a], x).
	row(bad_number, 2, [signal-1, copy-a, constant-a], x).
	row(bad_number, 3, [signal-9, copy-b, constant-a], y).
	row(bad_number, 4, [signal-10, copy-b, constant-a], y).
	row(bad_target, 1, [signal-0, copy-a, constant-a], label(x)).
	row(bad_target, 2, [signal-1, copy-a, constant-a], x).
	row(bad_target, 3, [signal-9, copy-b, constant-a], y).
	row(bad_target, 4, [signal-10, copy-b, constant-a], y).
	row(outside_domain, 1, [signal-9, copy-a, constant-a], x).
	row(outside_domain, 2, [signal-1, copy-a, constant-a], x).
	row(outside_domain, 3, [signal-0, copy-b, constant-a], y).
	row(outside_domain, 4, [signal-1, copy-b, constant-a], y).
	row(weak, 1, [signal-0, copy-a, constant-a], x).
	row(weak, 2, [signal-0, copy-a, constant-a], x).
	row(weak, 3, [signal-0, copy-a, constant-a], x).
	row(weak, 4, [signal-0, copy-b, constant-a], y).
	row(weak, 5, [signal-1, copy-a, constant-a], x).
	row(weak, 6, [signal-1, copy-b, constant-a], y).
	row(weak, 7, [signal-1, copy-b, constant-a], y).
	row(weak, 8, [signal-1, copy-b, constant-a], y).
	row(multiclass, 1, [signal-0, copy-a, constant-a], x).
	row(multiclass, 2, [signal-0, copy-a, constant-a], x).
	row(multiclass, 3, [signal-1, copy-b, constant-a], y).
	row(multiclass, 4, [signal-1, copy-b, constant-a], z).
	row(multiclass, 5, [signal-2, copy-b, constant-a], y).
	row(multiclass, 6, [signal-2, copy-b, constant-a], z).
	row(unique, Id, Features, Target) :-
		row(mixed, Id, Features, Target).
	row(unused_categories, Id, Features, Target) :-
		row(weak, Id, Features, Target).
	row(single_class, Id, Features, x) :-
		row(mixed, Id, Features, _).
	row(empty_domain, Id, Features, Target) :-
		row(mixed, Id, Features, Target).
	row(compound_domain, Id, Features, Target) :-
		row(mixed, Id, Features, Target).
	row(unbound_domain, Id, Features, Target) :-
		row(mixed, Id, Features, Target).

:- end_object.
