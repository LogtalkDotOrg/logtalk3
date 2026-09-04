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


:- object(bar).

	:- public(bar/2).
	:- mode(bar(-integer, -atom), one).

	:- public(parse/0).

	:- public(token//1).
	:- mode_non_terminal(token(-atom), one).

	bar(X, Y) :-
		baz(X),
		qux(Y).

	baz(1).
	baz(2).

	qux(a).
	qux(b).
	qux(c).

	parse :-
		phrase(token(_), [a]).

	token(a) -->
		[a].
	token(a) -->
		[a].

:- end_object.


:- object(foo).

	:- public(solutions/0).
	:- mode(solutions, one).

	solutions :-
		bar::bar(_, _),
		fail.
	solutions.

:- end_object.


:- object(mode_cases).

	:- public(run/0).

	:- public(det_zero/1).
	:- mode(det_zero(-integer), zero).

	:- public(det_zero_or_one/1).
	:- mode(det_zero_or_one(-integer), zero_or_one).

	:- public(det_one/1).
	:- mode(det_one(-integer), one).

	:- public(det_zero_or_error/1).
	:- mode(det_zero_or_error(-integer), zero_or_error).

	:- public(det_one_or_error/1).
	:- mode(det_one_or_error(-integer), one_or_error).

	:- public(det_zero_or_one_or_error/1).
	:- mode(det_zero_or_one_or_error(-integer), zero_or_one_or_error).

	:- public(det_error/1).
	:- mode(det_error(-integer), error).

	:- public(mixed/1).
	:- mode(mixed(-integer), one).
	:- mode(mixed(?integer), zero_or_more).

	:- public(non_deterministic/1).
	:- mode(non_deterministic(-integer), zero_or_more).

	:- public(no_mode/1).

	:- public(deterministic_exit/1).
	:- mode(deterministic_exit(-integer), one).

	run :-
		det_zero(_),
		det_zero_or_one(_),
		det_one(_),
		det_zero_or_error(_),
		det_one_or_error(_),
		det_zero_or_one_or_error(_),
		det_error(_),
		mixed(_),
		non_deterministic(_),
		no_mode(_),
		undeclared(_),
		deterministic_exit(_).

	det_zero(1).
	det_zero(2).

	det_zero_or_one(1).
	det_zero_or_one(2).

	det_one(1).
	det_one(2).

	det_zero_or_error(1).
	det_zero_or_error(2).

	det_one_or_error(1).
	det_one_or_error(2).

	det_zero_or_one_or_error(1).
	det_zero_or_one_or_error(2).

	det_error(1).
	det_error(2).

	mixed(1).
	mixed(2).

	non_deterministic(1).
	non_deterministic(2).

	no_mode(1).
	no_mode(2).

	undeclared(1).
	undeclared(2).

	deterministic_exit(1).

:- end_object.
