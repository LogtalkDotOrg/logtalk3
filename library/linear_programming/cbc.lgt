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


:- object(cbc,
	imports(linear_programming_cli)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'COIN-OR CBC linear and mixed-integer programming backend using the ``cbc`` command-line tool. Supports the ``executable/1``, ``time_limit/1``, and ``mip_gap/1`` (relative gap) options.',
		see_also is [simplex, milp_branch_and_bound, glpk, highs, scip]
	]).

	:- uses(list, [
		append/3
	]).

	solver_name(cbc).

	default_executable(cbc).

	% CBC executes its command-line arguments in sequence: the problem must be read first
	solver_arguments(Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		^^option(mip_gap(Gap), Options),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	^^number_atom(Seconds, SecondsAtom),
			TimeArguments = ['-sec', SecondsAtom]
		),
		(	Gap == none ->
			GapArguments = []
		;	^^number_atom(Gap, GapAtom),
			GapArguments = ['-ratioGap', GapAtom]
		),
		append(TimeArguments, GapArguments, ExtraArguments),
		append(['$input'| ExtraArguments], ['-solve', '-solu', '$solution'], Arguments).

	% the solution file starts with a "Status - objective value V" line followed by one line
	% per variable with its (zero-based) index, name, value, and reduced cost
	decode_output(_, Solution, Status, Objective, Values, Text) :-
		(	Solution == [] ->
			domain_error(linear_programming_output, missing_solution)
		;	true
		),
		^^split_lines(Solution, Lines),
		(	Lines = [Header| Rest],
			header(Header, Text, ReportedObjective) ->
			true
		;	domain_error(linear_programming_output, missing_status)
		),
		cbc_status(Text, ReportedObjective, Status),
		(	( Status == optimal ; Status == feasible ) ->
			Objective = ReportedObjective,
			variable_lines(Rest, Pairs),
			Values = solution(Pairs)
		;	Objective = none,
			Values = none
		).

	header(Line, Text, Objective) :-
		atom_codes(' - objective value ', Marker),
		append(Head, Rest0, Line),
		append(Marker, ObjectiveCodes, Rest0),
		!,
		^^normalized_text(Head, Text),
		^^parse_number(ObjectiveCodes, Objective).

	cbc_status('Optimal', _, optimal).
	cbc_status('Infeasible', _, infeasible).
	cbc_status('Integer infeasible', _, infeasible).
	cbc_status('Unbounded', _, unbounded).
	cbc_status('Dual infeasible', _, unbounded).
	cbc_status('Stopped on time', Objective, Status) :-
		stopped_status(Objective, time_limit, Status).
	cbc_status('Stopped on nodes', Objective, Status) :-
		stopped_status(Objective, node_limit, Status).
	cbc_status('Stopped on iterations', _, iteration_limit).
	cbc_status('Stopped on difficulties', _, numerical_error).

	% CBC reports a huge objective value when it has no solution
	stopped_status(Objective, NoSolution, Status) :-
		(	abs(Objective) >= 1.0e49 ->
			Status = NoSolution
		;	Status = feasible
		).

	variable_lines([], []).
	variable_lines([Line| Lines], Values) :-
		^^words(Line, Words0),
		(	Words0 = [[0'*, 0'*]| Words] ->
			true
		;	Words = Words0
		),
		(	Words = [_, NameCodes, ValueCodes| _],
			variable_index(NameCodes, Index),
			^^parse_number(ValueCodes, Value) ->
			Values = [Index-Value| Values1]
		;	Values = Values1
		),
		variable_lines(Lines, Values1).

	variable_index([0'x| Digits], Index) :-
		Digits \== [],
		number_codes(Index, Digits).

:- end_object.
