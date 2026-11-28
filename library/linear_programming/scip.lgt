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


:- object(scip,
	imports(linear_programming_cli)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'SCIP linear and mixed-integer programming backend using the ``scip`` command-line tool. Supports the ``executable/1``, ``time_limit/1``, and ``mip_gap/1`` (relative gap) options.',
		see_also is [simplex, milp_branch_and_bound, glpk, cbc, highs]
	]).

	:- uses(list, [
		append/3
	]).

	solver_name(scip).

	default_executable(scip).

	% SCIP is driven by shell commands (each given using the -c option)
	solver_arguments(Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		^^option(mip_gap(Gap), Options),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	^^number_atom(Seconds, SecondsAtom),
			TimeArguments = ['-c', ['set limits time ', SecondsAtom]]
		),
		(	Gap == none ->
			GapArguments = []
		;	^^number_atom(Gap, GapAtom),
			GapArguments = ['-c', ['set limits gap ', GapAtom]]
		),
		append(TimeArguments, GapArguments, SettingsArguments),
		append(['-c', ['read ', '$input']| SettingsArguments], ['-c', 'optimize', '-c', 'display solution', '-c', quit], Arguments).

	% the status is read from the "SCIP Status" line (its reason is between brackets) and the
	% solution from the "display solution" output ("objective value:" and "name value" lines)
	decode_output(Output, _, Status, Objective, Values, Text) :-
		^^split_lines(Output, Lines),
		(	^^line_after(Lines, 'SCIP Status', StatusCodes) ->
			bracket_text(StatusCodes, Text)
		;	domain_error(linear_programming_output, missing_status)
		),
		solution(Lines, SolutionData, SolutionObjective),
		(	SolutionObjective == none ->
			Feasible = false
		;	Feasible = true
		),
		scip_status(Text, Feasible, Status),
		(	( Status == optimal ; Status == feasible ) ->
			Objective = SolutionObjective,
			Values = SolutionData
		;	Objective = none,
			Values = none
		).

	bracket_text(Codes, Text) :-
		(	append(_, [0'[| Rest], Codes),
			append(Inside, [0']| _], Rest) ->
			^^normalized_text(Inside, Text)
		;	^^normalized_text(Codes, Text)
		).

	solution(Lines, SolutionData, Objective) :-
		(	append(_, [Line| Rest], Lines),
			^^line_after([Line], 'objective value:', ObjectiveCodes),
			^^parse_number(ObjectiveCodes, Number) ->
			Objective = Number,
			variable_lines(Rest, Values),
			SolutionData = solution(Values)
		;	Objective = none,
			SolutionData = none
		).

	variable_lines([], []).
	variable_lines([Line| Lines], Values) :-
		^^words(Line, Words),
		(	Words = [NameCodes, ValueCodes| _],
			variable_index(NameCodes, Index),
			^^parse_number(ValueCodes, Value) ->
			Values = [Index-Value| Values1],
			variable_lines(Lines, Values1)
		;	Values = []
		).

	variable_index([0'x| Digits], Index) :-
		Digits \== [],
		number_codes(Index, Digits).

	scip_status('optimal solution found', _, optimal).
	scip_status(infeasible, _, infeasible).
	scip_status(unbounded, _, unbounded).
	scip_status('infeasible or unbounded', _, infeasible_or_unbounded).
	scip_status('time limit reached', Feasible, Status) :-
		(	Feasible == true ->
			Status = feasible
		;	Status = time_limit
		).
	scip_status('gap limit reached', _, feasible).
	scip_status('node limit reached', Feasible, Status) :-
		(	Feasible == true ->
			Status = feasible
		;	Status = node_limit
		).
	scip_status('solution limit reached', _, feasible).
	scip_status('best solution limit reached', _, feasible).

:- end_object.
