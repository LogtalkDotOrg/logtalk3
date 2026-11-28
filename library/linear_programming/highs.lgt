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


:- object(highs,
	imports(linear_programming_cli)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'HiGHS linear and mixed-integer programming backend using the ``highs`` command-line tool. Supports the ``executable/1`` and ``time_limit/1`` options. The ``mip_gap/1`` option is not supported (the HiGHS command-line tool can only set it using an options file) and is rejected.',
		see_also is [simplex, milp_branch_and_bound, glpk, cbc, scip]
	]).

	:- uses(list, [
		append/3, member/2
	]).

	solver_name(highs).

	default_executable(highs).

	solver_arguments(Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		^^option(mip_gap(Gap), Options),
		(	Gap == none ->
			true
		;	domain_error(option, mip_gap(Gap))
		),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	^^number_atom(Seconds, SecondsAtom),
			TimeArguments = ['--time_limit', SecondsAtom]
		),
		Arguments = ['$input', '--solution_file', '$solution'| TimeArguments].

	% the status is read from the log ("Model status : X" for LPs or "Status X" in the MIP
	% solving report) and the values from the solution file
	decode_output(Output, Solution, Status, Objective, Values, Text) :-
		^^split_lines(Output, Log),
		^^split_lines(Solution, Lines),
		(	log_status(Log, Text) ->
			true
		;	solution_status(Lines, Text) ->
			true
		;	domain_error(linear_programming_output, missing_status)
		),
		solution_values(Lines, SolutionData, Feasible, SolutionObjective),
		highs_status(Text, Feasible, Status),
		(	(	Status == optimal
			;	Status == feasible
			) ->
			Objective = SolutionObjective,
			Values = SolutionData
		;	Objective = none,
			Values = none
		).

	log_status(Log, Text) :-
		(	member(Line, Log),
			atom_codes('Model status', Prefix),
			append(Prefix, Rest, Line),
			append(_, [0':| After], Rest) ->
			^^normalized_text(After, Text)
		;	member(Line, Log),
			^^words(Line, [StatusWord| Words]),
			atom_codes('Status', StatusWord),
			Words \== [] ->
			words_text(Words, Text)
		;	fail
		).

	words_text(Words, Text) :-
		join(Words, Codes),
		atom_codes(Text, Codes).

	join([Word], Word) :-
		!.
	join([Word| Words], Joined) :-
		join(Words, Rest),
		append(Word, [32| Rest], Joined).

	% the solution file starts with "Model status" followed by the status on the next line
	solution_status(Lines, Text) :-
		append(_, [Header, StatusLine| _], Lines),
		atom_codes('Model status', Codes),
		Header == Codes,
		!,
		^^normalized_text(StatusLine, Text).

	% the primal solution section: "Feasible", "Objective V", "# Columns N", and N "name value" lines
	solution_values(Lines, SolutionData, Feasible, Objective) :-
		(	append(_, [Marker, FeasibleLine| Rest], Lines),
			atom_codes('# Primal solution values', Codes),
			Marker == Codes ->
			^^normalized_text(FeasibleLine, FeasibleText),
			(	FeasibleText == 'Feasible' ->
				Feasible = true
			;	Feasible = false
			),
			(	^^line_after(Rest, 'Objective', ObjectiveCodes),
				^^parse_number(ObjectiveCodes, Number) ->
				Objective = Number
			;	Objective = none
			),
			(	append(_, [ColumnsLine| ColumnLines], Rest),
				^^line_after([ColumnsLine], '# Columns', _) ->
				column_values(ColumnLines, Values),
				SolutionData = solution(Values)
			;	SolutionData = none
			)
		;	Feasible = false,
			Objective = none,
			SolutionData = none
		).

	column_values([], []).
	column_values([Line| Lines], Values) :-
		^^words(Line, Words),
		(	Words = [NameCodes, ValueCodes],
			column_index(NameCodes, Index),
			^^parse_number(ValueCodes, Value) ->
			Values = [Index-Value| Values1],
			column_values(Lines, Values1)
		;	Values = []
		).

	column_index([0'x| Digits], Index) :-
		Digits \== [],
		number_codes(Index, Digits).

	highs_status('Optimal', _, optimal).
	highs_status('Infeasible', _, infeasible).
	highs_status('Unbounded', _, unbounded).
	highs_status('Primal infeasible or unbounded', _, infeasible_or_unbounded).
	highs_status('Unbounded or infeasible', _, infeasible_or_unbounded).
	highs_status('Time limit reached', Feasible, Status) :-
		(	Feasible == true ->
			Status = feasible
		;	Status = time_limit
		).
	highs_status('Iteration limit reached', _, iteration_limit).
	highs_status('Objective bound', _, feasible).
	highs_status('Objective target', _, feasible).

:- end_object.
