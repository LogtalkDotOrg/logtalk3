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


:- object(glpk,
	imports(linear_programming_cli)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'GLPK (GNU Linear Programming Kit) linear and mixed-integer programming backend using the ``glpsol`` command-line tool. Supports the ``executable/1``, ``time_limit/1`` (rounded up to whole seconds), and ``mip_gap/1`` options.',
		see_also is [simplex, milp_branch_and_bound, cbc, highs, scip]
	]).

	:- uses(list, [
		append/3
	]).

	solver_name(glpk).

	default_executable(glpsol).

	solver_arguments(Options, Arguments) :-
		^^option(time_limit(Seconds), Options),
		^^option(mip_gap(Gap), Options),
		(	Seconds =:= 0 ->
			TimeArguments = []
		;	(	integer(Seconds) ->
				Limit is Seconds
			;	Limit is ceiling(Seconds)
			),
			^^number_atom(Limit, LimitAtom),
			TimeArguments = ['--tmlim', LimitAtom]
		),
		(	Gap == none ->
			GapArguments = []
		;	^^number_atom(Gap, GapAtom),
			GapArguments = ['--mipgap', GapAtom]
		),
		append(TimeArguments, GapArguments, ExtraArguments),
		Arguments = ['--lp', '$input', '--output', '$solution'| ExtraArguments].

	% the status is read from the solution report and refined using the log messages
	decode_output(Output, Solution, Status, Objective, Values, Text) :-
		(	Solution == [] ->
			domain_error(linear_programming_output, missing_report)
		;	true
		),
		^^split_lines(Solution, Report),
		^^split_lines(Output, Log),
		(	^^line_after(Report, 'Status:', StatusCodes) ->
			^^normalized_text(StatusCodes, Text)
		;	domain_error(linear_programming_output, missing_status)
		),
		(	^^line_after(Report, 'Objective:', ObjectiveCodes),
			^^words(ObjectiveCodes, [_, _, ObjectiveWord| _]),
			^^parse_number(ObjectiveWord, Number) ->
			Objective = Number
		;	Objective = none
		),
		(	^^has_text(Log, 'TIME LIMIT EXCEEDED') ->
			TimeLimit = true
		;	TimeLimit = false
		),
		(	^^has_text(Log, 'UNBOUNDED') ->
			Unbounded = true
		;	Unbounded = false
		),
		(	^^has_text(Log, 'NO PRIMAL FEASIBLE') ->
			NoFeasible = true
		;	NoFeasible = false
		),
		glpk_status(Text, TimeLimit, Unbounded, NoFeasible, Status),
		report_values(Report, Values).

	glpk_status('OPTIMAL', _, _, _, optimal).
	glpk_status('INTEGER OPTIMAL', _, _, _, optimal).
	glpk_status('INFEASIBLE', _, _, _, infeasible).
	glpk_status('NO FEASIBLE', _, _, _, infeasible).
	glpk_status('INTEGER EMPTY', _, _, _, infeasible).
	glpk_status('UNBOUNDED', _, _, _, unbounded).
	glpk_status('FEASIBLE', _, _, _, feasible).
	glpk_status('INTEGER NON-OPTIMAL', _, _, _, feasible).
	glpk_status('UNDEFINED', TimeLimit, Unbounded, NoFeasible, Status) :-
		undefined_status(TimeLimit, Unbounded, NoFeasible, Status).
	glpk_status('INTEGER UNDEFINED', TimeLimit, Unbounded, NoFeasible, Status) :-
		undefined_status(TimeLimit, Unbounded, NoFeasible, Status).

	undefined_status(_, true, _, unbounded) :-
		!.
	undefined_status(_, _, true, infeasible) :-
		!.
	undefined_status(true, _, _, time_limit) :-
		!.
	undefined_status(_, _, _, numerical_error).

	% the columns table follows the "Column name" header line
	report_values(Report, SolutionData) :-
		(	append(_, [Header| Lines], Report),
			^^has_text([Header], 'Column name') ->
			column_values(Lines, Values),
			SolutionData = solution(Values)
		;	SolutionData = none
		).

	column_values([], []).
	column_values([Line| Lines], Values) :-
		^^words(Line, Words),
		(	Words == [] ->
			Values = []
		;	Words = [_, NameCodes| Rest],
			column_index(NameCodes, Index),
			^^first_number(Rest, Value) ->
			Values = [Index-Value| Values1],
			column_values(Lines, Values1)
		;	column_values(Lines, Values)
		).

	column_index([0'x| Digits], Index) :-
		Digits \== [],
		number_codes(Index, Digits).

:- end_object.
