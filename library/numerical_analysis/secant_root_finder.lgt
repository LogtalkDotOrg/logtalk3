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


:- object(secant_root_finder(_Function_),
	imports(root_finder(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Derivative-free secant root finder using two initial guesses.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		],
		see_also is [root_finder_protocol, bisection_root_finder(_), newton_root_finder(_)]
	]).

	find_root(Initial, Root, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(max_iterations(MaxIterations), Options),
		check_guesses(Initial, First, Second),
		^^evaluate_function(First, FirstValue),
		^^evaluate_function(Second, SecondValue),
		(	abs(FirstValue) =< TolF ->
			Root = First,
			FinalValue = FirstValue,
			Iterations = 0,
			Reason = function_tolerance,
			Converged = true,
			Evaluations = 2
		;	abs(SecondValue) =< TolF ->
			Root = Second,
			FinalValue = SecondValue,
			Iterations = 0,
			Reason = function_tolerance,
			Converged = true,
			Evaluations = 2
		;	secant(0, MaxIterations, TolX, TolF, First, FirstValue, Second, SecondValue, Root, FinalValue, Iterations, InnerEvaluations, Reason, Converged),
			Evaluations is InnerEvaluations + 2
		),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			final_value(FinalValue),
			converged(Converged),
			termination_reason(Reason)
		].

	secant(Iteration, MaxIterations, _TolX, _TolF, _First, _FirstValue, Second, SecondValue, Second, SecondValue, Iteration, 0, max_iterations, false) :-
		Iteration >= MaxIterations,
		!.
	secant(Iteration, _MaxIterations, _TolX, _TolF, _First, FirstValue, Second, SecondValue, Second, SecondValue, Iteration, 0, zero_denominator, false) :-
		abs(SecondValue - FirstValue) =< 0.0,
		!.
	secant(Iteration, MaxIterations, TolX, TolF, First, FirstValue, Second, SecondValue, Root, FinalValue, Iterations, Evaluations, Reason, Converged) :-
		Next is Second - SecondValue * (Second - First) / (SecondValue - FirstValue),
		^^evaluate_function(Next, NextValue),
		NextIteration is Iteration + 1,
		(	abs(NextValue) =< TolF ->
			Root = Next,
			FinalValue = NextValue,
			Iterations = NextIteration,
			Evaluations = 1,
			Reason = function_tolerance,
			Converged = true
		;	abs(Next - Second) =< TolX ->
			Root = Next,
			FinalValue = NextValue,
			Iterations = NextIteration,
			Evaluations = 1,
			Reason = position_tolerance,
			Converged = true
		;	secant(NextIteration, MaxIterations, TolX, TolF, Second, SecondValue, Next, NextValue, Root, FinalValue, Iterations, TailEvaluations, Reason, Converged),
			Evaluations is TailEvaluations + 1
		).

	check_guesses(Initial, _, _) :-
		var(Initial),
		instantiation_error.
	check_guesses(guesses(First, Second), First, Second) :-
		number(First), number(Second), First =\= Second,
		!.
	check_guesses(Initial, _, _) :-
		domain_error(root_guesses, Initial).

	default_option(Option) :-
		^^default_option(Option).

	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
