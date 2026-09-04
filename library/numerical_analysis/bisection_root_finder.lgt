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


:- object(bisection_root_finder(_Function_),
	imports(root_finder(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Bisection root finder for a continuous scalar function over a sign-changing bracket.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		],
		see_also is [root_finder_protocol, univariate_function_protocol]
	]).

	find_root(Initial, Root, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(max_iterations(MaxIterations), Options),
		^^check_bracket(Initial, Lower, Upper),
		^^evaluate_function(Lower, LowerValue),
		(	abs(LowerValue) =< TolF ->
			Root = Lower,
			Iterations = 0,
			Evaluations = 1,
			FinalValue = LowerValue,
			Reason = function_tolerance,
			Converged = true
		;	^^evaluate_function(Upper, UpperValue),
			(	abs(UpperValue) =< TolF ->
				Root = Upper,
				Iterations = 0,
				Evaluations = 2,
				FinalValue = UpperValue,
				Reason = function_tolerance,
				Converged = true
			;	LowerValue * UpperValue < 0.0 ->
				bisect(0, MaxIterations, TolX, TolF, Lower, LowerValue, Upper, UpperValue, Root, FinalValue, Iterations, InnerEvaluations, Reason, Converged),
				Evaluations is InnerEvaluations + 2
			;	domain_error(root_bracket, Initial)
			)
		),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			final_value(FinalValue),
			converged(Converged),
			termination_reason(Reason)
		].

	bisect(Iteration, MaxIterations, _TolX, _TolF, Lower, LowerValue, Upper, UpperValue, Root, FinalValue, Iteration, 0, max_iterations, false) :-
		Iteration >= MaxIterations,
		!,
		best_endpoint(Lower, LowerValue, Upper, UpperValue, Root, FinalValue).
	bisect(Iteration, MaxIterations, TolX, TolF, Lower, LowerValue, Upper, UpperValue, Root, FinalValue, Iterations, Evaluations, Reason, Converged) :-
		Middle is Lower + (Upper - Lower) / 2.0,
		^^evaluate_function(Middle, MiddleValue),
		NextIteration is Iteration + 1,
		(	abs(MiddleValue) =< TolF ->
			Root = Middle,
			FinalValue = MiddleValue,
			Iterations = NextIteration,
			Evaluations = 1,
			Reason = function_tolerance,
			Converged = true
		;	(Upper - Lower) / 2.0 =< TolX ->
			Root = Middle,
			FinalValue = MiddleValue,
			Iterations = NextIteration,
			Evaluations = 1,
			Reason = position_tolerance,
			Converged = true
		;	LowerValue * MiddleValue < 0.0 ->
			bisect(NextIteration, MaxIterations, TolX, TolF, Lower, LowerValue, Middle, MiddleValue, Root, FinalValue, Iterations, TailEvaluations, Reason, Converged),
			Evaluations is TailEvaluations + 1
		;	bisect(NextIteration, MaxIterations, TolX, TolF, Middle, MiddleValue, Upper, UpperValue, Root, FinalValue, Iterations, TailEvaluations, Reason, Converged),
			Evaluations is TailEvaluations + 1
		).

	best_endpoint(Lower, LowerValue, Upper, UpperValue, Root, Value) :-
		(	abs(LowerValue) =< abs(UpperValue) ->
			Root = Lower,
			Value = LowerValue
		;	Root = Upper,
			Value = UpperValue
		).

:- end_object.
