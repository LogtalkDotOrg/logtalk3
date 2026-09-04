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


:- object(newton_root_finder(_Function_),
	imports(root_finder(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Newton root finder using a function first derivative and one initial guess.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol`` and defining ``derivative/2``.'
		],
		see_also is [root_finder_protocol, bisection_root_finder(_), secant_root_finder(_)]
	]).

	:- uses(_Function_, [
		derivative/2
	]).

	find_root(Initial, Root, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(max_iterations(MaxIterations), Options),
		check_guess(Initial, Guess),
		require_derivative,
		^^evaluate_function(Guess, Value),
		newton(0, MaxIterations, TolX, TolF, Guess, Value, Root, FinalValue, Iterations, FunctionEvaluations, DerivativeEvaluations, Reason, Converged),
		Statistics = [
			iterations(Iterations),
			evaluations(FunctionEvaluations),
			derivative_evaluations(DerivativeEvaluations),
			final_value(FinalValue),
			converged(Converged),
			termination_reason(Reason)
		].

	newton(Iteration, _MaxIterations, _TolX, TolF, X, Value, X, Value, Iteration, 1, 0, function_tolerance, true) :-
		abs(Value) =< TolF,
		!.
	newton(Iteration, MaxIterations, _TolX, _TolF, X, Value, X, Value, Iteration, 1, 0, max_iterations, false) :-
		Iteration >= MaxIterations,
		!.
	newton(Iteration, MaxIterations, TolX, TolF, X, Value, Root, FinalValue, Iterations, FunctionEvaluations, DerivativeEvaluations, Reason, Converged) :-
		evaluate_derivative(X, Derivative),
		(	abs(Derivative) =< 0.0 ->
			Root = X,
			FinalValue = Value,
			Iterations = Iteration,
			FunctionEvaluations = 1,
			DerivativeEvaluations = 1,
			Reason = zero_derivative,
			Converged = false
		;	Next is X - Value / Derivative,
			^^evaluate_function(Next, NextValue),
			NextIteration is Iteration + 1,
			(	abs(Next - X) =< TolX ->
				Root = Next,
				FinalValue = NextValue,
				Iterations = NextIteration,
				FunctionEvaluations = 2,
				DerivativeEvaluations = 1,
				Reason = position_tolerance,
				Converged = true
			;	newton(NextIteration, MaxIterations, TolX, TolF, Next, NextValue, Root, FinalValue, Iterations, TailFunctionEvaluations, TailDerivativeEvaluations, Reason, Converged),
				FunctionEvaluations is TailFunctionEvaluations + 1,
				DerivativeEvaluations is TailDerivativeEvaluations + 1
			)
		).

	evaluate_derivative(Argument, Derivative) :-
		derivative(Argument, Derivative),
		(	number(Derivative) ->
			true
		;	domain_error(function_derivative, Derivative)
		).

	require_derivative :-
		(	_Function_::predicate_property(derivative(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, derivative/2)
		).

	check_guess(Initial, _) :-
		var(Initial),
		instantiation_error.
	check_guess(guess(Guess), Guess) :-
		number(Guess),
		!.
	check_guess(Initial, _) :-
		domain_error(root_guess, Initial).

:- end_object.
