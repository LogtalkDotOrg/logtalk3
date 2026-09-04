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


:- object(brent_root_finder(_Function_),
	imports(root_finder(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Brent-Dekker derivative-free root finder combining interpolation with bisection safeguards.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		],
		see_also is [root_finder_protocol, bisection_root_finder(_), secant_root_finder(_)]
	]).

	find_root(Initial, Root, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(max_iterations(MaxIterations), Options),
		^^check_bracket(Initial, Lower, Upper),
		^^evaluate_function(Lower, LowerValue),
		^^evaluate_function(Upper, UpperValue),
		(	abs(LowerValue) =< TolF ->
			Root = Lower, FinalValue = LowerValue, Iterations = 0,
			Evaluations = 2, Reason = function_tolerance, Converged = true
		;	abs(UpperValue) =< TolF ->
			Root = Upper, FinalValue = UpperValue, Iterations = 0,
			Evaluations = 2, Reason = function_tolerance, Converged = true
		;	LowerValue * UpperValue < 0.0 ->
			order_endpoints(Lower, LowerValue, Upper, UpperValue, A, FA, B, FB),
			brent(0, MaxIterations, TolX, TolF, A, FA, B, FB, A, FA, A, true, Root, FinalValue, Iterations, InnerEvaluations, Reason, Converged),
			Evaluations is InnerEvaluations + 2
		;	domain_error(root_bracket, Initial)
		),
		Statistics = [
			iterations(Iterations), evaluations(Evaluations), final_value(FinalValue),
			converged(Converged), termination_reason(Reason)
		].

	brent(Iteration, _MaxIterations, _TolX, TolF, _A, _FA, B, FB, _C, _FC, _D, _Bisected, B, FB, Iteration, 0, function_tolerance, true) :-
		abs(FB) =< TolF,
		!.
	brent(Iteration, _MaxIterations, TolX, _TolF, A, _FA, B, FB, _C, _FC, _D, _Bisected, B, FB, Iteration, 0, position_tolerance, true) :-
		abs(B - A) =< TolX,
		!.
	brent(Iteration, MaxIterations, _TolX, _TolF, _A, _FA, B, FB, _C, _FC, _D, _Bisected, B, FB, Iteration, 0, max_iterations, false) :-
		Iteration >= MaxIterations,
		!.
	brent(Iteration, MaxIterations, TolX, TolF, A, FA, B, FB, C, FC, D, Bisected, Root, FinalValue, Iterations, Evaluations, Reason, Converged) :-
		interpolation_candidate(A, FA, B, FB, C, FC, Candidate),
		(	safeguard(Candidate, A, B, C, D, Bisected, TolX) ->
			S is (A + B) / 2.0,
			NextBisected = true
		;	S = Candidate,
			NextBisected = false
		),
		^^evaluate_function(S, FS),
		(	FA * FS < 0.0 ->
			A0 = A, FA0 = FA, B0 = S, FB0 = FS
		;	A0 = S, FA0 = FS, B0 = B, FB0 = FB
		),
		order_endpoints(A0, FA0, B0, FB0, A1, FA1, B1, FB1),
		NextIteration is Iteration + 1,
		brent(NextIteration, MaxIterations, TolX, TolF, A1, FA1, B1, FB1, B, FB, C, NextBisected, Root, FinalValue, Iterations, TailEvaluations, Reason, Converged),
		Evaluations is TailEvaluations + 1.

	interpolation_candidate(A, FA, B, FB, C, FC, Candidate) :-
		FA =\= FC,
		FB =\= FC,
		!,
		Candidate is
			A * FB * FC / ((FA - FB) * (FA - FC)) +
			B * FA * FC / ((FB - FA) * (FB - FC)) +
			C * FA * FB / ((FC - FA) * (FC - FB)).
	interpolation_candidate(A, FA, B, FB, _C, _FC, Candidate) :-
		Candidate is B - FB * (B - A) / (FB - FA).

	safeguard(S, A, B, C, D, Bisected, Tolerance) :-
		Boundary is (3.0 * A + B) / 4.0,
		Low is min(Boundary, B),
		High is max(Boundary, B),
		(	S =< Low; S >= High
		;	Bisected == true, abs(S - B) >= abs(B - C) / 2.0
		;	Bisected == false, abs(S - B) >= abs(C - D) / 2.0
		;	Bisected == true, abs(B - C) < Tolerance
		;	Bisected == false, abs(C - D) < Tolerance
		).

	order_endpoints(A, FA, B, FB, OrderedA, OrderedFA, OrderedB, OrderedFB) :-
		(	abs(FA) < abs(FB) ->
			OrderedA = B, OrderedFA = FB, OrderedB = A, OrderedFB = FA
		;	OrderedA = A, OrderedFA = FA, OrderedB = B, OrderedFB = FB
		).

:- end_object.
