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


:- object(gauss_legendre_quadrature(_Function_),
	imports(quadrature(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Fixed-order Gauss-Legendre quadrature for orders 2, 4, 8, and 16.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		],
		see_also is [quadrature_protocol, adaptive_simpson_quadrature(_)]
	]).

	integrate(Lower, Upper, Integral, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(order(Order), Options),
		^^check_bounds(Lower, Upper),
		(	Lower =:= Upper ->
			Integral = 0.0,
			Evaluations = 0,
			Reason = zero_interval
		;	Midpoint is (Lower + Upper) / 2.0,
			Scale is (Upper - Lower) / 2.0,
			positive_nodes(Order, Nodes),
			quadrature_sum(Nodes, Midpoint, Scale, 0.0, Sum),
			Integral is Scale * Sum,
			Evaluations = Order,
			Reason = fixed_order
		),
		Statistics = [
			evaluations(Evaluations), order(Order), estimated_error(unavailable), converged(true),
			termination_reason(Reason)
		].

	quadrature_sum([], _, _, Sum, Sum).
	quadrature_sum([Node-Weight| Nodes], Midpoint, Scale, Sum0, Sum) :-
		Left is Midpoint - Scale * Node,
		Right is Midpoint + Scale * Node,
		^^evaluate_integrand(Left, LeftValue),
		^^evaluate_integrand(Right, RightValue),
		Sum1 is Sum0 + Weight * (LeftValue + RightValue),
		quadrature_sum(Nodes, Midpoint, Scale, Sum1, Sum).

	positive_nodes(2, [
		0.5773502691896257-1.0
	]).
	positive_nodes(4, [
		0.3399810435848563-0.6521451548625461,
		0.8611363115940526-0.3478548451374538
	]).
	positive_nodes(8, [
		0.1834346424956498-0.3626837833783620,
		0.5255324099163290-0.3137066458778873,
		0.7966664774136267-0.2223810344533745,
		0.9602898564975363-0.1012285362903763
	]).
	positive_nodes(16, [
		0.09501250983763744-0.1894506104550685,
		0.2816035507792589-0.1826034150449236,
		0.4580167776572274-0.16915651939500254,
		0.6178762444026438-0.14959598881657673,
		0.7554044083550030-0.12462897125553387,
		0.8656312023878318-0.09515851168249278,
		0.9445750230732326-0.06225352393864789,
		0.9894009349916499-0.027152459411754095
	]).

	default_option(order(8)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(order(Order)) :-
		once((Order == 2; Order == 4; Order == 8; Order == 16)).
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
