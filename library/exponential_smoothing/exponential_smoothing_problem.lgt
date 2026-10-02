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


:- object(exponential_smoothing_problem(_Method_, _Series_, _OptimizationSeries_, _Frequency_, _ParameterSpecification_, _InitializationSpecification_),
	implements([local_optimization_problem_protocol, differential_evolution_problem_protocol]),
	imports(exponential_smoothing_common)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Bounded sum-of-squared-errors optimization problem for exponential smoothing parameters. Implements both the local-optimization (Nelder-Mead) and Differential Evolution problem protocols over the same objective.',
		parameters is [
			'Method' - 'Exponential smoothing method.',
			'Series' - 'Validated training series.',
			'OptimizationSeries' - 'Training series with missing observations removed for optimizer initialization and bounds.',
			'Frequency' - 'Seasonal frequency or ``none``.',
			'ParameterSpecification' - 'Canonical list of fixed or automatic parameter options.',
			'InitializationSpecification' - 'Initialization strategy and number of initial cycles.'
		]
	]).

	initial_point(Point) :-
		^^optimization_initial_point(_Method_, _OptimizationSeries_, _Frequency_, _ParameterSpecification_, _InitializationSpecification_, Point0),
		multiplicative_initial_point(_Method_, _ParameterSpecification_, Point0, Point).

	position_bounds(Bounds) :-
		^^optimization_bounds(_Method_, _OptimizationSeries_, _Frequency_, _ParameterSpecification_, _InitializationSpecification_, Bounds).

	objective(Point, SumSquaredError) :-
		^^optimization_components(_Method_, _OptimizationSeries_, _Frequency_, _ParameterSpecification_, _InitializationSpecification_, Point, Parameters, EffectiveInitializationSpecification),
		(	catch(
				^^fit_smoothing(_Method_, _Series_, _Frequency_, Parameters, EffectiveInitializationSpecification, _State, SumSquaredError, _ErrorCount, _Residuals),
				error(domain_error(positive_multiplicative_level, _), _),
				fail
			) ->
			true
		;	objective_penalty(_OptimizationSeries_, SumSquaredError)
		).

	fitness(Point, Fitness) :-
		objective(Point, Fitness).

	multiplicative_initial_point(holt_winters_multiplicative, [alpha(auto)| _], [_Alpha| Point], [1.0| Point]) :-
		!.
	multiplicative_initial_point(holt_winters_multiplicative_damped, [alpha(auto)| _], [_Alpha| Point], [1.0| Point]) :-
		!.
	multiplicative_initial_point(_Method, _ParameterSpecification, Point, Point).

	objective_penalty(Series, Penalty) :-
		sum_squares(Series, 0.0, SumSquares),
		Penalty is 1000000.0 * (SumSquares + 1.0).

	sum_squares([], SumSquares, SumSquares).
	sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_squares(Values, SumSquares1, SumSquares).

:- end_object.
