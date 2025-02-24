%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


	normal(Mean, Deviation, Scaled) :-
		normal(Value),
		Scaled is Mean + Deviation * Value.

	normal(Value) :-
		random(X1),
		random(X2),
		Value is sqrt(-2.0 * log(X1)) * cos(2.0*pi*X2).

	lognormal(Mean, Deviation, Scaled) :-
		normal(Mean, Deviation, Value),
		Scaled is exp(Mean + Deviation * Value).

	lognormal(Value) :-
		normal(Normal),
		Value is exp(Normal).

	wald(Mean, Scale, Value) :-
		normal(Normal),
		Y is Normal * Normal,
		X is Mean + (Mean*Mean*Y) / (2*Scale) - (Mean / (2*Scale)) * sqrt(4*Mean*Scale*Y + Mean*Mean*Y*Y),
		uniform(Uniform),
		(	Uniform =< Mean / (Mean + X) ->
			Value is X
		;	Value is (Mean*Mean) / X
		).

	:- set_logtalk_flag(suspicious_calls, silent).
	geometric(Probability, Value) :-
		(	Probability =:= 0.0 ->
			Value is 0
		;	random(Random),
			Value is ceiling(log(1 - Random) / log(1 - Probability))
		).

	hypergeometric(Population, Successes, Draws, Value) :-
		hypergeometric(Draws, Population, Successes, 0, Value).

	hypergeometric(0, _, _, Value, Value) :-
		!.
	hypergeometric(N, Population0, Successes0, Value0, Value) :-
		M is N - 1,
		Population1 is Population0 - 1,
		uniform(Uniform),
		(	Uniform < Successes0 / Population0 ->
			Value1 is Value0 + 1,
			Successes1 is Successes0 - 1,
			hypergeometric(M, Population1, Successes1, Value1, Value)
		;	hypergeometric(M, Population1, Successes0, Value0, Value)
		).

	exponential(Lambda, Value) :-
		Lambda > 0,
		random(Random),
		Value is -log(1.0 - Random) / Lambda.

	binomial(Trials, Probability, Value) :-
		Trials >= 0,
		binomial(Trials, Probability, 0, Value).

	binomial(0, _, Value, Value) :-
		!.
	binomial(N, Probability, Value0, Value) :-
		M is N - 1,
		random(Random),
		(	Random < Probability ->
			Value1 is Value0 + 1,
			binomial(M, Probability, Value1, Value)
		;	binomial(M, Probability, Value0, Value)
		).

	bernoulli(Probability, Value) :-
		binomial(1, Probability, 0, Value).

	beta(Alpha, Beta, Value) :-
		gamma(Alpha, AlphaValue),
		gamma(Beta, BetaValue),
		Value is AlphaValue / (AlphaValue + BetaValue).

	gamma(Alpha, Value) :-
		Alpha > 0.0,
		(	Alpha < 1.0 ->
			uniform(Uniform),
			gamma(Alpha + 1.0, Value0),
			Value is Value0 * Uniform ** (1.0 / Alpha)
		;	D is Alpha - 1.0 / 3.0,
        	C is 1.0 / sqrt(9.0 * D),
			gamma(D, C, 0.0, 0.0, Value)
		).

	gamma(D, C, V0, _, Value) :-
		V0 =< 0.0,
		!,
		normal(Normal),
		V is 1.0 + C * Normal,
		gamma(D, C, V, Normal, Value).
	gamma(D, C, V, Normal, Value) :-
		V3 is V * V * V,
		uniform(Uniform),
		(	Uniform < 1.0 - 0.0331 * (Normal * Normal) * (Normal * Normal) ->
			Value is D * V3
		;	log(Uniform) < 0.5 * Normal * Normal + D * (1.0 - V3 + log(V3)) ->
			Value is D * V3
		;	gamma(D, C, V, Normal, Value)
		).

	logistic(Location, Scale, Value) :-
		random(Random),
		Value is Location + Scale * log(Random / (1 - Random)).

	logistic(Location, Value) :-
		random(Random),
		Value is Location + log(Random / (1 - Random)).

	logistic(Value) :-
		logistic(0.0, Value).

	poisson(Mean, Value) :-
		Mean >= 0,
		poisson(Mean, 0, 1.0, Value).

	poisson(Mean, Value, Product, Value) :-
		Product =< exp(- Mean),
		!.
	poisson(Mean, N0, Product0, Value) :-
		random(Random),
		N is N0 + 1,
		Product is Product0 * Random,
		poisson(Mean, N, Product, Value).

	power(Exponent, Value) :-
		random(Random),
		Value is Exponent * Random ** (1 / Exponent) / Exponent.

	weibull(Shape, Value) :-
		random(Random),
		Value is (-log(Random)) ** (1 / Shape).

	weibull(Lambda, Shape, Value) :-
		Shape > 0,
		random(Random),
		Value is Lambda * (-log(Random)) ** (1 / Shape).

	uniform(Lower, Upper, Value) :-
		random(Lower, Upper, Value).

	uniform(Value) :-
		random(Value).

	triangular(Left, Mode, Right, Value) :-
		Left =< Mode, Mode =< Right,
		random(Random),
		(	Random < (Mode - Left) / (Right - Left) ->
			Value is Left + sqrt(Random * (Right - Left) * (Mode - Left))
		;	Value is Right - sqrt((1 - Random) * (Right - Left) * (Right - Mode))
		).

	circular_uniform_polar(Radius, Rho, Theta) :-
		random(Random),
		Rho is Radius * sqrt(Random),
		DoublePi is 2 * pi,
		random(0.0, DoublePi, Theta).

	circular_uniform_cartesian(Radius, X, Y) :-
		circular_uniform_polar(Radius, Rho, Theta),
		X is Rho * cos(Theta),
		Y is Rho * sin(Theta).

	chi_squared(DegreesOfFreedom, Value) :-
		DegreesOfFreedom > 0,
		chi_squared(DegreesOfFreedom, 0, Value).

	chi_squared(0, Value, Value) :-
		!.
	chi_squared(N, Value0, Value) :-
		M is N - 1,
		normal(Normal),
		Value1 is Value0 + Normal * Normal,
		chi_squared(M, Value1, Value).

	standard_t(DegreesOfFreedom, Value) :-
		chi_squared(DegreesOfFreedom, ChiSquared),
		normal(Normal),
		Value is Normal / sqrt(ChiSquared / DegreesOfFreedom).

	fisher(DegreesOfFreedomNumerator, DegreesOfFreedomDenominator, Value) :-
		chi_squared(DegreesOfFreedomNumerator, ChiSquaredNumerator),
		chi_squared(DegreesOfFreedomDenominator, ChiSquaredDenominator),
		Value is (ChiSquaredNumerator / DegreesOfFreedomNumerator) / (ChiSquaredDenominator / DegreesOfFreedomDenominator).

	logseries(Shape, Value) :-
		0.0 < Shape, Shape < 1.0,
		Q is - log(1.0 - Shape),
		logseries(Shape, Q, Value).

	logseries(Shape, Q, Value) :-
		uniform(Uniform1),
		uniform(Uniform2),
		(	Uniform2 =:= 0.0 ->
			logseries(Shape, Q, Value)
		;	K is truncate(1 + log(Uniform2) / log(1.0 - Shape)),
			(	Uniform1 =< (1.0 - (1.0 - Shape) ** K) / (Q * K) ->
				Value is K
			;	logseries(Shape, Q, Value)
			)
		).

	von_mises(Mode, Concentration, Value) :-
		S is 0.5 / Concentration,
		R is S + sqrt(1.0 + S * S),
		von_mises(Mode, Concentration, S, R, Value).

	von_mises(Mode, Concentration, S, R, Value) :-
		uniform(Uniform1),
		uniform(Uniform2),
		Z is cos(pi * Uniform1),
		F is (1.0 + R * Z) / (R + Z),
		C is Concentration * (R - F),
		(	(Uniform2 < C * (2 - C); Uniform2 =< C * exp(1 - C)) ->
			uniform(Uniform3),
			(	Uniform3 > 0.5 ->
				Value is Mode + acos(F) - truncate((Mode + acos(F) / (2*pi))) * 2*pi
			;	Value is Mode - acos(F) - truncate((Mode - acos(F) / (2*pi))) * 2*pi
			)
		;	von_mises(Mode, Concentration, S, R, Value)
		).
