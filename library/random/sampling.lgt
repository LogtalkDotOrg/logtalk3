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


	normal(Mean, Deviation, Scaled) :-
		standard_normal(Value),
		Scaled is Mean + Deviation * Value.

	lognormal(Mean, Deviation, Scaled) :-
		normal(Mean, Deviation, Value),
		Scaled is exp(Mean + Deviation * Value).

	wald(Mean, Scale, Value) :-
		Mean > 0.0,
		Scale > 0.0,
		standard_normal(Normal),
		Y is Normal * Normal,
		X is Mean + (Mean*Mean*Y) / (2*Scale) - (Mean / (2*Scale)) * sqrt(4*Mean*Scale*Y + Mean*Mean*Y*Y),
		random(Uniform),
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
		random(Uniform),
		(	Uniform < Successes0 / Population0 ->
			Value1 is Value0 + 1,
			Successes1 is Successes0 - 1,
			hypergeometric(M, Population1, Successes1, Value1, Value)
		;	hypergeometric(M, Population1, Successes0, Value0, Value)
		).

	exponential(Scale, Value) :-
		Scale > 0,
		standard_exponential(Value0),
		Value is Value0 * Scale.

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
		standard_gamma(Alpha, AlphaValue),
		standard_gamma(Beta, BetaValue),
		Value is AlphaValue / (AlphaValue + BetaValue).

	gamma(Shape, Scale, Value) :-
		Scale > 0.0,
		standard_gamma(Shape, Value0),
		Value is Value0 * Scale.

	logistic(Location, Scale, Value) :-
		Scale > 0.0,
		random(Random),
		Value is Location + Scale * log(Random / (1 - Random)).

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

	weibull(Shape, Scale, Value) :-
		Shape > 0,
		Scale > 0,
		random(Random),
		(	Random =:= 0.0 ->
			weibull(Shape, Scale, Value)
		;	Value is Scale * (-log(Random)) ** (1 / Shape)
		).

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
		standard_normal(Normal),
		Value1 is Value0 + Normal * Normal,
		chi_squared(M, Value1, Value).

	standard_t(DegreesOfFreedom, Value) :-
		chi_squared(DegreesOfFreedom, ChiSquared),
		standard_normal(Normal),
		Value is Normal / sqrt(ChiSquared / DegreesOfFreedom).

	standard_cauchy(Location, Scale, Value) :-
		random(Uniform),
		Value is Location + Scale * tan(pi * (Uniform - 0.5)).

	standard_exponential(Value) :-
		random(Uniform),
		Value is -log(1.0 - Uniform).

	standard_gamma(Shape, Value) :-
		Shape > 0.0,
		standard_gamma_(Shape, Value).

	standard_gamma_(Shape, Value) :-
		(	Shape < 1.0 ->
			random(Uniform),
			standard_gamma(Shape + 1.0, Value0),
			Value is Value0 * Uniform ** (1.0 / Shape)
		;	D is Shape - 1.0 / 3.0,
        	C is 1.0 / sqrt(9.0 * D),
			standard_gamma(D, C, 0.0, 0.0, Value)
		).

	standard_gamma(D, C, V0, _, Value) :-
		V0 =< 0.0,
		!,
		standard_normal(Normal),
		V is 1.0 + C * Normal,
		standard_gamma(D, C, V, Normal, Value).
	standard_gamma(D, C, V, Normal, Value) :-
		V3 is V * V * V,
		random(Uniform),
		(	Uniform =:= 0.0 ->
			standard_gamma(D, C, V, Normal, Value)
		;	Uniform < 1.0 - 0.0331 * (Normal * Normal) * (Normal * Normal) ->
			Value is D * V3
		;	log(Uniform) < 0.5 * Normal * Normal + D * (1.0 - V3 + log(V3)) ->
			Value is D * V3
		;	standard_gamma(D, C, V, Normal, Value)
		).

	standard_normal(Value) :-
		random(X1),
		(	X1 =:= 0.0 ->
			standard_normal(Value)
		;	random(X2),
			Value is sqrt(-2.0 * log(X1)) * cos(2.0*pi*X2)
		).

	fisher(DegreesOfFreedomNumerator, DegreesOfFreedomDenominator, Value) :-
		chi_squared(DegreesOfFreedomNumerator, ChiSquaredNumerator),
		chi_squared(DegreesOfFreedomDenominator, ChiSquaredDenominator),
		Value is (ChiSquaredNumerator / DegreesOfFreedomNumerator) / (ChiSquaredDenominator / DegreesOfFreedomDenominator).

	logseries(Shape, Value) :-
		0.0 < Shape, Shape < 1.0,
		Q is - log(1.0 - Shape),
		logseries(Shape, Q, Value).

	logseries(Shape, Q, Value) :-
		random(Uniform1),
		random(Uniform2),
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
		random(Uniform1),
		random(Uniform2),
		Z is cos(pi * Uniform1),
		F is (1.0 + R * Z) / (R + Z),
		C is Concentration * (R - F),
		(	(Uniform2 < C * (2 - C); Uniform2 =< C * exp(1 - C)) ->
			random(Uniform3),
			(	Uniform3 > 0.5 ->
				Value is Mode + acos(F) - truncate((Mode + acos(F) / (2*pi))) * 2*pi
			;	Value is Mode - acos(F) - truncate((Mode - acos(F) / (2*pi))) * 2*pi
			)
		;	von_mises(Mode, Concentration, S, R, Value)
		).

	gumbel(Location, Scale, Value) :-
		random(Uniform),
		(	Uniform =:= 0.0 ->
			gumbel(Location, Scale, Value)
		;	Value is Location - Scale * log(-log(Uniform))
		).

	dirichlet(Alphas, Thetas) :-
		dirichlet(Alphas, Thetas0, 0, Sum),
		dirichlet(Thetas0, Sum, Thetas).

	dirichlet([], [], Sum, Sum).
	dirichlet([Alpha| Alphas], [Theta0| Thetas0], Sum0, Sum) :-
		standard_gamma(Alpha, Theta0),
		Sum1 is Sum0 + Theta0,
		dirichlet(Alphas, Thetas0, Sum1, Sum).

	dirichlet([], _, []).
	dirichlet([Theta0| Thetas0], Sum, [Theta| Thetas]) :-
		Theta is Theta0 / Sum,
		dirichlet(Thetas0, Sum, Thetas).
