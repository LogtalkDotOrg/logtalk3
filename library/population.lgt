%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(population,
	imports(statistics)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2013/04/19,
		comment is 'Statistical population represented as a list of numbers.'
	]).

	skewness([X| Xs], Skewness) :-
		^^arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Cube is (X - Mean) ** 3,
		^^squares_and_cubes(Xs, Mean, Square, Squares, Cube, Cubes),
		Skew is (Cubes / N) / (Squares / N) ** 1.5,
		Skewness is sqrt(N*(N - 1)) * Skew / (N - 2).

	kurtosis([X| Xs], Kurtosis) :-
		^^arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Hyper is (X - Mean) ** 4,
		^^squares_and_hypers(Xs, Mean, Square, _, Hyper, Hypers),
		^^variance(Xs, 1, N, X, 0, M2),
		K2 is M2 / (N - 1),
		Kurtosis is N * (N + 1) * Hypers / ((N - 1) * (N - 2) * (N - 3) * K2**2) - 3 * (N - 1)**2/((N - 2) * (N - 3)).

	standard_deviation([X| Xs], Deviation) :-
		^^variance(Xs, 1, N, X, 0, M2),
		Deviation is sqrt(M2 / N).

	variance([X| Xs], Variance) :-
		^^variance(Xs, 1, N, X, 0, M2),
		Variance is M2 / N.

:- end_object.
