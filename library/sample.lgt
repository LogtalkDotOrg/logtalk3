%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(sample,
	imports(statistics)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2013/04/19,
		comment is 'Statistical sample represented as a list of numbers.',
		see_also is [population]
	]).

	skewness([X| Xs], Skewness) :-
		^^arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Cube is (X - Mean) ** 3,
		^^squares_and_cubes(Xs, Mean, Square, Squares, Cube, Cubes),
		Skewness is (Cubes / N) / (Squares / (N-1)) ** 1.5.

	kurtosis([X| Xs], Kurtosis) :-
		^^arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Hyper is (X - Mean) ** 4,
		^^squares_and_hypers(Xs, Mean, Square, Squares, Hyper, Hypers),
		Kurtosis is (Hypers / N) / (Squares / (N-1)) ** 2 - 3.

	standard_deviation([X| Xs], Deviation) :-
		^^variance(Xs, 1, N, X, 0, M2),
		Deviation is sqrt(M2 / (N - 1)).

	variance([X| Xs], Variance) :-
		^^variance(Xs, 1, N, X, 0, M2),
		Variance is M2 / (N - 1).

:- end_object.
