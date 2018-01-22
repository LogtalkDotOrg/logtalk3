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


:- object(fibonacci).

	:- info([
		version is 1.0,
		date is 2010/12/19,
		author is 'Paulo Moura',
		comment is 'Computation of Fibonacci numbers using a fold left meta-predicate.'
	]).

	:- public(nth/2).
	:- mode(nth(+integer, -integer), one).
	:- info(nth/2, [
		comment is 'Calculates the Nth Fibonacci number.',
		argnames is ['Nth', 'Number']
	]).

	nth(N, F) :-
		meta::fold_left(next, 0-[0,1], _, N-[F, _]).

	next(N1-[F1, F2], _, N2-[F2, F3]) :-
		F3 is F1 + F2,
		N2 is N1 + 1.

:- end_object.
