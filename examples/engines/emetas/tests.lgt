%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/06/18,
		comment is 'Unit tests for the "emetas" example.'
	]).

	test(emetas_1) :-
		emetas::best_of(X, (>), a(X)),
		X == 4.

	test(emetas_2) :-
		emetas::find_all(X, a(X), Xs),
		Xs == [2, 1, 4, 3].

	test(emetas_3) :-
		emetas::find_all_reified(X, a(X), Xs),
		Xs == [2, 1, 4, 3].

	test(emetas_4) :-
		emetas::find_at_most(3, X, b(X), Xs),
		Xs == [1, 2, 3].

	test(emetas_5) :-
		emetas::find_at_most(3, X, c(X), Xs),
		Xs = [1, 2].

	% auxiliary predicates

	a(2). a(1). a(4). a(3).

	b(1). b(2). b(3). b(4). b(5).

	c(1). c(2).

:- end_object.
