%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-12-26,
		comment is 'Unit tests for the "defaulty" example.'
	]).

	test(defaulty_01) :-
		defaulty::count_atomics([a,1,_,b,2,_,c,3,_], As, Ns),
		^^assertion(atoms, As == 3),
		^^assertion(numbers, Ns == 3).

	test(defaulty_02) :-
		tagged::count_atomics([a(a),n(1),o(_),a(b),n(2),o(_),a(c),n(3),o(_)], As, Ns),
		^^assertion(atoms, As == 3),
		^^assertion(numbers, Ns == 3).

:- end_object.
