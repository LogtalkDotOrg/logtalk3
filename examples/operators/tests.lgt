%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/11/26,
		comment is 'Unit tests for the "operators" example.'
	]).

	cover(double).
	cover(triple).
	cover(graph).
	cover(graph1).
	%unit(graph2).
	cover(reverse).

	test(operators_1) :-
		findall(I-J, double::double(I, J), Solutions),
		Solutions == [1-2, 2-4, 3-6].

	test(operators_2) :-
		triple::read_from_file,
		findall(I-J, triple::triple(I, J), Solutions),
		Solutions == [1-3, 2-6, 3-9].

	test(operators_3) :-
		findall(N1-N2, graph1::edge(N1, N2), Solutions),
		Solutions == [a-b, a-c, b-d, c-d].

	test(operators_4) :-
		findall(Path, graph1::path(a, d, Path), Solutions),
		Solutions == [[a,b,d], [a,c,d]].

	test(operators_5) :-
		\+ current_op(_P, _T, edge).

	test(operators_6) :-
		reverse::reverse_file.

:- end_object.
