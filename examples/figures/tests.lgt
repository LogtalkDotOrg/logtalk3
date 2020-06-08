%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-06-08,
		comment is 'Unit tests for the "figures" example.'
	]).

	cover(figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).

	test(figures_01, true(Name == parallelogram)) :-
		figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
			edge(1, 2,  0, 20),
			edge(2, 3, 45, 30),
			edge(3, 4,  0, 20),
			edge(4, 1, 45, 30),
			class(Name)
		).

	test(figures_02, true(Name == rhombus)) :-
		figures(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
			edge(a, b, 45, 10),
			edge(b, c,  0, 10),
			edge(c, d, 45, 10),
			edge(d, a,  0, 10),
			class(Name)
		).

	test(figures_split_01, true(Name == square)) :-
		figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
			edge(1, 2,  0, 20),
			edge(2, 3, 90, 20),
			edge(3, 4,  0, 20),
			edge(4, 1, 90, 20),
			perpendicularity,
			class(Name)
		).

	test(figures_split_02, true(Name == rhombus)) :-
		figures_split(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)::(
			edge(a, b, 45, 10),
			edge(b, c,  0, 10),
			edge(c, d, 45, 10),
			edge(d, a,  0, 10),
			perpendicularity,
			class(Name)
		).

:- end_object.
