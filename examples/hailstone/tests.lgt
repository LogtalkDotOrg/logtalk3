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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "hailstone" example.'
	]).

	cover(hailstone).

	test(hailstone_1) :-
		hailstone::generate_sequence(10, Sequence),
		Sequence == [10, 5, 16, 8, 4, 2, 1].

	test(hailstone_2) :-
		hailstone::write_sequence(10).

	test(hailstone_3) :-
		hailstone::sequence_length(27, Length),
		Length == 112.

	test(hailstone_4) :-
		hailstone::longest_sequence(1, 1000, N, Length),
		N == 871,
		Length == 179.

:- end_object.
