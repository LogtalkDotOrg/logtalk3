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
		date is 2016/10/10,
		comment is 'Unit tests for the "quick_check" example.'
	]).

	:- uses(list, [
		reverse/2, same_length/2, same_length/3
	]).

	% tests

	% run the default number of random tests
	quick_check(qc1, reverse_2_twice_prop(+list)).
	quick_check(qc2, same_length_2_prop(+list)).
	% run 25 random tests
	quick_check(qc3, same_length_3_prop(+list), [n(25)]).

	% properties (must be defined as local predicates but can, of course,
	% call any predicate defined elsewhere)

	% reversing a list twice must give the original list
	reverse_2_twice_prop(List) :-
		reverse(List, Reverse),
		reverse(Reverse, ReverseReverse),
		List == ReverseReverse.

	% same_length/2 must be true when using the same list for both arguments
	same_length_2_prop(List) :-
		same_length(List, List).

	% the length of two lists of the same length must be an non-negative integer
	same_length_3_prop(List) :-
		same_length(List, _, Length),
		Length >= 0.

:- end_object.
