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


% reversing a list twice must give the original list
reverse_2_twice_prop(List) :-
	list::reverse(List, Reverse),
	list::reverse(Reverse, ReverseReverse),
	List == ReverseReverse.

% same_length/2 must be true when using the same list for both arguments
same_length_2_prop(List) :-
	list::same_length(List, List).

% the length of two lists of the same length must be an non-negative integer
same_length_3_prop(List) :-
	list::same_length(List, _, Length),
	Length >= 0.

% a broken property just to check the return of a counter-example that falsifies it
broken_nth1_3_prop(List) :-
	list::nth1(Position, List, _),
	list::length(List, Length),
	Position > Length.
