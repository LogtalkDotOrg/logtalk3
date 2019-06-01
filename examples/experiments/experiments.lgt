%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(experiments).

	:- public(stats/3).

	% editing the random object in the following directive avoids
	% having to change all explicit message sending calls to it as
	% they are written using the object alias
	:- uses([
		fast_random as rnd
%		backend_random as rnd
%		random as rnd
	]).

	stats(TotalLess, TotalEqual, TotalGreater) :-
		% generate a set of 42 random integers in the [1,1000] interval
		rnd::set(42, 1, 1000, Set),
		% partition the set using the middle value
		meta::partition(compare, Set, 500, Less, Equal, Greater),
		% compute stats to evaluate the quality of the used random generator
		list::length(Less, TotalLess),
		list::length(Equal, TotalEqual),
		list::length(Greater, TotalGreater).

:- end_object.
