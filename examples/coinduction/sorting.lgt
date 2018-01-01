%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- object(sorting).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/03/22,
		comment is 'Experiment with co-inductive sorting of lists.'
	]).

	:- public(keysort/2).

	:- coinductive(sort/3).
	:- coinductive(partition/4).

	keysort(List, Sorted) :-
		sort(List, [], Sorted).

	sort([], Sorted, Sorted).
	sort([Pivot| Rest], Acc, Sorted) :- 
		partition(Rest, Pivot, Smaller0, Bigger0),
		sort(Smaller0, [Pivot| Bigger], Sorted),
		sort(Bigger0, Acc, Bigger).

	partition([], _, [], []).
	partition([Key-Value| Pairs], PivotKey-PivotValue, Smalls, Bigs) :-
		(	Key @< PivotKey ->
			Smalls = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Rest, Bigs)
		;	Key == PivotKey, Value @< PivotValue ->
			Smalls = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Rest, Bigs)
		;	Bigs = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Smalls, Rest)
		).

:- end_object.
