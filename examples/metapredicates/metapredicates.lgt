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


% example adapted from:
% Programming Language Prolog Part 2, Modules
% Committee Draft - January 14, 1998 X3J17/97/5

:- object(tracer).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2006/9/17,
		comment is 'Tracer for a goal call, exit, and fail ports.'
	]).

	:- public(trace/1).
	:- meta_predicate(trace(0)).	% changes interpretation of meta-calls on trace/1 clauses
	:- mode(trace(+callable), zero_or_more).
	:- info(trace/1, [
		comment is 'Traces goal execution.',
		argnames is ['Goal']
	]).

	trace(Goal) :-
		write('call: '), writeq(Goal), nl,
		call(Goal),		% Goal is called in the context of the object sending the message trace/1
		write('exit: '), writeq(Goal), nl.

	trace(Goal) :-
		write('fail: '), writeq(Goal), nl,
		fail.

:- end_object.


% sort code adapted from an example on the SICStus Prolog User Manual
% meta-predicate example taken from Prolog Part 2, Modules - Committee Draft

:- object(sort(_Type)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2013/04/23,
		comment is 'List sorting parameterized by the type of the list elements.'
	]).

	:- uses(list, [append/3]).		% calls to append(...) will be translated to list::append(...)
	:- uses(tracer, [trace/1]).		% calls to trace(...) will be translated to tracer::trace(...)

	:- public(sort/2).
	:- mode(sort(+list, -list), one).
	:- info(sort/2, [
		comment is 'Sorts a list in ascending order (quicksort algorithm).',
		argnames is ['List', 'Sorted']
	]).

	:- private(partition/4).
	:- mode(partition(+list, +nonvar, -list, -list), one).
	:- info(partition/4, [
		comment is 'Partition a list in two lists containing the elements smaller and larger than a pivot.',
		argnames is ['List', 'Pivot', 'Small', 'Large']
	]).

	sort([], []).

	sort([Head| Tail], Sorted) :-
		trace(partition(Tail, Head, Small, Large)),
		trace(sort(Small, Sorted1)),
		trace(sort(Large, Sorted2)),
		append(Sorted1, [Head| Sorted2], Sorted).

	partition([], _, [], []).

	partition([Head| Tail], Pivot, Small, Large) :-
		parameter(1, Type),
		(	Type :: (Head < Pivot) ->
			Small = [Head| Small1], Large = Large1
		;	Small = Small1, Large = [Head| Large1]
		),
		partition(Tail, Pivot, Small1, Large1).

:- end_object.
