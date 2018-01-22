%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- object(object,
	imports(category)).

	:- info([
		version is 4.1,
		author is 'Paulo Moura',
		date is 2013/04/19,
		comment is 'Example object for benchmarking library predicate calls and imported category predicate calls.'
	]).

	:- public(append/3).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	:- public(nrev/2).

	nrev([], []).
	nrev([Head| Tail], Reversed) :-
		nrev(Tail, ReversedTail),
		append(ReversedTail, [Head], Reversed).

	:- public(length/2).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	:- public(ctg_self/0).
	% call an imported category predicate by sending a message to self;
	% performance will depend on the distance between "self" and "this"
	% (always uses dynamic binding)
	ctg_self :-
		::ctg_pred.

	:- public(ctg_super/0).
	% call an imported category predicate by using the ^^/1 control construct;
	% (static binding may be used, depending on how the category is compiled)
	ctg_super :-
		^^ctg_pred.

	:- public(obj_local/0).
	% call a local object predicate directly; used for comparing performance with
	% calls to category predicates using the ::/1 and ^^/1 control constructs
	obj_local :-
		{generate_list(20, List)},
		length(List, _).

:- end_object.


:- object(descendant,
	extends(object)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Example object used for simulating a small hierarchy.'
	]).

:- end_object.


:- object(leaf,
	extends(descendant)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Example object used for simulating a small hierarchy.'
	]).

:- end_object.
