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


:- object(zlist,
	implements(zipperp)).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2019/01/17,
		comment is 'Zipper list predicates. Zippers should be regarded as opaque terms.'
	]).

	:- public(zip/4).
	:- mode(zip(+natural, +list, --zipper, --term), zero_or_one).
	:- info(zip/4, [
		comment is 'Adds a zipper to a list opened at the given index and also returns the element at the index. Fails if the list is empty or the index (starting at 1) does not exist.',
		argnames is ['Index', 'List', 'Zipper', 'Element']
	]).

	zip(Position, List, Zipper, Element) :-
	    zip(Position, List, [], Zipper, Element).

	zip(1, [Head|Tail], Acc, zip(Acc,Head,Tail), Head) :-
		!.
	zip(N, [Head|Tail], Acc, zip(Before,Element,After), Element) :-
	    N > 1,
	    M is N - 1,
	    zip(M, Tail, [Head|Acc], zip(Before,Element,After), Element).

	zip([Head| Tail], zip([],Head,Tail)).

	unzip(zip(Before,Current,After), List) :-
		unzip(Before, Current, After, List).

	unzip([], Current, After, [Current| After]).
	unzip([Element| Before], Current, After, List) :-
		unzip(Before, Element, [Current| After], List).

	current(zip(_, Element, _), Element).

	next(zip(Before,Element,[Head|Tail]), zip([Element|Before],Head,Tail)).

	next(zip(Before,Element,[Head|Tail]), zip([Element|Before],Head,Tail), Head).

	previous(zip([Element|Before],Head,Tail), zip(Before,Element,[Head|Tail])).

	previous(zip([Element|Before],Head,Tail), zip(Before,Element,[Head|Tail]), Element).

	rewind(zip(Before,Current,After), Zipper) :-
		rewind(Before, Current, After, Zipper).

	rewind(zip(Before,Current,After), Zipper, First) :-
		rewind(Before, Current, After, Zipper),
		Zipper = zip(_, First, _).

	rewind([], Head, After, zip([],Head,After)).
	rewind([Element| Before], Current, After, Zipper) :-
		rewind(Before, Element, [Current| After], Zipper).

	forward(zip(Before,Current,After), Zipper) :-
		forward(After, Current, Before, Zipper).

	forward(zip(Before,Current,After), Zipper, Last) :-
		forward(After, Current, Before, Zipper),
		Zipper = zip(_, Last, _).

	forward([], Last, Before, zip(Before,Last,[])).
	forward([Element| After], Current, Before, Zipper) :-
		forward(After, Element, [Current| Before], Zipper).

	:- meta_predicate(apply(1,*)).
	apply(Closure, zip(_,Element,_)) :-
		call(Closure, Element).

	insert_before(zip(Before,Current,After), Element, zip([Element| Before],Current,After)).

	insert_after(zip(Before,Current,After), Element, zip(Before,Current,[Element| After])).

	replace(zip(Before,_,After), Element, zip(Before,Element,After)).

	delete_and_previous(zip([Element|Before],_,Tail), zip(Before,Element,Tail)).

	delete_and_next(zip(Before,_,[Head|Tail]), zip(Before,Head,Tail)).

	delete_and_unzip(zip(Before,_,After), List) :-
		(	After = [Current| Tail] ->
			unzip(Before, Current, Tail, List)
		;	Before = [Current| Tail] ->
			unzip(Before, Current, After, List)
		;	List = []
		).

	delete_all_before(zip(_,Current,After), zip([],Current,After)).

	delete_all_before_and_unzip(zip(_,Current,After), List) :-
		unzip([], Current, After, List).

	delete_all_after(zip(Before,Current,_), zip(Before,Current,[])).

	delete_all_after_and_unzip(zip(Before,Current,_), List):-
		unzip(Before, Current, [], List).

:- end_object.
