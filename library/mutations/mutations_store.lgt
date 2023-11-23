%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(mutations_store,
	implements(expanding)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-23,
		comment is 'Stores mutation definitions for selected types. User extensible by defining objects or categories defining clauses for the ``mutation/3`` predicate and using this object as a hook object for their compilation.',
		see_also is [type]
	]).

	:- uses(list, [
		length/2, nth1/4
	]).

	:- uses(fast_random, [
		between/3, member/2, permutation/2, random/1, select/3, select/4
	]).

	:- public(mutation/3).
	:- mode(mutation(@callable, @term, -term), one).
	:- info(mutation/3, [
		comment is 'Returns a random mutation of a term into another term of the same type. The input ``Term`` is assumed to be valid for the given ``Type``.',
		argnames is ['Type', 'Term', 'Mutation']
	]).

	:- public(counter/2).
	:- mode(counter(?callable, ?positive_integer), zero_or_more).
	:- info(counter/2, [
		comment is 'Table of the number of mutations available per type.',
		argnames is ['Type', 'N']
	]).

	:- private(mutation/4).
	:- multifile(mutation/4).
	:- mode(mutation(?callable, ?positive_integer, @term, -term), zero_or_more).
	:- info(mutation/4, [
		comment is 'Returns a random mutation of a term into another term of the same type using mutator ``N``. The input ``Term`` is assume to be valid for the given ``Type``.',
		argnames is ['Type', 'N', 'Term', 'Mutation']
	]).

	:- private(counter_/2).
	:- dynamic(counter_/2).
	:- mode(counter_(?callable, ?positive_integer), zero_or_more).
	:- info(counter_/2, [
		comment is 'Internal counter for the number of mutations available for a given type.',
		argnames is ['Type', 'N']
	]).

	mutation(Type, Term, Mutation) :-
		counter_(Type, N),
		repeat,
			between(1, N, Random),
			mutation(Type, Random, Term, Mutation),
		!.

	counter(Type, N) :-
		counter_(Type, N).

	% pre-defined mutations
	counter_(atom, 4).
	counter_(integer, 5).
	counter_(float, 3).
	counter_(list, 5).
	counter_(compound, 2).

	term_expansion((:- Directive), [(:- Directive), (:- multifile(mutations_store::mutation/4))]) :-
		callable(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		).
	term_expansion((mutation(Type, Atom, Mutation) :- Body), (mutations_store::mutation(Type, N, Atom, Mutation) :- Body)) :-
		(	retract(counter_(Type, M)) ->
			N is M + 1
		;	N is 1
		),
		assertz(counter_(Type, N)).
	term_expansion(mutation(Type, Atom, Mutation), mutations_store::mutation(Type, N, Atom, Mutation)) :-
		(	retract(counter_(Type, M)) ->
			N is M + 1
		;	N is 1
		),
		assertz(counter_(Type, N)).

	% deletion of a random character
	mutation(atom, 1, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		select(_Random, Codes, Rest),
		atom_codes(Mutation, Rest).
	% adding a random character
	mutation(atom, 2, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		length(Codes, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		type::arbitrary(code(ascii_identifier), New),
		nth1(Index, MutationCodes, New, Codes),
		atom_codes(Mutation, MutationCodes).
	% replacing a random character
	mutation(atom, 3, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		type::arbitrary(code(ascii_identifier), New),
		select(_Random, Codes, New, MutationCodes),
		atom_codes(Mutation, MutationCodes).
	% permutation of the atom characters
	mutation(atom, 4, Atom, Mutation) :-
		atom_length(Atom, Length),
		Length > 1,
		atom_codes(Atom, Codes),
		permutation(Codes, PermutedCodes),
		atom_codes(Mutation, PermutedCodes),
		Atom \== Mutation.

	% exchange two consecutive digits
	mutation(integer, 1, Integer, Mutation) :-
		Absolute is abs(Integer),
		Absolute > 9,
		Sign is sign(Integer),
		number_chars(Absolute, Chars),
		exchange_two_consecutive_elements(Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% negate integer
	:- if(current_prolog_flag(bounded, true)).
		mutation(integer, 2, Integer, Mutation) :-
			% fail on integer overflow errors
			catch(Mutation is -1 * Integer, _, fail).
	:- else.
		mutation(integer, 2, Integer, Mutation) :-
			Mutation is -1 * Integer.
	:- endif.
	% add random digit
	mutation(integer, 3, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		number_chars(Absolute, Chars),
		between(0'0, 0'9, Code),
		char_code(Digit, Code),
		length(Chars, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		nth1(Index, MutationChars, Digit, Chars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% remove random digit
	mutation(integer, 4, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		Absolute > 9,
		number_chars(Absolute, Chars),
		select(_Random, Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.
	% permutation of the integer digits
	mutation(integer, 5, Integer, Mutation) :-
		Sign is sign(Integer),
		Absolute is abs(Integer),
		Absolute > 9,
		number_chars(Absolute, Chars),
		permutation(Chars, MutationChars),
		number_chars(Mutation0, MutationChars),
		Mutation is Sign * Mutation0.

	% negate float
	mutation(float, 1, Float, Mutation) :-
		% fail on float overflow and underflow errors
		catch(Mutation is -1 * Float, _, fail).
	% move decimal point to the left
	mutation(float, 2, Float, Mutation) :-
		% fail on float overflow and underflow errors
		catch(Mutation is Float / 10, _, fail).
	% move decimal point to the right
	mutation(float, 3, Float, Mutation) :-
		% fail on float overflow and underflow errors
		catch(Mutation is Float * 10, _, fail).

	% mutate a random element
	mutation(list, 1, List, Mutation) :-
		select(Random, List, New, Mutation),
		type(Random, Type),
		mutation(Type, Random, New).
	% drop a random list element
	mutation(list, 2, List, Mutation) :-
		select(_Random, List, Mutation).
	% duplicate a random element
	mutation(list, 3, List, Mutation) :-
		member(Element, List),
		length(List, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		nth1(Index, Mutation, Element, List).
	% exchange two consecutive elements
	mutation(list, 4, List, Mutation) :-
		exchange_two_consecutive_elements(List, Mutation).
	% permutation of the list elements
	mutation(list, 5, List, Mutation) :-
		permutation(List, Mutation).

	% mutate the functor name
	mutation(compound, 1, Compound, Mutation) :-
		Compound =.. [Name| Arguments],
		mutation(atom, Name, NameMutation),
		Mutation =.. [NameMutation| Arguments].
	% mutate the arguments
	mutation(compound, 2, Compound, Mutation) :-
		Compound =.. [Name| Arguments],
		mutation(list, Arguments, ArgumentsMutation),
		Mutation =.. [Name| ArgumentsMutation].

	% auxiliary predicates

	type(Element, atom) :-
		atom(Element),
		!.
	type(Element, integer) :-
		integer(Element),
		!.
	type(Element, float) :-
		float(Element),
		!.
	type([_| _], list) :-
		!.
	type([], list) :-
		!.
	type(Element, compound) :-
		compound(Element),
		!.
	type(Element, var) :-
		var(Element),
		!.
	type(_, term).

	exchange_two_consecutive_elements(List, Mutation) :-
		length(List, Length),
		Limit is Length - 1,
		between(1, Limit, N),
		exchange_two_consecutive_elements(N, List, Mutation).

	exchange_two_consecutive_elements(1, [Element1, Element2| Rest], [Element2, Element1| Rest]) :-
		!.
	exchange_two_consecutive_elements(N, [Head| Tail], [Head| Mutation]) :-
		M is N - 1,
		exchange_two_consecutive_elements(M, Tail, Mutation).

:- end_object.
