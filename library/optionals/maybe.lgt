%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(maybe).

	:- info([
		version is 0:9:0,
		author is 'Paulo Moura',
		date is 2025-06-19,
		comment is 'Types and predicates for type-checking and handling optional terms. Inspired by Haskell.',
		remarks is [
			'Type-checking support' - 'Defines type ``maybe(Type)`` for checking optional terms where the value hold by the optional term must be of the given type.',
			'QuickCheck support' - 'Defines clauses for the ``arbitrary::arbitrary/1-2``, ``arbitrary::shrinker/1``, ``arbitrary::shrink/3``, and ``arbitrary::edge_case/2`` predicates to allow generating random values for the ``maybe(Type)`` type.'
		],
		see_also is [optional, optional(_), type, arbitrary]
	]).

	:- public(cat/2).
	:- mode(cat(+list(optional), -list), one).
	:- info(cat/2, [
		comment is 'Returns the values stored in the non-empty optional terms.',
		argnames is ['Optionals', 'Values']
	]).

	:- public(sequence/2).
	:- mode(sequence(+list(optional), --nonvar), one).
	:- info(sequence/2, [
		comment is 'Returns an optional term with a list of all values when all optional terms are not empty. Otherwise returns an empty optional term.',
		argnames is ['Optionals', 'Optional']
	]).

	:- public(traverse/3).
	:- meta_predicate(traverse(2, *, *)).
	:- mode(traverse(+callable, +list, --nonvar), one).
	:- info(traverse/3, [
		comment is 'Applies a closure to each list element to generate optional terms and then sequences them into a single optional term holding all values or an empty optional term.',
		argnames is ['Closure', 'Terms', 'Optional']
	]).

	:- multifile(type::type/1).
	type::type(maybe(_)).

	:- multifile(type::check/2).
	type::check(maybe(Type), Term) :-
		type::check(optional, Term),
		optional(Term)::if_present(type::check(Type)).

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(maybe(_)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(maybe(Type), Arbitrary) :-
		(	random::maybe ->
			optional::empty(Arbitrary)
		;	type::arbitrary(Type, Term),
			optional::of(Term, Arbitrary)
		).

	:- multifile(arbitrary::shrinker/1).
	arbitrary::shrinker(maybe(_)).

	:- multifile(arbitrary::shrink/3).
	arbitrary::shrink(maybe(Type), optional(Large), optional(Small)) :-
		type::shrink(Type, Large, Small).

	:- multifile(arbitrary::edge_case/2).
	arbitrary::edge_case(maybe(_Type), empty).
	arbitrary::edge_case(maybe(Type), optional(Term)) :-
		type::edge_case(Type, Term).

	cat([], []).
	cat([Optional| Optionals], Values) :-
		(	optional(Optional)::or_else_fail(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		cat(Optionals, Rest).

	sequence([], optional([])).
	sequence([Optional| Optionals], Sequenced) :-
		( 	Optional = optional(Value) ->
			sequence(Optionals, RestSequenced),
			( 	RestSequenced = optional(RestValues) ->
				Sequenced = optional([Value| RestValues])
			; 	Sequenced = empty
			)
		; 	Optional == empty,
			Sequenced = empty
		).

	traverse(Closure, Terms, Sequenced) :-
		traverse_(Terms, Closure, Optionals),
		sequence(Optionals, Sequenced).

	:- meta_predicate(traverse_(*, 2, *)).
	traverse_([], _, []).
	traverse_([Term| Terms], Closure, [Optional| Optionals]) :-
		call(Closure, Term, Optional),
		traverse_(Terms, Closure, Optionals).

:- end_object.
