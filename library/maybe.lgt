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


:- object(maybe).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2017/12/18,
		comment is 'Types and predicates for type-checking and handling optionals. Inspired by Haskell.',
		remarks is [
			'Type maybe(Type)' - 'Allows type-checking optional references where the optional term must be of the given type.',
			'QuickCheck support' - 'Defines clauses for the arbitrary::arbitrary/1-2 predicates to allow generating random values for the maybe(Type) type.'
		],
		see_also is [optional, optional(_), type, arbitrary]
	]).

	:- public(cat/2).
	:- mode(cat(+list(optional), -list), one).
	:- info(cat/2, [
		comment is 'Returns the values stored in the non-empty optionals.',
		argnames is ['Optionals', 'Values']
	]).

	:- multifile(type::type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.

	type::type(maybe(_)).

	:- multifile(type::check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.

	type::check(maybe(Type), Term) :-
		type::check(optional, Term),
		optional(Term)::if_present(type::check(Type)).

	:- multifile(arbitrary::arbitrary/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(arbitrary::arbitrary/1).
	:- endif.

	arbitrary::arbitrary(maybe(_)).

	:- multifile(arbitrary::arbitrary/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(arbitrary::arbitrary/2).
	:- endif.

	arbitrary::arbitrary(maybe(Type), Arbitrary) :-
		random::random(Random),
		(	Random < 0.5 ->
			optional::empty(Arbitrary)
		;	type::arbitrary(Type, Term),
			optional::of(Term, Arbitrary)	
		).

	cat([], []).
	cat([Optional| Optionals], Values) :-
		(	optional(Optional)::or_else_fail(Value) ->
			Values = [Value| RestValues]
		;	Values = RestValues
		),
		cat(Optionals, RestValues).

:- end_object.
