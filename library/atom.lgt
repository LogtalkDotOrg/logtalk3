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



:- object(atom,
	extends(atomic)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/10/30,
		comment is 'Atom data type predicates.'
	]).

	:- public(replace_sub_atom/4).
	:- mode(replace_sub_atom(+atom, +atom, +atom, ?atom), zero_or_one).
	:- info(replace_sub_atom/4, [
		comment is 'Replaces all occurences of Old by New in Input returning Output. Returns Input if Old is the empty atom. Fails when Output does not unify with the resulting atom.',
		argnames is ['Old', 'New', 'Input', 'Output']
	]).

	replace_sub_atom(Old, New, Input, Output) :-
		atom_length(Old, Length),
		(	Length =:= 0 ->
			Output = Input
		;	replace_sub_atom(Old, Length, New, Input, [Fragment| Fragments]),
			append_fragments(Fragments, Fragment, Output)
		).

	replace_sub_atom(Old, Length, New, Input, Fragments) :-
		(	sub_atom(Input, Before, Length, _, Old) ->
			sub_atom(Input, 0, Before, _, Prefix),
			Skip is Before + Length,
			sub_atom(Input, Skip, _, 0, Suffix),
			atom_concat(Prefix, New, Fragment),
			Fragments = [Fragment| TailFragments],
			replace_sub_atom(Old, Length, New, Suffix, TailFragments)
		;	Fragments = [Input]
		).

	append_fragments([], Output, Output).
	append_fragments([Fragment| Fragments], Previous, Output) :-
		atom_concat(Previous, Fragment, Output0),
		append_fragments(Fragments, Output0, Output).

	valid(Atom) :-
		atom(Atom).

	check(Term) :-
		this(This),
		sender(Sender),
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
