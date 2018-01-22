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


:- object(atom,
	extends(atomic)).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2017/10/30,
		comment is 'Atom data type predicates.'
	]).

	:- public(replace_sub_atom/4).
	:- mode(replace_sub_atom(+atom, +atom, +atom, -atom), one).
	:- info(replace_sub_atom/4, [
		comment is 'Replaces all occurences of Old by New in Input returning Output. Returns Input if Old is the empty atom. Fails when Output does not unify with the resulting atom.',
		argnames is ['Old', 'New', 'Input', 'Output']
	]).

	:- public(split/3).
	:- mode(split(+atom, +atom, -list(atom)), one).
	:- info(split/3, [
		comment is 'Splits an atom at a given delimiter into a list of sub-atoms.',
		argnames is ['Atom', 'Delimiter', 'SubAtoms']
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

	split(Atom, Delimiter, SubAtoms) :-
		atom_length(Delimiter, Length),
		split(Atom, Delimiter, Length, SubAtoms).

	split(Atom, Delimiter, Length, [SubAtom| SubAtoms]) :-
		sub_atom(Atom, Before, Length, _, Delimiter),
		sub_atom(Atom, 0, Before, _, SubAtom),
		!,
		Skip is Before + Length,
		sub_atom(Atom, Skip, _, 0, Suffix),
		split(Suffix, Delimiter, Length, SubAtoms).
	split(Atom, _, _, [Atom]) :-
		% ensure type_error(atom, Atom) if Atom is not an atom
		sub_atom(Atom, 0, _, 0, Atom).

	valid(Atom) :-
		atom(Atom).

	check(Term) :-
		context(Context),
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, Context))
		;	throw(error(type_error(atom, Term), Context))
		).

:- end_object.
