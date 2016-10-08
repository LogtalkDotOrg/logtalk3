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


:- object(xml).

	:- info([
		version is 1.01,
		date is 2012/04/16,
		author is 'Paulo Moura',
		comment is 'Conversion between compound terms and XML.'
	]).

	:- public(convert/3).
	:- mode(convert(@compound, @compound, -atom), zero_or_one).
	:- mode(convert(-compound, -compound, +atom), zero_or_one).
	:- info(convert/3, [
		comment is 'Converts between a compound term and an interpretation and XML.',
		argnames is ['Term', 'Interpretation', 'XML']
	]).

	convert(Term, Interpretation, XML) :-
		var(XML) ->
			phrase(term(Term, Interpretation), List),
			atom_codes(XML, List)
			;
			atom_codes(XML, List),
			phrase(term(Term, Interpretation), List).

	term(Term, Interpretation) -->
		{nonvar(Term), nonvar(Interpretation),
		 Interpretation =.. [Functor| Tags], Term =.. [Functor| Args]},
		open_tag(Functor),
		arguments(Tags, Args),
		close_tag(Functor).
	term(Term, Interpretation) -->
		{var(Term), var(Interpretation)},
		open_tag(Functor),
		arguments(Tags, Args),
		close_tag(Functor),
		{Interpretation =.. [Functor| Tags], Term =.. [Functor| Args]}.

	arguments([], []) -->
		[].
	arguments([Tag| Tags], [Arg| Args]) -->
		open_tag(Tag),
		value(Arg),
		close_tag(Tag),
		arguments(Tags, Args).

	open_tag(Tag) -->
		"<", value(Tag), ">".

	close_tag(Tag) -->
		"</", value(Tag), ">".

	value(Value) -->
		{nonvar(Value), atom_codes(Value, Codes)}, characters(Codes).
	value(Value) -->
		{var(Value)}, characters(Codes), {atom_codes(Value, Codes)}.

	characters([]) --> [].
	characters([Code| Codes]) --> [Code], {character(Code)}, characters(Codes).

	character(Code) :- Code >= 0'a, Code =< 0'z, !.
	character(Code) :- Code >= 0'A, Code =< 0'Z.

:- end_object.
