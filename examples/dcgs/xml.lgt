%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
