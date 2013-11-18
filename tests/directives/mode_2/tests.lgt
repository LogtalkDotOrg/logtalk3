%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(source_data, on).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/11/18,
		comment is 'Unit tests for the mode/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo/4).
	:- mode(foo(+, -, ?, @), zero).

	test(mode_2_1) :-
		predicate_property(foo(_,_,_,_), mode(Template, Solutions)),
		Template == foo(+, -, ?, @),
		Solutions == zero.

	:- public(bar/4).
	:- mode(bar(+integer, -float, ?atom, @list), zero_or_one).

	test(mode_2_2) :-
		predicate_property(bar(_,_,_,_), mode(Template, Solutions)),
		Template == bar(+integer, -float, ?atom, @list),
		Solutions == zero_or_one.

	:- public(baz/1).
	:- mode(baz(@list(atom)), one).

	test(mode_2_3) :-
		predicate_property(baz(_), mode(Template, Solutions)),
		Template == baz(@list(atom)),
		Solutions == one.

	:- public(qux/1).
	:- mode(qux(-positive_integer), zero_or_more).

	test(mode_2_4) :-
		predicate_property(qux(_), mode(Template, Solutions)),
		Template == qux(-positive_integer),
		Solutions == zero_or_more.

	:- public(quux/1).
	:- mode(quux(+string), one_or_more).

	test(mode_2_5) :-
		predicate_property(quux(_), mode(Template, Solutions)),
		Template == quux(+string),
		Solutions == one_or_more.

:- end_object.
