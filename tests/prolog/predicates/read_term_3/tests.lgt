%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/12,
		comment is 'Unit tests for the ISO Prolog standard read_term/3, read_term/2, read/2, and read/1 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.1.4

	succeeds(iso_read_term_3_01) :-
		^^set_text_input('term1. term2. ...'),
		{read(T)},
		T == term1,
		^^check_text_input(' term2. ...').

	succeeds(iso_read_term_3_02) :-
		^^set_text_input(st_o, 'term1. term2. ...'),
		{read(st_o, term1)},
		^^check_text_input(st_o, ' term2. ...').

	succeeds(iso_read_term_3_03) :-
		^^set_text_input(st_o, ['foo(A+Roger,A+_). ','term2. ...']),
		{read_term(st_o, T, [variables(VL),variable_names(VN),singletons(VS)])},
		T = foo(X1+X2,X1+X3), VL = [X1,X2,X3], VN = ['A'=X1,'Roger'=X2], VS = ['Roger'=X2],
		^^check_text_input(st_o, ' term2. ...').

	succeeds(iso_read_term_3_04) :-
		^^set_text_input('3.1.  term2. ...'),
		\+ {read(4.1)},
		^^check_text_input('  term2. ...').

	succeeds(iso_read_term_3_05) :-
		^^set_text_input('foo 123. term2. ...'),
		catch({read(_T)}, error(syntax_error(_),_), true),
		^^check_text_input(' term2. ...').

	succeeds(iso_read_term_3_06) :-
		^^set_text_input('3.1'),
		catch({read(_T)}, error(syntax_error(_),_), true).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(iso_read_term_3_07) :-
		^^set_text_input('foo( bar). '),
		{read_term(T, [singletons(S)])},
		T == foo(bar), S == [].

	throws(sics_read_term_3_08, error(instantiation_error,_)) :-
		{read(_, _)}.

	throws(sics_read_term_3_09, error(instantiation_error,_)) :-
		{read_term(user_input, _, _)}.

	throws(sics_read_term_3_10, error(instantiation_error,_)) :-
		% some Prolog systems simply ignore non-recognized read options: provide a term
		% to be read in that case so that the unit test doesn't hang waiting for input
		^^set_text_input('a. '),
		current_input(S),
		{read_term(S, _, [variables(_)|_])}.

	throws(sics_read_term_3_11, error(instantiation_error,_)) :-
		{read_term(user_input,_,[variables(_),_])}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(sics_read_term_3_12, error(domain_error(stream_or_alias,foo),_)) :-
			{read(foo, _)}.
	:- else.
		throws(sics_read_term_3_12, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
			% the second exception term is a common but not conforming alternative
			{read(foo, _)}.
	:- endif.

	throws(sics_read_term_3_13, error(type_error(list,bar),_)) :-
		{read_term(user_input, _, bar)}.

	throws(sics_read_term_3_14, error(domain_error(read_option,bar),_)) :-
		% some Prolog systems simply ignore non-recognized read options: provide a term
		% to be read in that case so that the unit test doesn't hang waiting for input
		^^set_text_input('a. '),
		current_input(S),
		{read_term(S, _, [bar])}.

	throws(sics_read_term_3_15, error(permission_error(input,stream,user_output),_)) :-
		{read_term(user_output, _, [])}.

	succeeds(sics_read_term_3_16) :-
		^^set_text_input(''),
		{read(T)},
		T == end_of_file,
		current_input(Stream),
		stream_property(Stream, end_of_stream(past)).

	throws(sics_read_term_3_17, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{read_term(S, _, [])}.

	throws(sics_read_term_3_18, error(permission_error(input,binary_stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{read_term(_, [])}.

	throws(sics_read_term_3_19, error(permission_error(input,binary_stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{read(_)}.

	succeeds(sics_read_term_3_20) :-
		^^set_text_input(st_o, '', [eof_action(error)]),
		get_code(st_o, _),
		catch({read_term(st_o, _, [])}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_o)),
		stream_property(S, end_of_stream(past)).

	succeeds(sics_read_term_3_21) :-
		^^set_text_input('\'a.'),
		catch({read_term(_,[])}, error(syntax_error(_),_), true),
		^^check_text_input('').

	succeeds(sics_read_term_3_22) :-
		max_min_integer_as_atom(max_integer, Integer, Atom),
		^^set_text_input([Atom, '. ']),
		{read(X)},
		X == Integer.
		
	succeeds(sics_read_term_3_23) :-
		max_min_integer_as_atom(min_integer, Integer, Atom),
		^^set_text_input([Atom, '. ']),
		{read(X)},
		X == Integer.

	succeeds(lgt_read_term_3_24) :-
		^^set_text_input(st_o, '', [eof_action(eof_code)]),
		get_code(st_o, _),
		{read_term(st_o, Term, [])},
		Term == end_of_file.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input.

	max_min_integer_as_atom(Flag, Value, Atom) :-
		(	current_prolog_flag(bounded, true) ->
			current_prolog_flag(Flag, Value),
			number_codes(Value, Codes),
			atom_codes(Atom, Codes)
		;	Value = 0,
			Atom = '0'
		).

:- end_object.
