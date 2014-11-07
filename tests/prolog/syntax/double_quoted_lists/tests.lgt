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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog standard double quoted list syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.7.1

	succeeds(iso_double_quoted_list_01) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_02) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_03) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_04) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_05) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_06) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_07) :-
		^^set_text_input('"jim".'),
		set_prolog_flag(double_quotes, chars),
		{read(T)},
		atom_chars('jim', T).

	succeeds(iso_double_quoted_list_08) :-
		^^set_text_input('"jim".'),
		set_prolog_flag(double_quotes, codes),
		{read(T)},
		atom_codes('jim', T).

	succeeds(iso_double_quoted_list_09) :-
		^^set_text_input('"jim".'),
		set_prolog_flag(double_quotes, atom),
		{read(T)},
		T == 'jim'.

	succeeds(iso_double_quoted_list_10) :-
		^^set_text_input('"".'),
		set_prolog_flag(double_quotes, chars),
		{read(T)},
		T == [].

	succeeds(iso_double_quoted_list_11) :-
		^^set_text_input('"".'),
		set_prolog_flag(double_quotes, codes),
		{read(T)},
		T == [].

	succeeds(iso_double_quoted_list_12) :-
		^^set_text_input('"".'),
		set_prolog_flag(double_quotes, atom),
		{read(T)},
		T == ''.

	cleanup :-
		^^clean_text_input.

	double_quotes_example_1([
		'(      current_prolog_flag(double_quotes, chars), atom_chars(\'jim\', "jim")',
		';      current_prolog_flag(double_quotes, codes), atom_codes(\'jim\', "jim")',
		';      current_prolog_flag(double_quotes, atom), \'jim\' == "jim"',
		').'
	]).

	double_quotes_example_2([
		'(      current_prolog_flag(double_quotes, chars), [] == ""',
		';      current_prolog_flag(double_quotes, codes), [] == ""',
		';      current_prolog_flag(double_quotes, atom), \'\' == ""',
		').'
	]).

:- end_object.
