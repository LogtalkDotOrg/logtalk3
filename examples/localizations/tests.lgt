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
		date is 2015/08/23,
		comment is 'Unit tests for the "localizations" example.'
	]).

	% we use a message hook and a private/dynamic predicate to
	% save the tokens generated when printing a message in order
	% to check their correctness using the unit tests

	:- private(message/2).
	:- dynamic(message/2).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(banner, comment, my_game(CountryCode), Tokens) :-
		assertz(message(CountryCode, Tokens)).

	test(localizations_1) :-
		my_game(de)::banner,
		message(de, Tokens),
		Tokens == ['Willkommen Sie bei Mein tolles Spiel!'-[], nl].

	test(localizations_2) :-
		my_game(en)::banner,
		message(en, Tokens),
		Tokens == ['Welcome to my great game!'-[], nl].

	test(localizations_3) :-
		my_game(fr)::banner,
		message(fr, Tokens),
		Tokens == ['Bienvenue sur mon grand jeu!'-[], nl].

	test(localizations_4) :-
		my_game(pt)::banner,
		message(pt, Tokens),
		Tokens == ['Bem vindo ao meu grande jogo!'-[], nl].

	cleanup :-
		retractall(message(_, _)).

:- end_object.
