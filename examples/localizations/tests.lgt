%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
