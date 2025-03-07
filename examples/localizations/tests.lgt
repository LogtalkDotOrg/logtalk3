%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:4:3,
		author is 'Paulo Moura',
		date is 2021-07-26,
		comment is 'Unit tests for the "localizations" example.'
	]).

	cover(my_game(_)).
	cover(my_game_de_localization).
	cover(my_game_en_localization).
	cover(my_game_fr_localization).
	cover(my_game_pt_localization).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

	test(localizations_1, true(Assertion)) :-
		^^set_text_output(''),
		my_game(de)::banner,
		^^text_output_assertion('>>> Willkommen Sie bei Mein tolles Spiel!\r\n', Assertion).

	test(localizations_2, true(Assertion)) :-
		^^set_text_output(''),
		my_game(en)::banner,
		^^text_output_assertion('>>> Welcome to my great game!\r\n', Assertion).

	test(localizations_3, true(Assertion)) :-
		^^set_text_output(''),
		my_game(fr)::banner,
		^^text_output_assertion('>>> Bienvenue sur mon grand jeu!\r\n', Assertion).

	test(localizations_4, true(Assertion)) :-
		^^set_text_output(''),
		my_game(pt)::banner,
		^^text_output_assertion('>>> Bem vindo ao meu grande jogo!\r\n', Assertion).

	:- else.

	test(localizations_1, true(Assertion)) :-
		^^set_text_output(''),
		my_game(de)::banner,
		^^text_output_assertion('>>> Willkommen Sie bei Mein tolles Spiel!\n', Assertion).

	test(localizations_2, true(Assertion)) :-
		^^set_text_output(''),
		my_game(en)::banner,
		^^text_output_assertion('>>> Welcome to my great game!\n', Assertion).

	test(localizations_3, true(Assertion)) :-
		^^set_text_output(''),
		my_game(fr)::banner,
		^^text_output_assertion('>>> Bienvenue sur mon grand jeu!\n', Assertion).

	test(localizations_4, true(Assertion)) :-
		^^set_text_output(''),
		my_game(pt)::banner,
		^^text_output_assertion('>>> Bem vindo ao meu grande jogo!\n', Assertion).

	:- endif.

	cleanup :-
		^^clean_text_output.

:- end_object.
