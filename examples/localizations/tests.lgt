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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/04/05,
		comment is 'Unit tests for the "localizations" example.'
	]).

	cover(my_game(_)).
	cover(my_game_de_localization).
	cover(my_game_en_localization).
	cover(my_game_fr_localization).
	cover(my_game_pt_localization).

	test(localizations_1) :-
		^^set_text_output(''),
		my_game(de)::banner,
		^^check_text_output('>>> Willkommen Sie bei Mein tolles Spiel!\n').

	test(localizations_2) :-
		^^set_text_output(''),
		my_game(en)::banner,
		^^check_text_output('>>> Welcome to my great game!\n').

	test(localizations_3) :-
		^^set_text_output(''),
		my_game(fr)::banner,
		^^check_text_output('>>> Bienvenue sur mon grand jeu!\n').

	test(localizations_4) :-
		^^set_text_output(''),
		my_game(pt)::banner,
		^^check_text_output('>>> Bem vindo ao meu grande jogo!\n').

	cleanup :-
		^^clean_text_output.

:- end_object.
