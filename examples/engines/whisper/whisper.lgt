%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(whisper).

	:- info([
		version is 1.1,
		author is 'Jan Wielemaker. Adapted to Logtalk by Paulo Moura',
		date is 2017/08/26,
		comment is 'Example of chaining threaded engines.'
	]).

	:- threaded.

	:- public(whisper/3).
	:- mode(whisper(+non_negative_integer, +integer, -integer), one).
	:- info(whisper/3, [
		comment is 'Create a chain of N engines that whisper a term from the first to the second, ... up to the end.',
		argnames is ['N', 'From', 'Final']
	]).

	whisper(N, From, Final) :-
		threaded_engine_create(_, final, Last),
		whisper_list(N, Last, First),
		threaded_engine_post(First, From),
		threaded_engine_next(Last, Final).

	whisper_list(0, First, First) :-
		!.
	whisper_list(N, Next, First) :-
		threaded_engine_create(_, add1_and_tell(Next), Me),
		N1 is N - 1,
		whisper_list(N1, Me, First).

	final :-
		threaded_engine_fetch(X),
		{format('~w\n', [X])},
		threaded_engine_yield(X).

	add1_and_tell(Next) :-
		threaded_engine_fetch(X),
		X2 is X + 1,
		{format('Sending ~d to engine ~p\n', [X2, Next])},
		threaded_engine_post(Next, X2).

:- end_object.
