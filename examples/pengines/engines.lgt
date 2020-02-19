%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2020 Michael T. Richter and Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%	  http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(engines).

	:- info([
		version is 1:0:0,
		author is 'Michael T. Richter and Paulo Moura',
		date is 2020-02-19,
		comment is 'Simple example of using pengines from objects.'
	]).

	:- threaded.

	:- use_module(pengines, [
		pengine_ask/3,
		pengine_create/1,
		pengine_event_loop/2,
		pengine_next/2
	]).

	% override ambiguous pengines:pengine_create/1 meta-predicate template
	:- meta_predicate(pengines:pengine_create(*)).

	:- public(ask/1).

	ask(Engine) :-
		threaded_engine_create(_, ask_server, Engine).

	ask_server :-
		pengine_create([
			server('http://localhost:7777'),
			src_text("
				q(X) :- p(X).
				p(a). p(b). p(c).
			")
		]),
		pengine_event_loop(handle, []),
		% fail when the event loop terminates to avoid a spurious answer
		fail.

	handle(create(ID, _)) :-
		pengine_ask(ID, q(_X), []).
	handle(success(_ID, [X], false)) :-
		threaded_engine_yield(X).
	handle(success(ID, [X], true)) :-
		threaded_engine_yield(X),
		pengine_next(ID, []).

	:- public(answers/2).

	answers(Engine, Answers) :-
		threaded_engine_next_reified(Engine, Reified),
		collect_answers(Reified, Engine, Answers).

	collect_answers(no, Engine, []) :-
		threaded_engine_destroy(Engine).
	collect_answers(the(Answer), Engine, [Answer| Answers]) :-
		threaded_engine_next_reified(Engine, Reified),
		collect_answers(Reified, Engine, Answers).

	:- public(answer/2).

	answer(Engine, Answer) :-
		threaded_engine_next_reified(Engine, Reified),
		(	Reified == no ->
			threaded_engine_destroy(Engine),
			fail
		;	(	Reified = the(Answer)
			;	answer(Engine, Answer)
			)
		).

:- end_object.
