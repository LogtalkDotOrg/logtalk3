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


:- object(dumper).

	:- info([
		version is 1:0:0,
		author is 'Michael T. Richter and Paulo Moura',
		date is 2020-02-21,
		comment is 'Simple example of using pengines from objects. Direct port of the pengines documentation example.'
	]).

	% allow using implicit module qualification to call pengines predicates
	:- use_module(pengines, [
		pengine_ask/3,
		pengine_create/1,
		pengine_event_loop/2,
		pengine_next/2
	]).

	% override ambiguous pengines:pengine_create/1 meta-predicate template
	:- meta_predicate(pengines:pengine_create(*)).

	:- public(ask/0).

	ask :-
		pengine_create([
			server('http://localhost:7777'),
			src_text("
				q(X) :- p(X).
				p(a). p(b). p(c).
			")
		]),
		pengine_event_loop(handle, []).

	handle(create(ID, _)) :-
		pengine_ask(ID, q(_X), []).
	handle(success(_ID, [X], false)) :-
		writeq(X), nl.
	handle(success(ID, [X], true)) :-
		writeq(X), nl,
		pengine_next(ID, []).

:- end_object.
