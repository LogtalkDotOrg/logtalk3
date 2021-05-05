%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(hook_multi_pass(_Hooks_),
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-03-20,
		comment is '.',
		parameters is ['Hooks'-'List of hook objects'],
		remarks is [
			'Usage' - 'Compile source files that should be expanded using the pipeline of hook objects using the compiler option ``hook(hook_multi_pass(Hooks))``.'
		],
		see_also is [hook_set(_), hook_pipeline(_)]
	]).

	:- private(position_pass_/2).
	:- dynamic(position_pass_/2).
	position_pass_(_, 0).

	term_expansion(begin_of_file, Expansion) :-
		retract(position_pass_(_, Pass0)),
		Pass is Pass0 + 1,
		nth(_Hooks_, 1, Pass, Hook),
		logtalk_load_context(stream, Stream),
		stream_property(Stream, position(Position)),
		assertz(position_pass_(Position, Pass)),
		Hook::expand_term(begin_of_file, Expansion).
	term_expansion(mark, mark) :-
		retract(position_pass_(Position, Pass0)),
		Pass is Pass0 + 1,
		nth(_Hooks_, 1, Pass, _),
		!,
		logtalk_load_context(stream, Stream),
		set_stream_position(Stream, Position),
		assertz(position_pass_(Position, Pass)).
	term_expansion(mark, []).
	term_expansion(Term, Expansion) :-
		position_pass_(_, Pass),
		nth(_Hooks_, 1, Pass, Hook),
		!,
		Hook::expand_term(Term, Expansion).
	term_expansion(end_of_file, _) :-
		retractall(position_pass_(_, _)),
		assertz(position_pass_(_, 0)),
		fail.

	% auxiliary predicates

	nth([Element| _], Position, Position, Element) :-
		!.
	nth([_| List], Position0, Position, Element) :-
		Position1 is Position0 + 1,
		nth(List, Position1, Position, Element).

:- end_object.
