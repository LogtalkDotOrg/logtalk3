%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(hags,
	imports(assumptions)).

	:- info([
		version is 0:0:0,
		author is 'Orginal example by Paul Tarau et al. Adapted to Logtalk by Paulo Moura.',
		date is 2014-06-26,
		comment is 'Implementation of Hidden Accumulator Grammars (HAGs) using linear assumptions.'
	]).

	:- public(go/0).
	go :-
		l_phrase(axiom, Xs),
		write(Xs), nl,
		fail.
	go.

	% example
	axiom :- ng, v.
	ng :- a, n.
	a :- 'C'(the).      a :- 'C'(a).
	n :- 'C'(cat).      n :- 'C'(dog).
	v :- 'C'(walks).    v :- 'C'(sleeps).

	% creates and initializes a named `DCG' stream
	l_def(Name, S0, S) :- l_dcg(Name, _, _), !, ^^assumel(l_dcg(Name,S0,S)).
	l_def(Name, S0, S) :- ^^assumel(l_dcg(Name,S0,S)).

	% unifies with the current state of a named `DCG' stream
	l_val(Name, S0, S) :- l_dcg(Name, S0, S), ^^assumel(l_dcg(Name,S0,S)).

	% equivalent of the `C'/3 step in Prolog
	l_connect(Name, S0, X, S) :- l_dcg(Name, S0, S), S0 = [X| S1], ^^assumel(l_dcg(Name,S1,S)).

	% equivalent of phrase/3 in Prolog
	l_phrase(Name, Axiom, S0, S) :- l_def(Name, S0, S), call(Axiom), l_val(Name, S0, S).

	% file I/O inspired metaphors for switching between streams
	l_tell(Name) :- l_name(_), !, ^^assumel(l_name(Name)).
	l_tell(Name) :- ^^assumel(l_name(Name)).

	l_telling(Name) :- l_name(Name), !, ^^assumel(l_name(Name)).
	l_telling(Name) :- l_default(Name).

	% projection of previous operations on default DCG stream
	l_def(S0, S) :- l_telling(Name), !, l_def(Name, S0, S).
	l_def(S0, S) :- l_default(Name), l_tell(Name), l_def(Name, S0, S).

	l_val(S0, S) :- l_telling(Name), l_val(Name, S0, S).

	l_connect(S0, X, S) :- l_telling(Name), l_connect(Name, S0, X, S).

	l_phrase(Axiom, S0) :- l_telling(Name), l_phrase(Name, Axiom, S0, []).

	l_phrase(Axiom, S0, S) :- l_telling(Name), l_phrase(Name, Axiom, S0, S).

	l_default(1).

	% syntactic sugar for `connect' relation
	'C'(X) :- l_val(S0, S), l_connect(S0, X, S).

	:- private(l_dcg/3).
	:- dynamic(l_dcg/3).

	:- private(l_name/1).
	:- dynamic(l_name/1).

:- end_object.
