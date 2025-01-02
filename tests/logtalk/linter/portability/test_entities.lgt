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


:- [file].


:- logtalk_load(library).


:- write(hello).


:- object(portability).

	foo :-
		open(file, read, _, [encoding('UTF-16')]).

	bar :-
		read_term(_, [lines(_, _)]).

	baz :-
		current_prolog_flag(encoding, _).

	qux :-
		current_prolog_flag(double_quotes, string).

	:- if(predicate_property(tell(_), built_in)).
		quux :-
			tell(file).
	:- endif.

	:- if(catch(_ is popcount(42), _, fail)).
		corge :-
			_ is popcount(42).
	:- endif.

%	predicate :-
%		_ is fun(42).

:- end_object.
