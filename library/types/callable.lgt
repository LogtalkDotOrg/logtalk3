%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(callable,
	extends(term)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2018-07-11,
		comment is 'Callable term type predicates.'
	]).

	valid(Callable) :-
		(	atom(Callable) ->
			true
		;	compound(Callable)
		).

	check(Term) :-
		(	atom(Term) ->
			true
		;	compound(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(callable, Term)
		).

:- end_object.
