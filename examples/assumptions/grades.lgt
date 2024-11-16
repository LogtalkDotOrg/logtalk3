%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(grades,
	imports(assumptions)).

	:- info([
		version is 1:0:0,
		author is 'Example posted on reddit on a topic about Harrop clauses.',
		date is 2014-11-16,
		comment is 'Compute grades using linear assumptions.'
	]).

	:- public(grade/1).
	grade(Person) :-
		take(Person, german),
		take(Person, french).
	grade(Person) :-
		take(Person, german),
		take(Person, italian).

	:- private(take/2).
	:- dynamic(take/2).
	take(hans, french).

:- end_object.
