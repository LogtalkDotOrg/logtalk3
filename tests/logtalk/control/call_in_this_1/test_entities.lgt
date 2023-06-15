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


:- category(cat).

	:- public(bar/1).
	bar(X) :-
		@baz(Y),
		X is Y + 1.

:- end_category.


:- object(obj,
	imports(cat)).

	:- set_logtalk_flag(complements, allow).

	:- public(foo/1).
	foo(1).

	:- public(baz/1).
	baz(2).

	:- public(qux/1).
	qux(X) :-
		@baz(X).

:- end_object.


:- category(ccat,
	complements(obj)).

	foo(X) :-
		@foo(Y),
		X is Y + 1.

:- end_category.
