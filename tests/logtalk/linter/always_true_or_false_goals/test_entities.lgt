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


:- object(tautology).

	% goals are always true (usually happens due to typos)

	oops :- x \== y.

	really :- \+ x == y.

:- end_object.



:- object(falsehood).

	% goals are always false; usually happens due to typos...

	damn :- x == y.

	rats :- \+ x \== y.

	:- if(\+ current_logtalk_flag(prolog_dialect,xsb)).
		% avoid compilation error with XSB
		jinx(X) :- a is X*2.
	:- endif.

	hum(X) :- 1 is sqrt(X).

	% ... or misinterpretation of operator precedence

	p :-
		m(2 * 3 + 4) = m(_ * _).

	q :-
		unify_with_occurs_check(m(2 * 3 + 4), m(_ * _)).

	r :-
		a(1,_) \= a(2, _).

:- end_object.
