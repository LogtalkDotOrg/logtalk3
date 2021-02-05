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


:- object(p1,
	implements(expanding)).

	term_expansion(t(a), t(b)).

	goal_expansion(a(X), b(X)).

:- end_object.


:- object(p2,
	implements(expanding)).

	term_expansion(t(b), t(c)).

	goal_expansion(b(X), c(X)).

:- end_object.


:- object(p3,
	implements(expanding)).

	term_expansion(t(c), t(d)).

	goal_expansion(c(X), d(X)).

:- end_object.


:- object(s1,
	implements(expanding)).

	term_expansion(a(X), [(:- public(aa/1)), aa(X)]).

	goal_expansion(g(F1, Y), j(F2, Y)) :-
		F2 is F1 * 2.

:- end_object.


:- object(s2,
	implements(expanding)).

	term_expansion(b(X), [(:- public(bb/1)), bb(X)]).

	goal_expansion(h(F1, Y), g(F2, Y)) :-
		F2 is F1 * 3.

:- end_object.


:- object(s3,
	implements(expanding)).

	term_expansion(c(X), [(:- public(cc/1)), cc(X)]).

	goal_expansion(j(F1, Y), k(F2, Y)) :-
		F2 is F1 * 5.

:- end_object.
