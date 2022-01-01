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


:- protocol(processing).

	:- public(count_atomics/3).

:- end_protocol.



:- object(defaulty,
	implements(processing)).

	count_atomics(List, Atoms, Numbers) :-
		count_atomics(List, 0, Atoms, 0, Numbers).

	count_atomics([], Atoms, Atoms, Numbers, Numbers).
	count_atomics([Term| Terms], Atoms0, Atoms, Numbers0, Numbers) :-
		count_atomic(Term, Atoms0, Atoms1, Numbers0, Numbers1),
		count_atomics(Terms, Atoms1, Atoms, Numbers1, Numbers).

	count_atomic(Term, Atoms0, Atoms1, Numbers0, Numbers1) :-
		atom(Term),
		!,
		Atoms1 is Atoms0 + 1,
		Numbers1 is Numbers0.
	count_atomic(Term, Atoms0, Atoms1, Numbers0, Numbers1) :-
		number(Term),
		!,
		Atoms1 is Atoms0,
		Numbers1 is Numbers0 + 1.
	count_atomic(_, Atoms1, Atoms1, Numbers1, Numbers1).

:- end_object.



:- object(tagged,
	implements(processing)).

	count_atomics(List, Atoms, Numbers) :-
		count_atomics(List, 0, Atoms, 0, Numbers).

	count_atomics([], Atoms, Atoms, Numbers, Numbers).
	count_atomics([Term| Terms], Atoms0, Atoms, Numbers0, Numbers) :-
		count_atomic(Term, Atoms0, Atoms1, Numbers0, Numbers1),
		count_atomics(Terms, Atoms1, Atoms, Numbers1, Numbers).

	count_atomic(a(_), Atoms0, Atoms1, Numbers0, Numbers1) :-
		Atoms1 is Atoms0 + 1,
		Numbers1 is Numbers0.
	count_atomic(n(_), Atoms0, Atoms1, Numbers0, Numbers1) :-
		Atoms1 is Atoms0,
		Numbers1 is Numbers0 + 1.
	count_atomic(o(_), Atoms1, Atoms1, Numbers1, Numbers1).

:- end_object.
