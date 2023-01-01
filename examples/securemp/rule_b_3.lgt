%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- initialization(catch(client_b_3::test(_), Error, (writeq(Error), nl))).


:- object(library_b_3).

	:- public(m/2).
	:- meta_predicate(m(1, *)).
	m(Closure, Arg) :-
		Closure =.. List,
		my_append(List, [Arg, _], NewList),
		Call =.. NewList,
		call(Call).

	my_append([], List, List).
	my_append([Head| Tail], List, [Head| Tail2]) :-
		my_append(Tail, List, Tail2).

:- end_object.


:- object(client_b_3).

	:- public(test/1).
	test(X) :-
		library_b_3::m(a, X).

	a(1). a(2).

	a(3, one). a(4, two).

:- end_object.
