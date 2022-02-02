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

:- object(reverse_int, implements(comparingp)).

    :- public(n/1).
    :- public(valid/1).
    valid(X) :-
        X::n(N),
        integer(N).

    X > Y :-
        X::n(N0),
        Y::n(N1),
        { N0 < N1 }.

    X < Y :-
        X::n(N0),
        Y::n(N1),
        { N0 > N1 }.

    X =:= Y :-
        X::n(N0),
        Y::n(N1),
        { N0 =:= N1 }.

:- end_object.

:- object(r1, extends(reverse_int)).
    n(1).
:- end_object.

:- object(r2, extends(reverse_int)).
    n(1).
:- end_object.

:- object(r3, extends(reverse_int)).
    n(50).
:- end_object.

:- object(r4, extends(reverse_int)).
    n(100).
:- end_object.

:- object(tests_comparingp,
	extends(lgtunit)).

    cover(set(reverse_int)).

    % as_set/2 tests

    test(as_set_2_01, deterministic(N == 3)) :-
        set(reverse_int)::as_set([r1, r2, r3, r4], Set),
        length(Set, N).

    % insert/3 tests
    test(insert_3_01, deterministic(N == 2)) :-
        set(reverse_int)::as_set([r3], Set),
        set(reverse_int)::insert(Set, r4, Set1),
        length(Set1, N).

    test(insert_3_02, deterministic(N == 2)) :-
        set(reverse_int)::as_set([r1, r3], Set),
        set(reverse_int)::insert(Set, r2, Set1),
        length(Set1, N).

    % insert_all/3 tests

    test(insert_all_3_01, deterministic(N == 3)) :-
        set(reverse_int)::as_set([r1], Set),
        set(reverse_int)::insert_all([r3, r2, r4], Set, Set1),
        length(Set1, N).

    % valid/1 tests

	test(set_valid_1_01, deterministic) :-
		set(reverse_int)::as_set([], Set),
		set(reverse_int)::valid(Set).

	test(set_valid_1_02, deterministic) :-
		set(reverse_int)::as_set([r1,r3,r4], Set),
		set(reverse_int)::valid(Set).

:- end_object.