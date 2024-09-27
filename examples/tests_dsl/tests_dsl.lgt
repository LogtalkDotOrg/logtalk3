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


:- object(tests_dsl,
    implements(expanding)).

    :- public(op(1200, xfx, =>)).

    :- uses(gensym, [gensym/2, reset_gensym/1]).
    :- uses(os, [decompose_file_name/4]).

    term_expansion(
        begin_of_file,
        [begin_of_file, (:- object(Name, extends(lgtunit)))]
    ) :-
        logtalk_load_context(source, Source),
        decompose_file_name(Source, _, Name, _),
        reset_gensym(t).

    term_expansion(
        ({A, B, C} => D),
        (test(Test, true(R == D)) :- app::compute(A, B, C, R))
    ) :-
        gensym(t, Test).

    term_expansion(
        end_of_file,
        [(:- end_object), end_of_file]
    ).

:- end_object.
