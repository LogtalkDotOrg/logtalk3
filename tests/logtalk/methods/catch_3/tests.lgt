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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2024-06-22,
		comment is 'Unit tests for the catch/3 built-in method.'
	]).

	:- set_logtalk_flag(unknown_predicates, silent).

	test(catch_3_01, deterministic) :-
		catch(true, _, _).

	test(catch_3_02, deterministic) :-
		catch(a(1), _, _).

	test(catch_3_03, deterministic) :-
		Goal = a(1),
		catch(Goal, _, _).

	test(catch_3_04, false) :-
		catch(fail, _, _).

	test(catch_3_05, false) :-
		% avoid a warning about a no matching clause for goal a(4)
		% by delaying the argument instantiation to runtime
		N = 4,
		catch(a(N), _, _).

	test(catch_3_06, false) :-
		Goal = a(4),
		catch(Goal, _, _).

	test(catch_3_07, deterministic) :-
		catch(throw(e), Error, (Error == e)).

	test(catch_3_08, deterministic) :-
		catch(throw(e), Error, (Error == e)).

	test(catch_3_09, deterministic(X == 1)) :-
		phrase(gr1, [X]).

	test(catch_3_10, deterministic) :-
		phrase(gr2, _).

	a(1). a(2). a(3).

	gr1 -->
		catch(a, _, _).

	gr2 -->
		catch(b, _, {true}).

	a --> [1].

:- end_object.
