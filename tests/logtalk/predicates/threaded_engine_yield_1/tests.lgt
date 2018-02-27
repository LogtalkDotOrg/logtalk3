%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/02/27,
		comment is 'Unit tests for the threaded_engine_yield/1 built-in predicate.'
	]).

	:- threaded.

	% calls outside the context of an engine fail
	fails(threaded_engine_yield_1_01) :-
		threaded_engine_yield(_).

	% no restrictions on the kind of terms that can be returned as answers

	succeeds(threaded_engine_yield_1_02) :-
		threaded_engine_create(none, return_atom, test_engine_1),
		threaded_engine_next(test_engine_1, Answer),
		Answer == foo.

	succeeds(threaded_engine_yield_1_03) :-
		threaded_engine_create(none, return_compound, test_engine_2),
		threaded_engine_next(test_engine_2, Answer),
		^^variant(Answer, f(X,_,X)).

	succeeds(threaded_engine_yield_1_04) :-
		threaded_engine_create(none, return_var, test_engine_3),
		threaded_engine_next(test_engine_3, Answer),
		var(Answer).

	% auxiliary predicates

	return_atom :-
		threaded_engine_yield(foo).

	return_compound :-
		threaded_engine_yield(f(X,_,X)).

	return_var :-
		threaded_engine_yield(_).

:- end_object.
