%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- initialization(client_b_1::test).


:- object(library_b_1).

	:- public(meta/2).
	:- meta_predicate(meta(0, 0)).
	meta(Goal1, Goal2) :-
		call(Goal1), call(Goal2).

	:- public(meta/1).
	:- meta_predicate(meta(0)).
	meta(Goal1) :-
		meta(Goal1, local).

	local :-
		write('local predicate in object library'), nl.

:- end_object.


:- object(client_b_1).

	:- public(test/0).
	test :-
		library_b_1::meta(goal).

	goal :-
		write('goal meta-argument in object client'), nl.

	local :-
		write('local predicate in object client'), nl.

:- end_object.
