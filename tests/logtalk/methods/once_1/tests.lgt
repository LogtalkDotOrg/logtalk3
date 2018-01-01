%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/05/29,
		comment is 'Unit tests for the once/1 built-in method.'
	]).

	% once/1 calls are expanded and thus the error term is for call/1

	throws(once_1_01, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		once(_).

	throws(once_1_02, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		once(Goal).

	% it's not always possible to decompile the actual call

	throws(once_1_03, error(existence_error(procedure,_),logtalk(call(p(_)),This))) :-
		this(This),
		Goal = p(_),
		once(Goal).

	succeeds(once_1_04) :-
		once(!).

	succeeds(once_1_05) :-
		findall(X, once(a(X)), Xs),
		Xs == [1].

	succeeds(once_1_06) :-
		findall(X, (once(a(X)); X = 0), Xs),
		Xs == [1, 0].

	succeeds(once_1_07) :-
		findall(X, (once(!); a(X)), Xs),
		Xs = [H| T], var(H), T == [1, 2, 3].

	succeeds(once_1_08) :-
		findall(X, (once((!, fail)); a(X)), Xs),
		Xs == [1, 2, 3].

	fails(once_1_09) :-
		once(a(4)).

	fails(once_1_10) :-
		once(fail).

	% once/1 is opaque to cuts

	succeeds(once_1_11) :-
		findall(X, ((X = 1; X =2; X = 3), once(!)), L), 
		L == [1, 2, 3].

	succeeds(once_1_12) :-
		findall(X, ((X = 1; X =2; X = 3), once((true,!))), L), 
		L == [1, 2, 3].

	succeeds(once_1_13) :-
		findall(X, ((X = 1; X =2; X = 3), once((true;!))), L), 
		L == [1, 2, 3].

	% auxiliary predicates

	a(1).
	a(2).
	a(3).

:- end_object.
