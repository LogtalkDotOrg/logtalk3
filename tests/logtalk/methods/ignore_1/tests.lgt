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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/05/29,
		comment is 'Unit tests for the ignore/1 built-in method.'
	]).

	% ignore/1 calls are expanded and thus the error term is for call/1

	throws(ignore_1_01, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		ignore(_).

	throws(ignore_1_02, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		ignore(Goal).

	% it's not always possible to decompile the actual call

	throws(ignore_1_03, error(existence_error(procedure,_),logtalk(call(p(_)),This))) :-
		this(This),
		Goal = p(_),
		ignore(Goal).

	succeeds(ignore_1_04) :-
		ignore(true).

	succeeds(ignore_1_05) :-
		ignore(fail).

	% ignore/1 is opaque to cuts

	succeeds(ignore_1_06) :-
		findall(X, ((X = 1; X =2; X = 3), ignore(!)), L), 
		L == [1, 2, 3].

	succeeds(ignore_1_07) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((true,!))), L), 
		L == [1, 2, 3].

	succeeds(ignore_1_08) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((true;!))), L), 
		L == [1, 2, 3].

	succeeds(ignore_1_09) :-
		findall(X, ((X = 1; X =2; X = 3), ignore((fail;!))), L), 
		L == [1, 2, 3].

:- end_object.
