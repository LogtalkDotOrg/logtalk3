%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/11/10,
		comment is 'Unit tests for the once/1 built-in method.'
	]).

	% once/1 calls are expanded and thus the error term is for call/1

	throws(once_1_1, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		once(_).

	throws(once_1_2, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		once(Goal).

	% it's not always possible to decompile the actual call

	throws(once_1_3, error(existence_error(procedure,_),logtalk(call(p(_)),This))) :-
		this(This),
		Goal = p(_),
		once(Goal).

	succeeds(once_1_4) :-
		findall(X, once(a(X)), Xs),
		Xs == [1].

	succeeds(once_1_5) :-
		findall(X, (once(a(X)); X = 0), Xs),
		Xs == [1, 0].

	fails(once_1_6) :-
		once(a(4)).

	a(1).
	a(2).
	a(3).

:- end_object.
