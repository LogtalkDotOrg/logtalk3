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


:- object(fluents).

	:- info([
		version is 1.0,
		author is 'Paul Tarau and Paulo Moura',
		date is 2016/06/15,
		comment is 'Examples of implementing fluents using threaded engines.'
	]).

	:- threaded.

	% initialize a fluent and give it a name (which is object-scoped)
	:- initialization(threaded_engine_create(X, a(X), fluent)).

	:- public(next/1).
	:- mode(next(?term), zero_or_one).
	:- info(next/1, [
		comment is 'Returns the next solution for the fluent goal.',
		argnames is ['Next']
	]).

	next(Next) :-
		threaded_engine_next(fluent, Next).

	a(1). a(2). a(3).

:- end_object.
