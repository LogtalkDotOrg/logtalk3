%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(obj).

	:- uses([
		list as l
	]).

	:- public(p/1).
	p(X) :-
		l::member(X, [1,2,3]).

:- end_object.


:- object(obj1(_ListObject_)).

	:- uses([
		_ListObject_ as l
	]).

	:- public(p/1).
	p(X) :-
		l::member(X, [1,2,3]).

:- end_object.


:- object(obj2(_Type_)).

	:- uses([
		list(_Type_) as l
	]).

	:- public(p/0).
	p :-
		l::valid([1,2,3]).

:- end_object.
