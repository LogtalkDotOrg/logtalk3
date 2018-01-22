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


:- object(test_object).

	:- public(ie/1).
	ie(Object) :-
		Object::current_predicate(foo/1).

	:- public(te/0).
	te :-
		Object = 1,
		Object::current_predicate(foo/1).

:- end_object.



:- object(parent).

	:- public(foobar/0).
	foobar.

	:- public(foo/1).
	foo(parent).

	:- public(bar/2).
	bar(parent, parent).

:- end_object.



:- object(proto,
	extends(parent)).

	:- public(foobar/0).
	foobar.

	:- protected(foo/1).
	foo(proto).

	:- private(bar/2).
	bar(proto, proto).

:- end_object.
