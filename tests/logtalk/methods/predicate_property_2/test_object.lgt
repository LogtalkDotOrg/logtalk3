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


:- object(test_object).

	% for supporting testing instantiation errors
	:- public(ie/1).
	ie(Object) :-
		Object::predicate_property(foo, _).

	% for supporting testing type errors
	:- public(te/0).
	te :-
		Object = 1,
		Object::predicate_property(foo, _).

	% for supporting testing meta-predicate properties
	:- public(meta/2).
	:- meta_predicate(meta(0, *)).

	% for supporting testing non-terminal properties
	:- public(nt//0).

:- end_object.
