%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


% source file level set_logtalk_flag/2 directives are local to the file
:- set_logtalk_flag(complements, deny).
:- set_logtalk_flag(context_switching_calls, allow).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/12/06,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in directive.'
	]).

	% entity level  set_logtalk_flag/2 directives are local to the entity
	:- set_logtalk_flag(complements, allow).
	:- set_logtalk_flag(context_switching_calls, deny).

	test(set_logtalk_flag_2_1) :-
		this(This),
		object_property(This, complements).

	test(set_logtalk_flag_2_2) :-
		this(This),
		\+ object_property(This, context_switching_calls).

	test(set_logtalk_flag_2_3) :-
		bottom::p(X),
		X == top.

	test(set_logtalk_flag_2_4) :-
		logtalk_load(patch),
		bottom::p(X),
		X == patch.

	% tests for the "optimize" flag

	test(set_logtalk_flag_2_5) :-
		optimize_off::p.

	test(set_logtalk_flag_2_6) :-
		optimize_on_1::p.

	test(set_logtalk_flag_2_7) :-
		optimize_on_2::p.

:- end_object.
