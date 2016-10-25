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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/10/25,
		comment is 'Unit tests for the logtalk_make/0-1 built-in predicates.'
	]).

	% logtalk_make/0

	test(logtalk_make_0_01) :-
		% call in the "user" pseudo-object
		{logtalk_make}.

	test(logtalk_make_0_02) :-
		% call from within this object
		logtalk_make.

	% logtalk_make/1

	test(logtalk_make_1_01) :-
		% call in the "user" pseudo-object
		{logtalk_make(all)}.

	test(logtalk_make_1_02) :-
		% call from within this object
		logtalk_make(all).

	test(logtalk_make_1_03) :-
		% call in the "user" pseudo-object
		{logtalk_make(clean)}.

	test(logtalk_make_1_04) :-
		% call from within this object
		logtalk_make(clean).

	test(logtalk_make_1_05) :-
		% call in the "user" pseudo-object
		{logtalk_make(missing)}.

	test(logtalk_make_1_06) :-
		% call from within this object
		logtalk_make(missing).

	test(logtalk_make_1_07) :-
		% call in the "user" pseudo-object
		{logtalk_make(circular)}.

	test(logtalk_make_1_08) :-
		% call from within this object
		logtalk_make(circular).

	% supress all logtalk_make/0-1 messages to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, comment(make), core, _Tokens).
	logtalk::message_hook(_Message, warning(make), core, _Tokens).

:- end_object.
