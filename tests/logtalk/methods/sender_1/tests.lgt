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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017-03-06,
		comment is 'Unit tests for the sender/1 built-in method.'
	]).

	:- public(test_sender/1).
	test_sender(Sender) :-
		sender(Sender).

	test(sender_1) :-
		this(This),
		{This::test_sender(Sender)},
		Sender == user.

	test(sender_2) :-
		this(This),
		{This::test_sender(user)}.

	test(sender_3) :-
		this(This),
		\+ {This::test_sender(other)}.

	test(sender_4) :-
		this(This),
		sender_1_test_object_1::p(Sender),
		Sender == This.

:- end_object.
