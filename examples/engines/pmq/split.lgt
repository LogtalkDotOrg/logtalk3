%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(split).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-11-19,
		comment is 'An implementation of a split queue for priority and normal messages using a threaded engine.'
	]).

	:- threaded.

	% initialize a perpetual threaded engine holding two queues
	:- initialization(
		threaded_engine_create(none, loop(Top-Top, Normal-Normal), split)
	).

	% send a message to the queues
	:- public(send/1).
	send(Term) :-
		threaded_engine_post(split, Term).

	% retrieve a list of messages with priority messages listed first
	:- public(messages/1).
	messages(Messages) :-
		threaded_engine_post(split, messages),
		threaded_engine_next(split, Messages).

	% we split messages with priority greater than 10 but we clould
	% also pass the split value uisng e.g. an object parameter
	loop(Top0-TopTail0, Normal0-NormalTail0) :-
		threaded_engine_fetch(Term),
		(	Term == messages ->
			TopTail0 = Normal0, NormalTail0 = [],
			threaded_engine_yield(Top0),
			loop(Top-Top, Normal-Normal)
		;	Term = Priority-Message,
			(	Priority > 10 ->
				TopTail0 = [Message| TopTail1],
				loop(Top0-TopTail1, Normal0-NormalTail0)
			;	NormalTail0 = [Message| NormalTail1],
				loop(Top0-TopTail0, Normal0-NormalTail1)
			)
		).

:- end_object.
