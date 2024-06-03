%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2024-02-19,
		comment is 'Unit tests for the "threads/buffer" example.'
	]).

	:- threaded.

	clean :-
		buffer(_)::retractall(produced(_)),
		buffer(_)::retractall(consumed(_)).

	test(buffer_slower_consumer, true, [setup(clean)]) :-
		^^suppress_text_output,
		threaded_call(producer(4,0.3)::run(13)),
		threaded_call(consumer(4,0.8)::run(13)),
		threaded_exit(producer(4,0.3)::run(13)),
		threaded_exit(consumer(4,0.8)::run(13)),
		setof(PItem, buffer(4)::produced(PItem), PItems),
		^^assertion(PItems == [0,1,2,3,4,5,6,7,8,9,10,11,12]),
		setof(CItem, buffer(4)::consumed(CItem), CItems),
		^^assertion(CItems == [0,1,2,3,4,5,6,7,8,9,10,11,12]).

	test(buffer_slower_producer, true, [setup(clean)]) :-
		^^suppress_text_output,
		threaded_call(producer(4,0.8)::run(13)),
		threaded_call(consumer(4,0.3)::run(13)),
		threaded_exit(producer(4,0.8)::run(13)),
		threaded_exit(consumer(4,0.3)::run(13)),
		setof(PItem, buffer(4)::produced(PItem), PItems),
		^^assertion(PItems == [0,1,2,3,4,5,6,7,8,9,10,11,12]),
		setof(CItem, buffer(4)::consumed(CItem), CItems),
		^^assertion(CItems == [0,1,2,3,4,5,6,7,8,9,10,11,12]).

:- end_object.
