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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2019-08-06,
		comment is 'Unit tests for the "slides" example.'
	]).

	:- private(answer_/1).
	:- dynamic(answer_/1).

	cover(slides).

	setup :-
		% sequence of answers for the test
		assertz(answer_(n)),
		assertz(answer_(n)),
		assertz(answer_(n)),
		assertz(answer_(p)),
		assertz(answer_(f)),
		assertz(answer_(l)),
		assertz(answer_(e)),
		% divert the slide show output to the stream
		% identified with the "test_output" alias
		logtalk::retractall(message_prefix_stream(_, slides, _, _)),
		logtalk::assertz(message_prefix_stream(_, slides, '', test_output)).

	test(slides_01, true(Assertion)) :-
		^^set_text_output(test_output, ''),
		slides::show([1,2,3,4,5,6], {slide}),
		^^text_output_assertion(
			test_output,
			'First slide\nSecond slide\nThird slide\nFourth slide\nThird slide\nFirst slide\nLast slide\n',
			Assertion
		).

	:- multifile(logtalk::question_hook/6).
	:- dynamic(logtalk::question_hook/6).

	logtalk::question_hook(remote, question, slides, _, _, Answer) :-
		retract(answer_(Answer)).

:- end_object.
