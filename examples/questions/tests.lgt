%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% calls to the logtalk::ask_question/5 predicate can be intercepted
% by defining the logtalk::question_hook/6 multifile hook predicate;
% in this example, we provide a fixed (and valid) answer

:- category(hitchhikers_fixed_answers).

	:- multifile(logtalk::question_hook/6).
	:- dynamic(logtalk::question_hook/6).

	logtalk::question_hook(ultimate_question, question, hitchhikers, _, _, 42).

:- end_category.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-10-31,
		comment is 'Unit tests for the "questions" example.'
	]).

	% in a practical case, the fixed answer would
	% be used for followup goals being tested

	test(questions_01, true(N == 42)) :-
		logtalk::ask_question(question, hitchhikers, ultimate_question, '=='(42), N).

	% the question answer read loop (which calls the question
	% check closure) is not used when a fixed answer is
	% provided using the logtalk::question_hook/6 hook
	% predicate thus preventing the creation of endless loops

	test(questions_02, true(N == 42)) :-
		logtalk::ask_question(question, hitchhikers, ultimate_question, '=='(41), N).

:- end_object.
