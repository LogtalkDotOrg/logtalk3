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


:- category(hitchhikers_guide_to_the_galaxy).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	% abstract the question text using the atom ultimate_question
	% the second argument, hitchhikers, is the application component
	logtalk::message_tokens(ultimate_question, hitchhikers) -->
		['The answer to the ultimate question of Life, The Universe and Everything is?'-[], nl].

	:- multifile(logtalk::question_prompt_stream/4).
	:- dynamic(logtalk::question_prompt_stream/4).

	% the prompt is specified here instead of being part of the question text
	% as it will be repeated if the answer doesn't satisfy the question closure
	logtalk::question_prompt_stream(question, hitchhikers, '> ', user_input).

:- end_category.
