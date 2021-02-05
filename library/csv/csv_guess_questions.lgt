%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Jacinto Dávila <jdavila@optimusprime.ai>
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


:- category(csv_guess_questions).

	:- info([
		version is 1:0:0,
		author is 'Jacinto Dávila',
		date is 2021-02-03,
		comment is 'Support for asking questions when guessing the separator and the record arity of CSV files.'
	]).

    :- multifile(logtalk::message_tokens//2).
    :- dynamic(logtalk::message_tokens//2).

    % abstract the question text using the atom ultimate_question
    % the second argument, hitchhikers, is the application component
    logtalk::message_tokens(guess_row(Row), csv) -->
        ['Is this the proper reading of a line of this file (y/n)? ~q'-[Row], nl].

	% asking about a possible combination
	logtalk::message_tokens(i_suggest_separator(Row, Sep), csv) -->
		['For ~q, I suggest ~w as separator'-[Row, Sep], nl].

	% asking about Arity
	logtalk::message_tokens(i_suggest_arity(Row, Arity), csv) -->
		['For ~q, I guess the arity is ~w'-[Row, Arity], nl].

   :- multifile(logtalk::question_prompt_stream/4).
   :- dynamic(logtalk::question_prompt_stream/4).

   % the prompt is specified here instead of being part of the question text
   % as it will be repeated if the answer doesn't satisfy the question closure
   logtalk::question_prompt_stream(question, csv, '> ', user_input).

:- end_category.
