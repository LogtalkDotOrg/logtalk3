%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(double_quotes, atom).


:- object(tests_tokenizer_atom,
	extends(lgtunit)).

	:- uses([
		tokenizer(atom, english_tokenizer) as tokenizer,
		tokenizer(atom, tiny_tokenizer) as tiny_tokenizer
	]).

	:- include(tests_tokenizer).
	:- if(\+ current_logtalk_flag(unicode, unsupported)).
		:- include(tests_tokenizer_unicode).

		normalization_input(Input) :-
			atom_codes(Input, [8216,72,101,108,108,111,8217,8212,119,111,114,108,100]).
	:- endif.

:- end_object.
