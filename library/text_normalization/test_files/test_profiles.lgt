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


:- object(test_text_normalization_profile,
	extends(default_text_normalization)).

	case_conversion(lower, [73], [305]) :-
		!.
	case_conversion(Mode, Codes, Converted) :-
		^^case_conversion(Mode, Codes, Converted).

	named_entity(example, [88]) :-
		!.
	named_entity(capital_i, [73]) :-
		!.
	named_entity(pair, [65, 66]) :-
		!.
	named_entity(Name, Codes) :-
		^^named_entity(Name, Codes).

:- end_object.


:- object(test_text_case,
	imports(text_case_folding)).

	:- public([
		convert/4
	]).

	convert(Mode, Codes, Profile, Converted) :-
		^^convert_case_codes(Mode, Codes, Profile, Converted).

:- end_object.


:- object(test_text_unicode,
	imports(text_unicode)).

	:- public([
		normalize/3
	]).

	normalize(Form, Codes, Normalized) :-
		^^normalize_unicode_codes(Form, Codes, Normalized).

:- end_object.


:- object(test_text_entities,
	imports(text_entities)).

	:- public(decode/3).

	decode(Codes, Unknown, Decoded) :-
		^^decode_entities_codes(Codes, test_text_normalization_profile, Unknown, Decoded).

:- end_object.


:- object(test_text_whitespace,
	imports(text_whitespace)).

	:- public(normalize/6).

	normalize(Codes, Trim, Collapse, LineEndings, Controls, Normalized) :-
		^^normalize_whitespace_codes(Codes, Trim, Collapse, LineEndings, Controls, Normalized).

:- end_object.