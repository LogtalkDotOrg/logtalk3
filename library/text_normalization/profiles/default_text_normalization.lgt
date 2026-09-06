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


:- object(default_text_normalization,
	implements(text_normalization_profile_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Default language-neutral text normalization profile.',
		see_also is [text_normalizer(_, _)]
	]).

	case_conversion(_, _, _) :-
		fail.

	diacritic_fold(0x00D8, [0x004F]).
	diacritic_fold(0x00F8, [0x006F]).
	diacritic_fold(0x0141, [0x004C]).
	diacritic_fold(0x0142, [0x006C]).

	named_entity(Name, Codes) :-
		xml_whatwg_entities::xml_whatwg_named_entity(Name, Codes).

:- end_object.
