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

% Generated from WHATWG entities.json. Do not edit.
% 21 semicolon-terminated named character references.

:- category(common_text_entities).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Generated common semicolon-terminated HTML named character references.'
	]).

	:- protected(common_named_entity/2).
	:- mode(common_named_entity(+atom, -list(integer)), zero_or_one).
	:- info(common_named_entity/2, [comment is 'Common named character reference mapping.', argnames is ['Name', 'Codes']]).

	common_named_entity(amp, [38]).
	common_named_entity(apos, [39]).
	common_named_entity(cent, [162]).
	common_named_entity(copy, [169]).
	common_named_entity(euro, [8364]).
	common_named_entity(gt, [62]).
	common_named_entity(hellip, [8230]).
	common_named_entity(laquo, [171]).
	common_named_entity(lt, [60]).
	common_named_entity(mdash, [8212]).
	common_named_entity(middot, [183]).
	common_named_entity(nbsp, [160]).
	common_named_entity(ndash, [8211]).
	common_named_entity(para, [182]).
	common_named_entity(pound, [163]).
	common_named_entity(quot, [34]).
	common_named_entity(raquo, [187]).
	common_named_entity(reg, [174]).
	common_named_entity(sect, [167]).
	common_named_entity(trade, [8482]).
	common_named_entity(yen, [165]).

:- end_category.
