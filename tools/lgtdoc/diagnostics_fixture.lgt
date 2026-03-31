%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(lgtdoc_diagnostics_fixture(_Arg_)).

	:- info([
		author is 'Paulo Moura',
		date is 2099-01-01,
		remarks is [
			note - 'Entity remark without punctuation'
		]
	]).

	:- public(foo/1).
	foo(_).

	:- public(bar/1).
	:- mode(bar(?integer), one).
	:- info(bar/1, [
		fails_if is 'Predicate text without punctuation',
		exceptions is [
			condition - custom_exception
		],
		remarks is [
			note - 'Predicate remark without punctuation'
		]
	]).
	bar(_).

:- end_object.
