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


:- object(open_ai).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Facade predicates for the OpenAI-compatible public non-admin API library.'
	]).

	:- public(document/1).
	:- mode(document(-compound), one).
	:- info(document/1, [
		comment is 'Returns the generated OpenAPI document for the supported OpenAI-compatible endpoint surface.',
		argnames is ['Document']
	]).

	:- public(operation_count/1).
	:- mode(operation_count(-integer), one).
	:- info(operation_count/1, [
		comment is 'Returns the number of supported public non-admin OpenAI operations in the pinned catalog.',
		argnames is ['Count']
	]).

	document(Document) :-
		open_api::document(open_ai_api, Document).

	operation_count(Count) :-
		open_ai_catalog::operation_count(Count).

:- end_object.

