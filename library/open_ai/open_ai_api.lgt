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


:- object(open_ai_api,
	implements(open_api_provider_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'OpenAPI provider for the public non-admin OpenAI-compatible API surface.'
	]).

	api_info(info('OpenAI API', Version, 'Public non-admin OpenAI-compatible API surface.', [
		description('Generated from the pinned official OpenAI OpenAPI document with organization and project administration endpoints excluded.')
	])) :-
		open_ai_catalog::spec_version(Version).

	servers([
		server('https://api.openai.com/v1', 'OpenAI API')
	]).

	security([['ApiKeyAuth'-[]]]).

	security_scheme('ApiKeyAuth', http(bearer)).

	operations(Operations) :-
		findall(Operation, operation_descriptor(Operation), Operations).

	operation_descriptor(operation(Id, Method, Path, Summary, Parameters, RequestBody, Responses, Properties)) :-
		open_ai_catalog::operation(Id, _Tag, Method, Path, Summary, Parameters, RequestBody, Responses),
		open_ai_catalog::operation_properties(Id, Properties).

:- end_object.

