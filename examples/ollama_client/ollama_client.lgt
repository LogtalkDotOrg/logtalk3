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


:- object(ollama_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Ollama client example using native model discovery and the OpenAI-compatible client facade.'
	]).

	:- public(available/0).
	:- mode(available, zero_or_one).
	:- info(available/0, [
		comment is 'Succeeds when a local Ollama server answers a chat-capable models request. Uses the default ``http://127.0.0.1:11434/v1`` URL.'
	]).

	:- public(available/1).
	:- mode(available(+list(compound)), zero_or_one).
	:- info(available/1, [
		comment is 'Succeeds when a local Ollama server answers a chat-capable models request. Use the ``base_url(URL)`` option to override the default ``http://127.0.0.1:11434/v1`` URL.',
		argnames is ['Options']
	]).

	:- public(models/1).
	:- mode(models(-list(atom)), one_or_error).
	:- info(models/1, [
		comment is 'Returns the list of chat-capable model identifiers available from the local Ollama server. Uses the default ``http://127.0.0.1:11434/v1`` URL.',
		argnames is ['Models'],
		exceptions is [
			'The delegated native Ollama request rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'The server response is not a successful JSON response' - domain_error(ollama_response, 'Response'),
			'The native tags response does not contain a valid models list' - domain_error(ollama_tags_response, 'JSON'),
			'A native tags response model entry does not contain a valid name and size' - domain_error(ollama_tags_model, 'ModelJSON'),
			'The native model details response does not contain a valid capabilities list' - domain_error(ollama_model_capabilities_response, 'JSON')
		]
	]).

	:- public(models/2).
	:- mode(models(-list(atom), +list(compound)), one_or_error).
	:- info(models/2, [
		comment is 'Returns the list of chat-capable model identifiers available from the local Ollama server. Use the ``base_url(URL)`` option to override the default ``http://127.0.0.1:11434/v1`` URL.',
		argnames is ['Models', 'Options'],
		exceptions is [
			'The delegated native Ollama request rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'The server response is not a successful JSON response' - domain_error(ollama_response, 'Response'),
			'The native tags response does not contain a valid models list' - domain_error(ollama_tags_response, 'JSON'),
			'A native tags response model entry does not contain a valid name and size' - domain_error(ollama_tags_model, 'ModelJSON'),
			'The native model details response does not contain a valid capabilities list' - domain_error(ollama_model_capabilities_response, 'JSON')
		]
	]).

	:- public(ask/3).
	:- mode(ask(+atom, +atom, -atom), one_or_error).
	:- info(ask/3, [
		comment is 'Asks a question using the given local Ollama model and returns the assistant answer. Uses the default ``http://127.0.0.1:11434/v1`` URL.',
		argnames is ['Model', 'Question', 'Answer'],
		exceptions is [
			'The delegated OpenAI client request rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'The server response is not a successful JSON response' - domain_error(ollama_response, 'Response'),
			'The chat completion response does not contain a valid answer' - domain_error(ollama_chat_response, 'JSON')
		]
	]).

	:- public(ask/4).
	:- mode(ask(+atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(ask/4, [
		comment is 'Asks a question using the given local Ollama model and caller-supplied OpenAI client options. Use the ``base_url(URL)`` option to override the default ``http://127.0.0.1:11434/v1`` URL.',
		argnames is ['Model', 'Question', 'Answer', 'Options'],
		exceptions is [
			'``Options`` contains an invalid OpenAI client option' - domain_error(open_ai_client_option, 'Option'),
			'The delegated OpenAI client request rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'The server response is not a successful JSON response' - domain_error(ollama_response, 'Response'),
			'The chat completion response does not contain a valid answer' - domain_error(ollama_chat_response, 'JSON')
		]
	]).

	:- protected(open_ai_request/5).
	:- mode(open_ai_request(+atom, +list(pair), +compound, --compound, +list(compound)), one_or_error).
	:- info(open_ai_request/5, [
		comment is 'Hook predicate used to call the OpenAI client facade. Tests can override it to return canned responses.',
		argnames is ['OperationId', 'PathParameters', 'Body', 'Response', 'Options'],
		exceptions is [
			'The delegated OpenAI client request rejects an operation method declared by the catalog' - domain_error(open_ai_operation_method, 'Method'),
			'The delegated OpenAI client request rejects a path parameter' - domain_error(open_ai_path_parameter, 'Parameter'),
			'The delegated OpenAI client request rejects an option' - domain_error(open_ai_client_option, 'Option'),
			'The delegated OpenAI client request rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- protected(ollama_request/4).
	:- mode(ollama_request(+atom, +atom, --compound, +list(compound)), one_or_error).
	:- mode(ollama_request(+atom, +compound, --compound, +list(compound)), one_or_error).
	:- info(ollama_request/4, [
		comment is 'Hook predicate used to call native Ollama model info endpoints. Tests can override it to return canned responses.',
		argnames is ['OperationId', 'Body', 'Response', 'Options'],
		exceptions is [
			'The delegated HTTP client rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- uses(json_pointer, [
		evaluate/3
	]).

	:- uses(list, [
		member/2, memberchk/2, sort/4
	]).

	available :-
		available([]).

	available(Options0) :-
		request_options(Options0, Options),
		catch(::models(_, Options), _, fail).

	models(Models) :-
		models(Models, []).

	models(Models, Options0) :-
		context(Context),
		request_options(Options0, Options),
		::ollama_request(listTags, empty, Response, Options),
		response_json(Response, Context, JSON),
		model_size_pairs(JSON, Context, Pairs),
		sort(1, @=<, Pairs, SortedPairs),
		chat_model_ids(SortedPairs, Context, Options, Models).

	ask(Model, Question, Answer) :-
		::ask(Model, Question, Answer, []).

	ask(Model, Question, Answer, Options0) :-
		context(Context),
		chat_body(Model, Question, Body),
		request_options(Options0, Options),
		::open_ai_request(createChatCompletion, [], Body, Response, Options),
		response_json(Response, Context, JSON),
		(	evaluate([choices, '0', message, content], JSON, Answer),
			atom(Answer) ->
			true
		;	throw(error(domain_error(ollama_chat_response, JSON), Context))
		).

	% default definition
	open_ai_request(OperationId, PathParameters, Body, Response, Options) :-
		open_ai_client::request(OperationId, PathParameters, Body, Response, Options).

	% default definition
	ollama_request(showModel, Body, Response, Options) :-
		show_url(Options, URL),
		ollama_http_options(Options, HTTPOptions),
		http_client::post(URL, Body, Response, HTTPOptions).
	ollama_request(listTags, empty, Response, Options) :-
		tags_url(Options, URL),
		ollama_http_options(Options, HTTPOptions),
		http_client::get(URL, Response, HTTPOptions).

	default_base_url('http://127.0.0.1:11434/v1').

	request_options(Options, Options) :-
		member(base_url(_), Options),
		!.
	request_options(Options, [base_url(BaseURL)| Options]) :-
		default_base_url(BaseURL).

	chat_body(Model, Question, content('application/json', json({
		model-Model,
		messages-[
			{role-system, content-'Answer briefly.'},
			{role-user, content-Question}
		],
		stream- @false,
		max_tokens-16,
		reasoning_effort-'none'
	}))).

	response_json(Response, _Context, JSON) :-
		http_core::status(Response, status(200, 'OK')),
		http_core::body(Response, content('application/json', json(JSON))),
		!.
	response_json(Response, Context, _) :-
		throw(error(domain_error(ollama_response, Response), Context)).

	model_size_pairs(JSON, Context, Pairs) :-
		(	evaluate([models], JSON, Models) ->
			model_size_pairs_list(Models, Context, Pairs)
		;	throw(error(domain_error(ollama_tags_response, JSON), Context))
		).

	chat_model_ids([], _, _, []).
	chat_model_ids([_Size-ModelID| ModelIDs], Context, Options, Models) :-
		(	chat_model(ModelID, Context, Options) ->
			Models = [ModelID| Tail]
		;	Models = Tail
		),
		chat_model_ids(ModelIDs, Context, Options, Tail).

	chat_model(Model, Context, Options) :-
		show_body(Model, Body),
		::ollama_request(showModel, Body, Response, Options),
		response_json(Response, Context, JSON),
		model_capabilities(JSON, Context, Capabilities),
		chat_capabilities(Capabilities).

	chat_capabilities(Capabilities) :-
		member(completion, Capabilities),
		!.
	chat_capabilities(Capabilities) :-
		memberchk(chat, Capabilities).

	show_body(Model, content('application/json', json({model-Model}))).

	show_url(Options, URL) :-
		api_base_url(Options, BaseURL),
		atom_concat(BaseURL, '/api/show', URL).

	tags_url(Options, URL) :-
		api_base_url(Options, BaseURL),
		atom_concat(BaseURL, '/api/tags', URL).

	api_base_url(Options, BaseURL) :-
		memberchk(base_url(OpenAIBaseURL), Options),
		strip_open_ai_suffix(OpenAIBaseURL, BaseURL).

	strip_open_ai_suffix(OpenAIBaseURL, BaseURL) :-
		(	sub_atom(OpenAIBaseURL, Before, 3, 0, '/v1') ->
			sub_atom(OpenAIBaseURL, 0, Before, _, BaseURL)
		;	BaseURL = OpenAIBaseURL
		).

	ollama_http_options([], []).
	ollama_http_options([base_url(_)| Options], HTTPOptions) :-
		!,
		ollama_http_options(Options, HTTPOptions).
	ollama_http_options([Option| Options], [Option| HTTPOptions]) :-
		ollama_http_options(Options, HTTPOptions).

	model_capabilities(JSON, Context, Capabilities) :-
		(	evaluate([capabilities], JSON, Capabilities),
			capabilities_list(Capabilities) ->
			true
		;	throw(error(domain_error(ollama_model_capabilities_response, JSON), Context))
		).

	capabilities_list([]).
	capabilities_list([Capability| Capabilities]) :-
		atom(Capability),
		capabilities_list(Capabilities).

	model_size_pairs_list([], _, []).
	model_size_pairs_list([ModelJSON| ModelJSONs], Context, [Size-Model| Pairs]) :-
		model_size_pair(ModelJSON, Context, Size-Model),
		model_size_pairs_list(ModelJSONs, Context, Pairs).

	model_size_pair(ModelJSON, Context, Size-Model) :-
		(	evaluate([name], ModelJSON, Model),
			atom(Model),
			evaluate([size], ModelJSON, Size),
			integer(Size) ->
			true
		;	throw(error(domain_error(ollama_tags_model, ModelJSON), Context))
		).

:- end_object.
