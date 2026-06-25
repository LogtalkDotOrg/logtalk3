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
		date is 2026-06-25,
		comment is 'Ollama client example using the OpenAI-compatible client facade.'
	]).

	:- public(available/0).
	:- mode(available, zero_or_one).
	:- info(available/0, [
		comment is 'Succeeds when a local Ollama OpenAI-compatible server answers a models request.'
	]).

	:- public(models/1).
	:- mode(models(-list(atom)), one_or_error).
	:- info(models/1, [
		comment is 'Returns the list of model identifiers available from the local Ollama server.',
		argnames is ['Models']
	]).

	:- public(ask/3).
	:- mode(ask(+atom, +atom, -atom), one_or_error).
	:- info(ask/3, [
		comment is 'Asks a question using the given local Ollama model and returns the assistant answer.',
		argnames is ['Model', 'Question', 'Answer']
	]).

	:- public(ask/4).
	:- mode(ask(+atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(ask/4, [
		comment is 'Asks a question using the given local Ollama model and caller-supplied OpenAI client options.',
		argnames is ['Model', 'Question', 'Answer', 'Options']
	]).

	:- protected(open_ai_request/5).
	:- mode(open_ai_request(+atom, +list(pair), +compound, --compound, +list(compound)), one_or_error).
	:- info(open_ai_request/5, [
		comment is 'Hook predicate used to call the OpenAI client facade. Tests can override it to return canned responses.',
		argnames is ['OperationId', 'PathParameters', 'Body', 'Response', 'Options']
	]).

	:- uses(json_pointer, [
		evaluate/3
	]).

	:- uses(list, [
		select/3
	]).

	available :-
		catch(::models(_), error(_, _), fail).

	models(Models) :-
		context(Context),
		request_options([], Options),
		::open_ai_request(listModels, [], empty, Response, Options),
		response_json(Response, Context, JSON),
		model_ids(JSON, Context, Models).

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

	default_base_url('http://127.0.0.1:11434/v1').

	request_options(Options, Options) :-
		select(base_url(_), Options, _),
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
		max_tokens-16
	}))).

	response_json(Response, _Context, JSON) :-
		http_core::status(Response, status(200, 'OK')),
		http_core::body(Response, content('application/json', json(JSON))),
		!.
	response_json(Response, Context, _) :-
		throw(error(domain_error(ollama_response, Response), Context)).

	model_ids(JSON, Context, Models) :-
		(	evaluate([data], JSON, Data) ->
			model_ids_list(Data, Context, Models)
		;	throw(error(domain_error(ollama_models_response, JSON), Context))
		).

	model_ids_list([], _, []).
	model_ids_list([ModelJSON| ModelJSONs], Context, [Model| Models]) :-
		model_id(ModelJSON, Context, Model),
		model_ids_list(ModelJSONs, Context, Models).

	model_id(ModelJSON, Context, Model) :-
		(	evaluate([id], ModelJSON, Model),
			atom(Model) ->
			true
		;	throw(error(domain_error(ollama_model, ModelJSON), Context))
		).

:- end_object.
