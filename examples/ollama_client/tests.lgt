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


:- object(mock_ollama_client,
	extends(ollama_client)).

	open_ai_request(listModels, [], empty, Response, Options) :-
		Options = [base_url('http://127.0.0.1:11434/v1')],
		models_response([
			{id-'llama3.2:latest', object-model},
			{id-'qwen2.5:0.5b', object-model}
		], Response).
	open_ai_request(createChatCompletion, [], content('application/json', json({
		model-'llama3.2:latest',
		messages-[
			{role-system, content-'Answer briefly.'},
			{role-user, content-'2+2?'}
		],
		stream- @false,
		max_tokens-16
	})), Response, Options) :-
		Options = [base_url('http://example.com/v1')],
		chat_response('4.', Response).

	models_response(Models, Response) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({object-list, data-Models})), [], Response).

	chat_response(Answer, Response) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({choices-[{message-{content-Answer}}]})), [], Response).

:- end_object.


:- object(empty_ollama_client,
	extends(ollama_client)).

	open_ai_request(listModels, [], empty, Response, _Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({object-list, data-[]})), [], Response).

:- end_object.


:- object(unexpected_ollama_client,
	extends(ollama_client)).

	open_ai_request(listModels, [], empty, Response, _Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({models-[]})), [], Response).
	open_ai_request(createChatCompletion, [], _Body, Response, _Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({choices-[]})), [], Response).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'Unit tests for the "ollama_client" example.'
	]).

	cover(ollama_client).

	test(ollama_client_models_01, deterministic(Models == ['llama3.2:latest', 'qwen2.5:0.5b'])) :-
		mock_ollama_client::models(Models).

	test(ollama_client_models_02, deterministic(Models == [])) :-
		empty_ollama_client::models(Models).

	test(ollama_client_models_03, error(domain_error(ollama_models_response, _))) :-
		unexpected_ollama_client::models(_Models).

	test(ollama_client_ask_01, deterministic(Answer == '4.')) :-
		mock_ollama_client::ask('llama3.2:latest', '2+2?', Answer, [base_url('http://example.com/v1')]).

	test(ollama_client_ask_02, error(domain_error(ollama_chat_response, _))) :-
		unexpected_ollama_client::ask('llama3.2:latest', '2+2?', _Answer).

	test(ollama_client_live_available_01, deterministic, [condition(ollama_client::available)]) :-
		ollama_client::models(_Models).

	test(ollama_client_live_ask_01, deterministic(non_empty_atom(Answer)), [condition(live_test_model(_Model))]) :-
		live_test_model(Model),
		ollama_client::ask(Model, '2+2?', Answer).

	test(ollama_client_live_ask_02, deterministic(non_empty_atom(Answer)), [condition(live_test_model(_Model))]) :-
		live_test_model(Model),
		ollama_client::ask(Model, 'Say OK.', Answer).

	live_test_model(Model) :-
		catch(ollama_client::models(Models), _, fail),
		(	os::environment_variable('OLLAMA_CLIENT_TEST_MODEL', Model),
			list::member(Model, Models) ->
			true
		;	preferred_live_model(Models, Model)
		).

	preferred_live_model(Models, Model) :-
		preferred_chat_model(Model),
		list::member(Model, Models),
		!.

	preferred_chat_model('llama3.2:latest').
	preferred_chat_model('llama3.1:latest').
	preferred_chat_model('llama3:latest').
	preferred_chat_model('mistral-nemo:latest').
	preferred_chat_model('mistral:latest').
	preferred_chat_model('gemma3:latest').
	preferred_chat_model('gemma2:latest').
	preferred_chat_model('phi4:latest').
	preferred_chat_model('phi3:latest').
	preferred_chat_model('qwen2.5:latest').
	preferred_chat_model('qwen2.5:0.5b').

	non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

:- end_object.
