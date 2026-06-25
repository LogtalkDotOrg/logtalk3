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
			{id-'nomic-embed-text:latest', object-model},
			{id-'qwen2.5:0.5b', object-model}
		], Response).
	open_ai_request(createChatCompletion, [], content('application/json', json({
		model-'llama3.2:latest',
		messages-[
			{role-system, content-'Answer briefly.'},
			{role-user, content-'2+2?'}
		],
		stream- @false,
		max_tokens-16,
		reasoning_effort-'none'
	})), Response, Options) :-
		Options = [base_url('http://example.com/v1')],
		chat_response('4.', Response).

	ollama_request(showModel, content('application/json', json({model-'llama3.2:latest'})), Response, Options) :-
		Options = [base_url('http://127.0.0.1:11434/v1')],
		show_response([completion], Response).
	ollama_request(showModel, content('application/json', json({model-'nomic-embed-text:latest'})), Response, Options) :-
		Options = [base_url('http://127.0.0.1:11434/v1')],
		show_response([embedding], Response).
	ollama_request(showModel, content('application/json', json({model-'qwen2.5:0.5b'})), Response, Options) :-
		Options = [base_url('http://127.0.0.1:11434/v1')],
		show_response([completion, tools], Response).

	models_response(Models, Response) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({object-list, data-Models})), [], Response).

	show_response(Capabilities, Response) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({capabilities-Capabilities})), [], Response).

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


:- object(malformed_show_ollama_client,
	extends(ollama_client)).

	open_ai_request(listModels, [], empty, Response, _Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({object-list, data-[{id-'llama3.2:latest', object-model}]})), [], Response).
	ollama_request(showModel, content('application/json', json({model-'llama3.2:latest'})), Response, _Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], content('application/json', json({details-{family-llama}})), [], Response).

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

	test(ollama_client_models_04, error(domain_error(ollama_model_capabilities_response, _))) :-
		malformed_show_ollama_client::models(_Models).

	test(ollama_client_ask_01, deterministic(Answer == '4.')) :-
		mock_ollama_client::ask('llama3.2:latest', '2+2?', Answer, [base_url('http://example.com/v1')]).

	test(ollama_client_ask_02, error(domain_error(ollama_chat_response, _))) :-
		unexpected_ollama_client::ask('llama3.2:latest', '2+2?', _Answer).

	test(ollama_client_live_available_01, deterministic, [condition(live_available)]) :-
		live_test_options(Options),
		ollama_client::models(_Models, Options).

	test(ollama_client_live_ask_01, deterministic(non_empty_atom(Answer)), [condition(live_test_model(_Model, _Options))]) :-
		live_test_model(Model, Options),
		ollama_client::ask(Model, '2+2?', Answer, Options).

	test(ollama_client_live_ask_02, deterministic(non_empty_atom(Answer)), [condition(live_test_model(_Model, _Options))]) :-
		live_test_model(Model, Options),
		ollama_client::ask(Model, 'Say OK.', Answer, Options).

	live_available :-
		live_test_options(Options),
		ollama_client::available(Options).

	live_test_model(Model, Options) :-
		live_test_options(Options),
		catch(ollama_client::models(Models, Options), _, fail),
		(	os::environment_variable('OLLAMA_CLIENT_TEST_MODEL', Model),
			list::member(Model, Models) ->
			true
		;	Models = [Model| _]
		).

	live_test_options([base_url(URL)]) :-
		os::environment_variable('OLLAMA_SERVER_URL', URL),
		URL \== '',
		!.
	live_test_options([]).

	non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

:- end_object.
