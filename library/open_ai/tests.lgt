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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'Unit tests for the "open_ai" library.'
	]).

	:- uses(http_core, [
		body/2, header/3, status/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(open_ai).
	cover(open_ai_api).
	cover(open_ai_catalog).
	cover(open_ai_client).
	cover(open_ai_server(_)).

	test(open_ai_catalog_01, deterministic(Count == 161)) :-
		open_ai::operation_count(Count).

	test(open_ai_catalog_02, deterministic) :-
		open_ai_catalog::operation(createResponse, 'Responses', post, '/responses', _, [], request_body(_, _, _), _).

	test(open_ai_catalog_03, deterministic) :-
		open_ai_catalog::operation('CreateSkill', 'Skills', post, '/skills', _, [], request_body(_, _, _), _).

	test(open_ai_document_01, deterministic) :-
		open_ai::document(Document),
		open_api::validate_document(Document).

	test(open_ai_operation_url_01, deterministic(URL == 'https://api.openai.com/v1/responses/resp_123/input_items')) :-
		open_ai_client::operation_url(listInputItems, [response_id-'resp_123'], URL, []).

	test(open_ai_operation_url_02, deterministic(URL == 'http://127.0.0.1:8080/files/file_123/content')) :-
		open_ai_client::operation_url(downloadFile, [file_id-'file_123'], URL, [base_url('http://127.0.0.1:8080')]).

	test(open_ai_operation_url_03, error(domain_error(open_ai_path_parameter, file_id-123))) :-
		open_ai_client::operation_url(downloadFile, [file_id-123], [], _URL).

	test(open_ai_client_request_01, error(domain_error(open_ai_client_option, invalid_option))) :-
		open_ai_client::request(listModels, [], empty, _Response, [invalid_option]).

	test(open_ai_server_01, deterministic) :-
		Request = request(post, origin('/responses'), http(1, 1), [], content('application/json', json({model-'gpt-4.1'})), []),
		open_ai_server(sample_open_ai_backend)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({operation-createResponse, object-'test'}))).

	test(open_ai_server_02, deterministic) :-
		Request = request(get, origin('/missing'), http(1, 1), [], empty, []),
		open_ai_server(sample_open_ai_backend)::handle(Request, Response),
		status(Response, status(404, 'Not Found')).

	test(open_ai_server_provider_01, deterministic) :-
		open_ai_server(sample_open_ai_backend)::api_info(info('OpenAI API', _, _, _)).

	test(open_ai_server_provider_02, deterministic) :-
		open_ai_server(sample_open_ai_backend)::servers([server('/', 'OpenAI-compatible local server')]).

	test(open_ai_server_provider_03, deterministic) :-
		open_ai_server(sample_open_ai_backend)::security([['ApiKeyAuth'-[]]]).

	test(open_ai_server_provider_04, deterministic) :-
		open_ai_server(sample_open_ai_backend)::security_scheme('ApiKeyAuth', http(bearer)).

	test(open_ai_server_provider_05, fail) :-
		open_ai_server(sample_open_ai_backend)::schema(_Name, _Schema).

	test(open_ai_event_stream_01, deterministic(Event == event(response_output_text_delta, '{\"delta\":\"hi\"}'))) :-
		open_ai_event_stream::event('event: response_output_text_delta\ndata: {\"delta\":\"hi\"}\n\n', Event).

	test(open_ai_event_stream_02, deterministic(Frame == 'event: response.completed\ndata: {}\n\n')) :-
		open_ai_event_stream::frame(event('response.completed', '{}'), Frame).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(open_ai_client_request_02, deterministic) :-
			open_client_test_listener(Port, Listener),
			threaded_once(serve_client_test_once(Listener), Tag),
			local_base_url(Port, BaseURL),
			open_ai_client::request(
				listInputItems,
				[response_id-'resp_123'],
				empty,
				Response,
				[
					base_url(BaseURL),
					headers([x_test-yes]),
					api_key('key'),
					organization('org'),
					project('proj'),
					query([after-'item_1']),
					version(http(1, 1)),
					properties([]),
					connection_options([])
				]
			),
			threaded_exit(serve_client_test_once(Listener), Tag),
			http_socket_process::close_listener(Listener),
			status(Response, status(200, 'OK')),
			body(Response, content('application/json', json({method-get, path-'/responses/resp_123/input_items'}))).

		test(open_ai_client_request_03, deterministic) :-
			open_client_test_listener(Port, Listener),
			threaded_once(serve_client_test_once(Listener), Tag),
			local_base_url(Port, BaseURL),
			open_ai_client::request(
				createResponse,
				[],
				content('application/json', json({model-'gpt-4.1'})),
				Response,
				[base_url(BaseURL)]
			),
			threaded_exit(serve_client_test_once(Listener), Tag),
			http_socket_process::close_listener(Listener),
			status(Response, status(200, 'OK')),
			body(Response, content('application/json', json({method-post, path-'/responses'}))).

		test(open_ai_client_request_04, deterministic) :-
			open_client_test_listener(Port, Listener),
			threaded_once(serve_client_test_once(Listener), Tag),
			local_base_url(Port, BaseURL),
			open_ai_client::request(
				deleteFile,
				[file_id-'file_123'],
				empty,
				Response,
				[base_url(BaseURL)]
			),
			threaded_exit(serve_client_test_once(Listener), Tag),
			http_socket_process::close_listener(Listener),
			status(Response, status(200, 'OK')),
			body(Response, content('application/json', json({method-delete, path-'/files/file_123'}))).

		open_client_test_listener(Port, Listener) :-
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []).

		serve_client_test_once(Listener) :-
			catch(http_socket_process::serve_once(Listener, open_ai_client_test_handler, _ClientInfo), _, true).

		local_base_url(Port, URL) :-
			atomic_list_concat(['http://127.0.0.1:', Port], URL).

	:- endif.

:- end_object.
