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
		date is 2026-07-08,
		comment is 'Unit tests for the "yoda_server" example.'
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	:- uses(json_pointer, [
		evaluate/3
	]).

	:- uses(reader, [
		stream_to_codes/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	cover(yoda_backend).
	cover(yoda_open_ai_server).
	cover(yoda_server).

	test(yoda_server_words_01, deterministic(YodaWords == [learn, patience, will, you])) :-
		yoda_backend::yoda_words([you, will, learn, patience], YodaWords).

	test(yoda_server_words_02, deterministic(YodaWords == [hungry, am, i])) :-
		yoda_backend::yoda_words([i, am, hungry], YodaWords).

	test(yoda_server_words_03, deterministic(YodaWords == [have, must, patience, you])) :-
		yoda_backend::yoda_words([patience, you, must, have], YodaWords).

	test(yoda_server_sentence_01, deterministic(YodaSentence == 'learn patience will you')) :-
		yoda_backend::yoda_sentence('You will learn patience', YodaSentence).

	test(yoda_server_sentence_02, deterministic(YodaSentence == 'hungry am i')) :-
		yoda_backend::yoda_sentence('I am hungry', YodaSentence).

	test(yoda_server_sentence_03, deterministic(YodaSentence == 'have must patience you')) :-
		yoda_backend::yoda_sentence('Patience you must have', YodaSentence).

	test(yoda_server_handler_01, deterministic(YodaSentence == 'learn patience will you')) :-
		yoda_request('You will learn patience', JSON),
		Request = request(post, origin('/chat/completions'), http(1, 1), [], content('application/json', json(JSON)), []),
		yoda_open_ai_server::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json(ResponseJSON))),
		yoda_response(ResponseJSON, YodaSentence).

	test(yoda_server_handler_02, deterministic(YodaSentence == 'hungry am i')) :-
		yoda_request('I am hungry', JSON),
		Request = request(post, origin('/responses'), http(1, 1), [], content('application/json', json(JSON)), []),
		yoda_open_ai_server::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json(ResponseJSON))),
		yoda_response(ResponseJSON, YodaSentence).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(yoda_server_curl_chat_01, deterministic(YodaSentence == 'learn patience will you'), [condition(os::resolve_command_path(curl, _Path))]) :-
			curl_yoda('/chat/completions', 'You will learn patience', YodaSentence).

		test(yoda_server_curl_responses_01, deterministic(YodaSentence == 'hungry am i'), [condition(os::resolve_command_path(curl, _Path))]) :-
			curl_yoda('/responses', 'I am hungry', YodaSentence).

		curl_yoda(Path, Sentence, YodaSentence) :-
			Control = yoda_server_curl(Path, Sentence),
			yoda_server::start(Port, Control, Tag),
			catch(
				( 	atomic_list_concat(['http://127.0.0.1:', Port, Path], URL),
					yoda_request(Sentence, RequestJSON),
					curl_json(URL, RequestJSON, ResponseJSON)
				),
				Error,
				( 	cleanup_server(Control, Tag),
					throw(Error)
				)
			),
			yoda_server::stop(Control, Tag),
			yoda_response(ResponseJSON, YodaSentence).

		cleanup_server(Control, Tag) :-
			catch(yoda_server::stop(Control, Tag), _, true).

		curl_json(URL, RequestJSON, ResponseJSON) :-
			os::resolve_command_path(curl, Curl),
			json::generate(atom(RequestAtom), RequestJSON),
			process::create(
				Curl,
				['-sS', '--max-time', '10', '-H', 'Content-Type: application/json', '-H', 'Connection: close', '--data-binary', RequestAtom, URL],
				[stdout(Output), process(Process), type(text)]
			),
			stream_to_codes(Output, Codes),
			close(Output),
			process::wait(Process, Status),
			check_process_status(Status),
			atom_codes(ResponseAtom, Codes),
			json::parse(atom(ResponseAtom), ResponseJSON).

		check_process_status(Status) :-
			( 	process_status_success(Status) ->
				true
			; 	throw(error(domain_error(process_exit_status, Status), tests::curl_json/3))
			).

		process_status_success(0) :-
			!.
		process_status_success(exit(0)).

	:- endif.

	yoda_request(Sentence, {model-yoda, messages-[{role-user, content-Sentence}]}).

	yoda_response(JSON, YodaSentence) :-
		evaluate([choices, '0', message, content], JSON, YodaSentence).

:- end_object.
