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


:- object(yoda_backend,
	implements(open_ai_backend_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'OpenAI-compatible backend that rewrites a single user sentence in Yoda style.'
	]).

	:- public(yoda_sentence/2).
	:- mode(yoda_sentence(+atom, -atom), one).
	:- info(yoda_sentence/2, [
		comment is 'Converts a sentence atom into a lower-case Yoda-style sentence atom.',
		argnames is ['Sentence', 'YodaSentence']
	]).

	:- public(yoda_words/2).
	:- mode(yoda_words(+list(atom), -list(atom)), one).
	:- info(yoda_words/2, [
		comment is 'Reorders a list of lower-case sentence words around the first suitable pivot verb.',
		argnames is ['Words', 'YodaWords']
	]).

	:- uses(json_pointer, [
		evaluate/3
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	handle_open_ai(createChatCompletion, Request, Result) :-
		!,
		yoda_result(Request, Result).
	handle_open_ai(createResponse, Request, Result) :-
		!,
		yoda_result(Request, Result).
	handle_open_ai(OperationId, _Request, json(400, {error-{message-'Unsupported OpenAI operation.', operation-OperationId}})).

	yoda_result(Request, ok({choices-[{message-{content-YodaSentence}}]})) :-
		request_sentence(Request, Sentence),
		yoda_sentence(Sentence, YodaSentence).

	yoda_sentence(Sentence, YodaSentence) :-
		downcase_sentence(Sentence, LowercaseSentence),
		atom::split(LowercaseSentence, ' ', Words),
		yoda_words(Words, YodaWords),
		atomic_list_concat(YodaWords, ' ', YodaSentence).

	yoda_words(Words, YodaWords) :-
		(	append(Subject, [Verb| Complement], Words),
			Subject \== [],
			Complement \== [],
			pivot_verb(Verb) ->
			append(Complement, [Verb| Subject], YodaWords)
		;	YodaWords = Words
		).

	pivot_verb(am).    pivot_verb(is).     pivot_verb(are).
	pivot_verb(was).   pivot_verb(were).   pivot_verb(will).
	pivot_verb(would). pivot_verb(can).    pivot_verb(could).
	pivot_verb(shall). pivot_verb(should). pivot_verb(must).
	pivot_verb(do).    pivot_verb(does).   pivot_verb(did).
	pivot_verb(have).  pivot_verb(has).    pivot_verb(had).

	request_sentence(Request, Sentence) :-
		http_core::body(Request, content(_MediaType, json(JSON))),
		( 	evaluate([messages], JSON, Messages),
			user_message_sentences(Messages, Sentences),
			Sentences = [Sentence] ->
			true
		; 	throw(problem(400, 'urn:logtalk:yoda-server:invalid-request', 'Bad Request', 'Expected exactly one user message with string content.'))
		).

	user_message_sentences([], []).
	user_message_sentences([Message| Messages], Sentences) :-
		( 	evaluate([role], Message, user),
			evaluate([content], Message, Sentence),
			atom(Sentence) ->
			Sentences = [Sentence| Tail]
		; 	Sentences = Tail
		),
		user_message_sentences(Messages, Tail).

	downcase_sentence(Sentence, LowercaseSentence) :-
		atom_chars(Sentence, Chars),
		downcase_chars(Chars, LowercaseChars),
		atom_chars(LowercaseSentence, LowercaseChars).

	downcase_chars([], []).
	downcase_chars([Char| Chars], [LowercaseChar| LowercaseChars]) :-
		( 	'A' @=< Char, Char @=< 'Z' ->
			char_code(Char, Code),
			LowercaseCode is Code + 32,
			char_code(LowercaseChar, LowercaseCode)
		; 	LowercaseChar = Char
		),
		downcase_chars(Chars, LowercaseChars).

:- end_object.


:- object(yoda_open_ai_server,
	extends(open_ai_server(yoda_backend))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'OpenAI-compatible HTTP handler for the Yoda server example.'
	]).

:- end_object.


:- object(yoda_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Local HTTP server for the OpenAI-compatible Yoda backend.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	:- public(serve_until_shutdown/2).
	:- mode(serve_until_shutdown(?integer, +nonvar), one_or_error).
	:- info(serve_until_shutdown/2, [
		comment is 'Opens a local listener and serves accepted connections until ``stop/1`` is called for the given control term.',
		argnames is ['Port', 'Control']
	]).

	:- public(stop/1).
	:- mode(stop(+nonvar), one_or_error).
	:- info(stop/1, [
		comment is 'Requests shutdown of an open-ended server loop previously started with ``serve_until_shutdown/2``.',
		argnames is ['Control']
	]).

	:- private(server_listener_/2).
	:- dynamic(server_listener_/2).
	:- mode(server_listener_(?nonvar, ?compound), zero_or_more).
	:- info(server_listener_/2, [
		comment is 'Active listener for an open-ended server control term.',
		argnames is ['Control', 'Listener']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- public(start/3).
		:- mode(start(?integer, +nonvar, -integer), one_or_error).
		:- info(start/3, [
			comment is 'Starts the open-ended server in a worker thread, waits until the listener is accepting requests, and returns the worker tag.',
			argnames is ['Port', 'Control', 'Tag']
		]).

		:- public(stop/2).
		:- mode(stop(+nonvar, +integer), one_or_error).
		:- info(stop/2, [
			comment is 'Requests shutdown of an open-ended server loop and waits for the worker thread to finish.',
			argnames is ['Control', 'Tag']
		]).

		:- threaded.

	:- endif.

	serve(Port, Count) :-
		http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			http_socket_transport::serve_listener(Listener, yoda_open_ai_server, Count, _ClientInfos, [shutdown(close)]),
			Error,
			( 	catch(http_socket_transport::close_listener(Listener), _, true),
				throw(Error)
			)
		).

	serve_until_shutdown(Port, Control) :-
		http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
		register_server_listener(Control, Listener),
		serve_open_listener(Listener, Control, Port).

	serve_open_listener(Listener, Control, Port) :-
		catch(
			http_socket_transport::serve_until_shutdown(Listener, yoda_open_ai_server, Control, [], notify_server_ready(Control, Port)),
			Error,
			( 	retractall(server_listener_(Control, _)),
				catch(http_socket_transport::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		retractall(server_listener_(Control, _)).

	register_server_listener(Control, Listener) :-
		retractall(server_listener_(Control, _)),
		assertz(server_listener_(Control, Listener)).

	stop(Control) :-
		http_socket_transport::request_shutdown(Control).

	:- if(current_logtalk_flag(threads, supported)).

		start(Port, Control, Tag) :-
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			register_server_listener(Control, Listener),
			catch(
				( 	threaded_once(serve_open_listener(Listener, Control, Port), Tag),
					threaded_wait(yoda_server_ready(Control, Port))
				),
				Error,
				( 	retractall(server_listener_(Control, _)),
					catch(http_socket_transport::close_listener(Listener), _, true),
					throw(Error)
				)
			).

		stop(Control, Tag) :-
			stop(Control),
			wake_server_listener(Control),
			threaded_exit(serve_open_listener(_Listener, Control, _Port), Tag).

		wake_server_listener(Control) :-
			( 	server_listener_(Control, Listener) ->
				catch(http_socket_transport::request_listener_shutdown(Listener), _, true)
			; 	true
			).

		notify_server_ready(Control, Port) :-
			threaded_notify(yoda_server_ready(Control, Port)).

	:- else.

		notify_server_ready(_Control, _Port).

	:- endif.

:- end_object.
