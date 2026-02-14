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


:- object(json_rpc).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-14,
		comment is 'JSON-RPC 2.0 protocol encoding and decoding. Provides predicates for constructing and parsing JSON-RPC 2.0 request, notification, response, and error objects. Uses the ``json`` library for JSON parsing and generation.',
		remarks is [
			'Specification' - 'Implements the JSON-RPC 2.0 specification: https://www.jsonrpc.org/specification',
			'JSON representation' - 'Uses the ``json`` library default representation: curly terms for objects, dashes for pairs, and atoms for strings.',
			'Request' - 'A JSON-RPC 2.0 request is represented as ``{jsonrpc-''2.0'', method-Method, params-Params, id-Id}``.',
			'Notification' - 'A JSON-RPC 2.0 notification is represented as ``{jsonrpc-''2.0'', method-Method, params-Params}``.',
			'Successful response' - 'A JSON-RPC 2.0 successful response is represented as ``{jsonrpc-''2.0'', result-Result, id-Id}``.',
			'Error response' - 'A JSON-RPC 2.0 error response is represented as ``{jsonrpc-''2.0'', error-{code-Code, message-Message}, id-Id}`` or ``{jsonrpc-''2.0'', error-{code-Code, message-Message, data-Data}, id-Id}``.',
			'Batch request' - 'A JSON-RPC 2.0 batch request is represented as a list of request and/or notification terms.',
			'Error codes' - 'Standard error codes: -32700 (parse error), -32600 (invalid request), -32601 (method not found), -32602 (invalid params), -32603 (internal error). Server errors: -32000 to -32099.'
		]
	]).

	:- uses(json, [
		parse/2, generate/2
	]).

	:- uses(list, [
		valid/1 as is_list/1
	]).

	:- uses(reader, [
		line_to_chars/2
	]).

	% request construction

	:- public(request/4).
	:- mode(request(+atom, +list, +nonvar, --compound), one).
	:- info(request/4, [
		comment is 'Constructs a JSON-RPC 2.0 request term.',
		argnames is ['Method', 'Params', 'Id', 'Request']
	]).

	:- public(request/3).
	:- mode(request(+atom, +nonvar, --compound), one).
	:- info(request/3, [
		comment is 'Constructs a JSON-RPC 2.0 request term with no parameters.',
		argnames is ['Method', 'Id', 'Request']
	]).

	request(Method, Params, Id, {jsonrpc-'2.0', method-Method, params-Params, id-Id}).

	request(Method, Id, {jsonrpc-'2.0', method-Method, id-Id}).

	% notification construction

	:- public(notification/3).
	:- mode(notification(+atom, +list, --compound), one).
	:- info(notification/3, [
		comment is 'Constructs a JSON-RPC 2.0 notification term (a request without an id).',
		argnames is ['Method', 'Params', 'Notification']
	]).

	:- public(notification/2).
	:- mode(notification(+atom, --compound), one).
	:- info(notification/2, [
		comment is 'Constructs a JSON-RPC 2.0 notification term with no parameters.',
		argnames is ['Method', 'Notification']
	]).

	notification(Method, Params, {jsonrpc-'2.0', method-Method, params-Params}).

	notification(Method, {jsonrpc-'2.0', method-Method}).

	% response construction

	:- public(response/3).
	:- mode(response(+nonvar, +nonvar, --compound), one).
	:- info(response/3, [
		comment is 'Constructs a JSON-RPC 2.0 successful response term.',
		argnames is ['Result', 'Id', 'Response']
	]).

	response(Result, Id, {jsonrpc-'2.0', result-Result, id-Id}).

	% error response construction

	:- public(error_response/4).
	:- mode(error_response(+integer, +atom, +nonvar, --compound), one).
	:- info(error_response/4, [
		comment is 'Constructs a JSON-RPC 2.0 error response term with a null id (used when the request id cannot be determined).',
		argnames is ['Code', 'Message', 'Id', 'ErrorResponse']
	]).

	:- public(error_response/5).
	:- mode(error_response(+integer, +atom, +nonvar, +nonvar, --compound), one).
	:- info(error_response/5, [
		comment is 'Constructs a JSON-RPC 2.0 error response term with additional error data.',
		argnames is ['Code', 'Message', 'Data', 'Id', 'ErrorResponse']
	]).

	error_response(Code, Message, Id, {jsonrpc-'2.0', error-{code-Code, message-Message}, id-Id}).

	error_response(Code, Message, Data, Id, {jsonrpc-'2.0', error-{code-Code, message-Message, data-Data}, id-Id}).

	% standard error constructors

	:- public(parse_error/1).
	:- mode(parse_error(--compound), one).
	:- info(parse_error/1, [
		comment is 'Constructs a JSON-RPC 2.0 parse error response (-32700) with a null id.',
		argnames is ['ErrorResponse']
	]).

	:- public(invalid_request/1).
	:- mode(invalid_request(--compound), one).
	:- info(invalid_request/1, [
		comment is 'Constructs a JSON-RPC 2.0 invalid request error response (-32600) with a null id.',
		argnames is ['ErrorResponse']
	]).

	:- public(method_not_found/2).
	:- mode(method_not_found(+nonvar, --compound), one).
	:- info(method_not_found/2, [
		comment is 'Constructs a JSON-RPC 2.0 method not found error response (-32601).',
		argnames is ['Id', 'ErrorResponse']
	]).

	:- public(invalid_params/2).
	:- mode(invalid_params(+nonvar, --compound), one).
	:- info(invalid_params/2, [
		comment is 'Constructs a JSON-RPC 2.0 invalid params error response (-32602).',
		argnames is ['Id', 'ErrorResponse']
	]).

	:- public(internal_error/2).
	:- mode(internal_error(+nonvar, --compound), one).
	:- info(internal_error/2, [
		comment is 'Constructs a JSON-RPC 2.0 internal error response (-32603).',
		argnames is ['Id', 'ErrorResponse']
	]).

	parse_error(Response) :-
		error_response(-32700, 'Parse error', @null, Response).

	invalid_request(Response) :-
		error_response(-32600, 'Invalid Request', @null, Response).

	method_not_found(Id, Response) :-
		error_response(-32601, 'Method not found', Id, Response).

	invalid_params(Id, Response) :-
		error_response(-32602, 'Invalid params', Id, Response).

	internal_error(Id, Response) :-
		error_response(-32603, 'Internal error', Id, Response).

	% encoding (term to JSON)

	:- public(encode/2).
	:- mode(encode(+compound, --atom), one).
	:- info(encode/2, [
		comment is 'Encodes a JSON-RPC 2.0 term (request, notification, response, error, or batch) into a JSON atom.',
		argnames is ['Term', 'JSON']
	]).

	encode(Terms, JSON) :-
		is_list(Terms),
		!,
		generate(atom(JSON), Terms).
	encode(Term, JSON) :-
		generate(atom(JSON), Term).

	% decoding (JSON to term)

	:- public(decode/2).
	:- mode(decode(+atom, --compound), one_or_error).
	:- info(decode/2, [
		comment is 'Decodes a JSON atom into a JSON-RPC 2.0 term (request, notification, response, error, or batch).',
		argnames is ['JSON', 'Term']
	]).

	decode(JSON, Term) :-
		parse(atom(JSON), Term).

	% message classification

	:- public(is_request/1).
	:- mode(is_request(+compound), zero_or_one).
	:- info(is_request/1, [
		comment is 'True if the term is a valid JSON-RPC 2.0 request (has jsonrpc, method, and id fields).',
		argnames is ['Term']
	]).

	:- public(is_notification/1).
	:- mode(is_notification(+compound), zero_or_one).
	:- info(is_notification/1, [
		comment is 'True if the term is a valid JSON-RPC 2.0 notification (has jsonrpc and method fields but no id field).',
		argnames is ['Term']
	]).

	:- public(is_response/1).
	:- mode(is_response(+compound), zero_or_one).
	:- info(is_response/1, [
		comment is 'True if the term is a valid JSON-RPC 2.0 successful response (has jsonrpc, result, and id fields).',
		argnames is ['Term']
	]).

	:- public(is_error_response/1).
	:- mode(is_error_response(+compound), zero_or_one).
	:- info(is_error_response/1, [
		comment is 'True if the term is a valid JSON-RPC 2.0 error response (has jsonrpc, error, and id fields).',
		argnames is ['Term']
	]).

	:- public(is_batch/1).
	:- mode(is_batch(+compound), zero_or_one).
	:- info(is_batch/1, [
		comment is 'True if the term is a valid JSON-RPC 2.0 batch (a non-empty list).',
		argnames is ['Term']
	]).

	is_request(Term) :-
		has_pair(Term, jsonrpc, '2.0'),
		has_pair(Term, method, Method),
		atom(Method),
		has_pair(Term, id, _).

	is_notification(Term) :-
		has_pair(Term, jsonrpc, '2.0'),
		has_pair(Term, method, Method),
		atom(Method),
		\+ has_pair(Term, id, _).

	is_response(Term) :-
		has_pair(Term, jsonrpc, '2.0'),
		has_pair(Term, result, _),
		has_pair(Term, id, _).

	is_error_response(Term) :-
		has_pair(Term, jsonrpc, '2.0'),
		has_pair(Term, error, _),
		has_pair(Term, id, _).

	is_batch([Head| _]) :-
		nonvar(Head).

	% field extraction

	:- public(id/2).
	:- mode(id(+compound, --nonvar), zero_or_one).
	:- info(id/2, [
		comment is 'Extracts the id field from a JSON-RPC 2.0 message.',
		argnames is ['Message', 'Id']
	]).

	:- public(method/2).
	:- mode(method(+compound, --atom), zero_or_one).
	:- info(method/2, [
		comment is 'Extracts the method field from a JSON-RPC 2.0 request or notification.',
		argnames is ['Message', 'Method']
	]).

	:- public(params/2).
	:- mode(params(+compound, --nonvar), zero_or_one).
	:- info(params/2, [
		comment is 'Extracts the params field from a JSON-RPC 2.0 request or notification.',
		argnames is ['Message', 'Params']
	]).

	:- public(result/2).
	:- mode(result(+compound, --nonvar), zero_or_one).
	:- info(result/2, [
		comment is 'Extracts the result field from a JSON-RPC 2.0 response.',
		argnames is ['Message', 'Result']
	]).

	:- public(error/2).
	:- mode(error(+compound, --compound), zero_or_one).
	:- info(error/2, [
		comment is 'Extracts the error field from a JSON-RPC 2.0 error response.',
		argnames is ['Message', 'Error']
	]).

	:- public(error_code/2).
	:- mode(error_code(+compound, --integer), zero_or_one).
	:- info(error_code/2, [
		comment is 'Extracts the error code from a JSON-RPC 2.0 error response.',
		argnames is ['Message', 'Code']
	]).

	:- public(error_message/2).
	:- mode(error_message(+compound, --atom), zero_or_one).
	:- info(error_message/2, [
		comment is 'Extracts the error message from a JSON-RPC 2.0 error response.',
		argnames is ['Message', 'ErrorMessage']
	]).

	:- public(error_data/2).
	:- mode(error_data(+compound, --nonvar), zero_or_one).
	:- info(error_data/2, [
		comment is 'Extracts the error data from a JSON-RPC 2.0 error response. Fails if no data field is present.',
		argnames is ['Message', 'Data']
	]).

	id(Message, Id) :-
		has_pair(Message, id, Id).

	method(Message, Method) :-
		has_pair(Message, method, Method).

	params(Message, Params) :-
		has_pair(Message, params, Params).

	result(Message, Result) :-
		has_pair(Message, result, Result).

	error(Message, Error) :-
		has_pair(Message, error, Error).

	error_code(Message, Code) :-
		has_pair(Message, error, Error),
		has_pair(Error, code, Code).

	error_message(Message, ErrorMessage) :-
		has_pair(Message, error, Error),
		has_pair(Error, message, ErrorMessage).

	error_data(Message, Data) :-
		has_pair(Message, error, Error),
		has_pair(Error, data, Data).

	% stream I/O (server and client API)

	:- public(write_message/2).
	:- mode(write_message(+stream, +compound), one).
	:- info(write_message/2, [
		comment is 'Writes a JSON-RPC 2.0 message to an output stream as a single line of JSON followed by a newline. Flushes the output stream after writing.',
		argnames is ['Output', 'Message']
	]).

	:- public(read_message/2).
	:- mode(read_message(+stream, --compound), zero_or_one).
	:- info(read_message/2, [
		comment is 'Reads a JSON-RPC 2.0 message from an input stream. Reads a line of JSON text and parses it. Fails at end of stream.',
		argnames is ['Input', 'Message']
	]).

	write_message(Output, Message) :-
		generate(codes(Codes), Message),
		write_codes(Codes, Output),
		put_code(Output, 0'\n),
		flush_output(Output).

	read_message(Input, Message) :-
		read_line_to_atom(Input, Atom),
		Atom \== end_of_file,
		parse(atom(Atom), Message).

	% auxiliary predicates for curly-term pair lookup

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :- !.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

	% Read a line of text from a stream into an atom
	read_line_to_atom(Input, Atom) :-
		line_to_chars(Input, Chars),
		(	Chars == end_of_file ->
			Atom = end_of_file
		;	atom_chars(Atom, Chars)
		).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

:- end_object.
