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
		date is 2026-02-24,
		comment is 'Unit tests for the "json_rpc" library.'
	]).

	cover(json_rpc).

	cleanup :-
		^^clean_file('test_json_rpc.tmp'),
		^^clean_file('test_json_rpc_multi.tmp'),
		^^clean_file('test_json_rpc_eof.tmp'),
		^^clean_file('test_json_rpc_framed.tmp'),
		^^clean_file('test_json_rpc_framed_multi.tmp'),
		^^clean_file('test_json_rpc_framed_eof.tmp').

	% request construction tests

	test(json_rpc_request_4_01, true(Request == {jsonrpc-'2.0', method-subtract, params-[42,23], id-1})) :-
		json_rpc::request(subtract, [42,23], 1, Request).

	test(json_rpc_request_4_02, true(Request == {jsonrpc-'2.0', method-update, params-[1,2,3,4,5], id-2})) :-
		json_rpc::request(update, [1,2,3,4,5], 2, Request).

	test(json_rpc_request_4_03, true(Request == {jsonrpc-'2.0', method-foobar, params-{foo-bar, baz-42}, id-abc})) :-
		json_rpc::request(foobar, {foo-bar, baz-42}, abc, Request).

	test(json_rpc_request_3_01, true(Request == {jsonrpc-'2.0', method-foobar, id-1})) :-
		json_rpc::request(foobar, 1, Request).

	% notification construction tests

	test(json_rpc_notification_3_01, true(Notification == {jsonrpc-'2.0', method-update, params-[1,2,3,4,5]})) :-
		json_rpc::notification(update, [1,2,3,4,5], Notification).

	test(json_rpc_notification_2_01, true(Notification == {jsonrpc-'2.0', method-foobar})) :-
		json_rpc::notification(foobar, Notification).

	% response construction tests

	test(json_rpc_response_3_01, true(Response == {jsonrpc-'2.0', result-19, id-1})) :-
		json_rpc::response(19, 1, Response).

	test(json_rpc_response_3_02, true(Response == {jsonrpc-'2.0', result-[hello,5], id-9})) :-
		json_rpc::response([hello,5], 9, Response).

	% error response construction tests

	test(json_rpc_error_response_4_01, true(Response == {jsonrpc-'2.0', error-{code- -32601, message-'Method not found'}, id-1})) :-
		json_rpc::error_response(-32601, 'Method not found', 1, Response).

	test(json_rpc_error_response_4_02, true(Response == {jsonrpc-'2.0', error-{code- -32600, message-'Invalid Request'}, id- @null})) :-
		json_rpc::error_response(-32600, 'Invalid Request', @null, Response).

	test(json_rpc_error_response_5_01, true(Response == {jsonrpc-'2.0', error-{code- -32602, message-'Invalid params', data-extra_info}, id-3})) :-
		json_rpc::error_response(-32602, 'Invalid params', extra_info, 3, Response).

	% standard error constructor tests

	test(json_rpc_parse_error_1_01, true) :-
		json_rpc::parse_error(Response),
		json_rpc::error_code(Response, -32700),
		json_rpc::id(Response, @null).

	test(json_rpc_invalid_request_1_01, true) :-
		json_rpc::invalid_request(Response),
		json_rpc::error_code(Response, -32600),
		json_rpc::id(Response, @null).

	test(json_rpc_method_not_found_2_01, true) :-
		json_rpc::method_not_found(1, Response),
		json_rpc::error_code(Response, -32601),
		json_rpc::id(Response, 1).

	test(json_rpc_invalid_params_2_01, true) :-
		json_rpc::invalid_params(2, Response),
		json_rpc::error_code(Response, -32602),
		json_rpc::id(Response, 2).

	test(json_rpc_internal_error_2_01, true) :-
		json_rpc::internal_error(3, Response),
		json_rpc::error_code(Response, -32603),
		json_rpc::id(Response, 3).

	% encoding tests

	test(json_rpc_encode_2_01, true) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::encode(Request, JSON),
		atom(JSON).

	test(json_rpc_encode_2_02, true) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::encode(Notification, JSON),
		atom(JSON).

	test(json_rpc_encode_2_03, true) :-
		json_rpc::response(19, 1, Response),
		json_rpc::encode(Response, JSON),
		atom(JSON).

	% decoding tests

	test(json_rpc_decode_2_01, true(json_rpc::is_request(Term))) :-
		json_rpc::decode('{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}', Term).

	test(json_rpc_decode_2_02, true(json_rpc::is_notification(Term))) :-
		json_rpc::decode('{"jsonrpc":"2.0","method":"update","params":[1,2,3,4,5]}', Term).

	test(json_rpc_decode_2_03, true(json_rpc::is_response(Term))) :-
		json_rpc::decode('{"jsonrpc":"2.0","result":19,"id":1}', Term).

	test(json_rpc_decode_2_04, true(json_rpc::is_error_response(Term))) :-
		json_rpc::decode('{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":1}', Term).

	% encode/decode roundtrip tests

	test(json_rpc_roundtrip_request_01, true(json_rpc::is_request(Decoded))) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::encode(Request, JSON),
		json_rpc::decode(JSON, Decoded),
		json_rpc::method(Decoded, subtract),
		json_rpc::id(Decoded, 1).

	test(json_rpc_roundtrip_response_01, true(json_rpc::is_response(Decoded))) :-
		json_rpc::response(19, 1, Response),
		json_rpc::encode(Response, JSON),
		json_rpc::decode(JSON, Decoded),
		json_rpc::result(Decoded, 19),
		json_rpc::id(Decoded, 1).

	test(json_rpc_roundtrip_error_01, true(json_rpc::is_error_response(Decoded))) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::encode(ErrorResponse, JSON),
		json_rpc::decode(JSON, Decoded),
		json_rpc::error_code(Decoded, -32601),
		json_rpc::id(Decoded, 1).

	test(json_rpc_roundtrip_notification_01, true(json_rpc::is_notification(Decoded))) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::encode(Notification, JSON),
		json_rpc::decode(JSON, Decoded),
		json_rpc::method(Decoded, update).

	% message classification tests

	test(json_rpc_is_request_1_01, true) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::is_request(Request).

	test(json_rpc_is_request_1_02, fail) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::is_request(Notification).

	test(json_rpc_is_request_1_03, fail) :-
		json_rpc::response(19, 1, Response),
		json_rpc::is_request(Response).

	test(json_rpc_is_notification_1_01, true) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::is_notification(Notification).

	test(json_rpc_is_notification_1_02, fail) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::is_notification(Request).

	test(json_rpc_is_response_1_01, true) :-
		json_rpc::response(19, 1, Response),
		json_rpc::is_response(Response).

	test(json_rpc_is_response_1_02, fail) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::is_response(Request).

	test(json_rpc_is_error_response_1_01, true) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::is_error_response(ErrorResponse).

	test(json_rpc_is_error_response_1_02, fail) :-
		json_rpc::response(19, 1, Response),
		json_rpc::is_error_response(Response).

	test(json_rpc_is_batch_1_01, true) :-
		json_rpc::request(subtract, [42,23], 1, R1),
		json_rpc::request(update, [1], 2, R2),
		json_rpc::is_batch([R1, R2]).

	test(json_rpc_is_batch_1_02, fail) :-
		json_rpc::is_batch([]).

	% field extraction tests

	test(json_rpc_id_2_01, true(Id == 1)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::id(Request, Id).

	test(json_rpc_id_2_02, true(Id == @null)) :-
		json_rpc::error_response(-32700, 'Parse error', @null, Response),
		json_rpc::id(Response, Id).

	test(json_rpc_id_2_03, fail) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::id(Notification, _).

	test(json_rpc_method_2_01, true(Method == subtract)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::method(Request, Method).

	test(json_rpc_method_2_02, true(Method == update)) :-
		json_rpc::notification(update, [1,2,3], Notification),
		json_rpc::method(Notification, Method).

	test(json_rpc_params_2_01, true(Params == [42,23])) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::params(Request, Params).

	test(json_rpc_params_2_02, fail) :-
		json_rpc::request(foobar, 1, Request),
		json_rpc::params(Request, _).

	test(json_rpc_result_2_01, true(Result == 19)) :-
		json_rpc::response(19, 1, Response),
		json_rpc::result(Response, Result).

	test(json_rpc_error_2_01, true) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::error(ErrorResponse, Error),
		json_rpc::is_error_response(ErrorResponse),
		Error = {code- -32601, message-'Method not found'}.

	test(json_rpc_error_code_2_01, true(Code == -32601)) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::error_code(ErrorResponse, Code).

	test(json_rpc_error_message_2_01, true(Message == 'Method not found')) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::error_message(ErrorResponse, Message).

	test(json_rpc_error_data_2_01, true(Data == extra_info)) :-
		json_rpc::error_response(-32602, 'Invalid params', extra_info, 3, ErrorResponse),
		json_rpc::error_data(ErrorResponse, Data).

	test(json_rpc_error_data_2_02, fail) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		json_rpc::error_data(ErrorResponse, _).

	% batch encoding tests

	test(json_rpc_encode_batch_01, true(atom(JSON))) :-
		json_rpc::request(subtract, [42,23], 1, R1),
		json_rpc::notification(update, [1,2,3], N1),
		json_rpc::encode([R1, N1], JSON).

	test(json_rpc_encode_2_04, true(atom(JSON))) :-
		json_rpc::encode([], JSON).

	% stream I/O tests

	test(json_rpc_write_read_message_01, true(json_rpc::is_request(ReadMsg))) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		^^file_path('test_json_rpc.tmp', File),
		open(File, write, Output),
		json_rpc::write_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg),
		close(Input).

	test(json_rpc_write_read_message_02, true(Method-Id == subtract-1)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		^^file_path('test_json_rpc.tmp', File),
		open(File, write, Output),
		json_rpc::write_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg),
		close(Input),
		json_rpc::method(ReadMsg, Method),
		json_rpc::id(ReadMsg, Id).

	test(json_rpc_write_read_multiple_messages_01, true(M1-M2 == subtract-update)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::notification(update, [1,2,3], Notification),
		^^file_path('test_json_rpc_multi.tmp', File),
		open(File, write, Output),
		json_rpc::write_message(Output, Request),
		json_rpc::write_message(Output, Notification),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg1),
		json_rpc::read_message(Input, ReadMsg2),
		close(Input),
		json_rpc::method(ReadMsg1, M1),
		json_rpc::method(ReadMsg2, M2).

	test(json_rpc_read_message_eof_01, false, [cleanup(close(in))]) :-
		^^file_path('test_json_rpc_eof.tmp', File),
		open(File, write, Output),
		close(Output),
		open(File, read, Input, [alias(in)]),
		json_rpc::read_message(Input, _).

	% Content-Length framed I/O tests

	% Write and read back a single framed request
	test(json_rpc_write_read_framed_message_01, true(json_rpc::is_request(ReadMsg))) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		^^file_path('test_json_rpc_framed.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg),
		close(Input).

	% Verify field values survive the framed write/read roundtrip
	test(json_rpc_write_read_framed_message_02, true(Method-Id == subtract-1)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		^^file_path('test_json_rpc_framed.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg),
		close(Input),
		json_rpc::method(ReadMsg, Method),
		json_rpc::id(ReadMsg, Id).

	% Framed roundtrip preserves a response with a structured result
	test(json_rpc_write_read_framed_message_03, true(Result == 19)) :-
		json_rpc::response(19, 1, Response),
		^^file_path('test_json_rpc_framed.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Response),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg),
		close(Input),
		json_rpc::result(ReadMsg, Result).

	% Framed roundtrip preserves an error response
	test(json_rpc_write_read_framed_message_04, true(Code == -32601)) :-
		json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse),
		^^file_path('test_json_rpc_framed.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, ErrorResponse),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg),
		close(Input),
		json_rpc::error_code(ReadMsg, Code).

	% Framed roundtrip preserves a notification (no id)
	test(json_rpc_write_read_framed_message_05, true(json_rpc::is_notification(ReadMsg))) :-
		json_rpc::notification(update, [1,2,3], Notification),
		^^file_path('test_json_rpc_framed.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Notification),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg),
		close(Input).

	% Write and read multiple framed messages sequentially
	test(json_rpc_write_read_framed_multiple_01, true(M1-M2 == subtract-update)) :-
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::notification(update, [1,2,3], Notification),
		^^file_path('test_json_rpc_framed_multi.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Request),
		json_rpc::write_framed_message(Output, Notification),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, ReadMsg1),
		json_rpc::read_framed_message(Input, ReadMsg2),
		close(Input),
		json_rpc::method(ReadMsg1, M1),
		json_rpc::method(ReadMsg2, M2).

	% Three framed messages in sequence with mixed types
	test(json_rpc_write_read_framed_multiple_02, true(R1-R2-R3 == request-response-notification)) :-
		json_rpc::request(foo, [1], 1, Req),
		json_rpc::response(42, 1, Resp),
		json_rpc::notification(bar, [2], Notif),
		^^file_path('test_json_rpc_framed_multi.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Req),
		json_rpc::write_framed_message(Output, Resp),
		json_rpc::write_framed_message(Output, Notif),
		close(Output),
		open(File, read, Input),
		json_rpc::read_framed_message(Input, R1Msg),
		json_rpc::read_framed_message(Input, R2Msg),
		json_rpc::read_framed_message(Input, R3Msg),
		close(Input),
		(json_rpc::is_request(R1Msg) -> R1 = request ; R1 = other),
		(json_rpc::is_response(R2Msg) -> R2 = response ; R2 = other),
		(json_rpc::is_notification(R3Msg) -> R3 = notification ; R3 = other).

	% Reading from an empty file fails
	test(json_rpc_read_framed_message_eof_01, false, [cleanup(close(in))]) :-
		^^file_path('test_json_rpc_framed_eof.tmp', File),
		open(File, write, Output),
		close(Output),
		open(File, read, Input, [alias(in)]),
		json_rpc::read_framed_message(Input, _).

	% After reading all framed messages, a subsequent read fails
	test(json_rpc_read_framed_message_eof_02, false, [cleanup(close(in))]) :-
		json_rpc::request(foo, [1], 1, Request),
		^^file_path('test_json_rpc_framed_eof.tmp', File),
		open(File, write, Output),
		json_rpc::write_framed_message(Output, Request),
		close(Output),
		open(File, read, Input, [alias(in)]),
		json_rpc::read_framed_message(Input, _),
		% Second read should fail (no more messages)
		json_rpc::read_framed_message(Input, _).

:- end_object.
