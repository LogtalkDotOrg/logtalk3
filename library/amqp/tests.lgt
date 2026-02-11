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
		date is 2026-02-11,
		comment is 'Unit tests for the "amqp" library.'
	]).

	:- uses(list, [
		valid/1 as is_list/1
	]).

	cover(amqp).

	% ==========================================================================
	% Binary Encoding Tests - Octet
	% ==========================================================================

	test(amqp_encode_octet_01, true(Bytes == [0])) :-
		amqp<<encode_octet(0, Bytes).

	test(amqp_encode_octet_02, true(Bytes == [255])) :-
		amqp<<encode_octet(255, Bytes).

	test(amqp_encode_octet_03, true(Bytes == [127])) :-
		amqp<<encode_octet(127, Bytes).

	test(amqp_decode_octet_01, true(Value == 0)) :-
		amqp<<decode_octet([0, 99, 99], Value, _).

	test(amqp_decode_octet_02, true(Value == 255)) :-
		amqp<<decode_octet([255], Value, _).

	test(amqp_decode_octet_03, true(Rest == [1, 2, 3])) :-
		amqp<<decode_octet([42, 1, 2, 3], _, Rest).

	% ==========================================================================
	% Binary Encoding Tests - Short (16-bit)
	% ==========================================================================

	test(amqp_encode_short_01, true(Bytes == [0, 0])) :-
		amqp<<encode_short(0, Bytes).

	test(amqp_encode_short_02, true(Bytes == [255, 255])) :-
		amqp<<encode_short(65535, Bytes).

	test(amqp_encode_short_03, true(Bytes == [1, 0])) :-
		amqp<<encode_short(256, Bytes).

	test(amqp_encode_short_04, true(Bytes == [0, 255])) :-
		amqp<<encode_short(255, Bytes).

	test(amqp_decode_short_01, true(Value == 0)) :-
		amqp<<decode_short([0, 0], Value, _).

	test(amqp_decode_short_02, true(Value == 65535)) :-
		amqp<<decode_short([255, 255], Value, _).

	test(amqp_decode_short_03, true(Value == 256)) :-
		amqp<<decode_short([1, 0, 99], Value, _).

	test(amqp_decode_short_04, true(Rest == [99, 100])) :-
		amqp<<decode_short([0, 1, 99, 100], _, Rest).

	% ==========================================================================
	% Binary Encoding Tests - Long (32-bit)
	% ==========================================================================

	test(amqp_encode_long_01, true(Bytes == [0, 0, 0, 0])) :-
		amqp<<encode_long(0, Bytes).

	test(amqp_encode_long_02, true(Bytes == [0, 0, 1, 0])) :-
		amqp<<encode_long(256, Bytes).

	test(amqp_encode_long_03, true(Bytes == [0, 1, 0, 0])) :-
		amqp<<encode_long(65536, Bytes).

	test(amqp_encode_long_04, true(Bytes == [0, 0, 255, 255])) :-
		amqp<<encode_long(65535, Bytes).

	test(amqp_decode_long_01, true(Value == 0)) :-
		amqp<<decode_long([0, 0, 0, 0], Value, _).

	test(amqp_decode_long_02, true(Value == 256)) :-
		amqp<<decode_long([0, 0, 1, 0], Value, _).

	test(amqp_decode_long_03, true(Value == 16777216)) :-
		amqp<<decode_long([1, 0, 0, 0], Value, _).

	test(amqp_decode_long_04, true(Rest == [1, 2])) :-
		amqp<<decode_long([0, 0, 0, 42, 1, 2], _, Rest).

	% ==========================================================================
	% Binary Encoding Tests - Long Long (64-bit)
	% ==========================================================================

	test(amqp_encode_longlong_01, true(Bytes == [0, 0, 0, 0, 0, 0, 0, 0])) :-
		amqp<<encode_longlong(0, Bytes).

	test(amqp_encode_longlong_02, true(Bytes == [0, 0, 0, 0, 0, 0, 1, 0])) :-
		amqp<<encode_longlong(256, Bytes).

	test(amqp_decode_longlong_01, true(Value == 0)) :-
		amqp<<decode_longlong([0, 0, 0, 0, 0, 0, 0, 0], Value, _).

	test(amqp_decode_longlong_02, true(Value == 256)) :-
		amqp<<decode_longlong([0, 0, 0, 0, 0, 0, 1, 0], Value, _).

	% ==========================================================================
	% Binary Encoding Tests - Short String
	% ==========================================================================

	test(amqp_encode_shortstr_01, true(Bytes == [0])) :-
		amqp<<encode_shortstr('', Bytes).

	test(amqp_encode_shortstr_02, true(Bytes == [5, 104, 101, 108, 108, 111])) :-
		amqp<<encode_shortstr(hello, Bytes).

	test(amqp_encode_shortstr_03, true(Bytes == [3, 97, 98, 99])) :-
		amqp<<encode_shortstr(abc, Bytes).

	test(amqp_decode_shortstr_01, true(Atom == '')) :-
		amqp<<decode_shortstr([0], Atom, _).

	test(amqp_decode_shortstr_02, true(Atom == hello)) :-
		amqp<<decode_shortstr([5, 104, 101, 108, 108, 111], Atom, _).

	test(amqp_decode_shortstr_03, true(Rest == [99, 100])) :-
		amqp<<decode_shortstr([2, 97, 98, 99, 100], _, Rest).

	% ==========================================================================
	% Binary Encoding Tests - Long String
	% ==========================================================================

	test(amqp_encode_longstr_01, true(Bytes == [0, 0, 0, 0])) :-
		amqp<<encode_longstr('', Bytes).

	test(amqp_encode_longstr_02, true(Bytes == [0, 0, 0, 5, 104, 101, 108, 108, 111])) :-
		amqp<<encode_longstr(hello, Bytes).

	test(amqp_decode_longstr_01, true(Atom == '')) :-
		amqp<<decode_longstr([0, 0, 0, 0], Atom, _).

	test(amqp_decode_longstr_02, true(Atom == hello)) :-
		amqp<<decode_longstr([0, 0, 0, 5, 104, 101, 108, 108, 111], Atom, _).

	% ==========================================================================
	% Binary Encoding Tests - Field Table
	% ==========================================================================

	test(amqp_encode_table_01, true(Bytes == [0, 0, 0, 0])) :-
		amqp<<encode_table([], Bytes).

	test(amqp_decode_table_01, true(Pairs == [])) :-
		amqp<<decode_table([0, 0, 0, 0], Pairs, _).

	% ==========================================================================
	% Binary Encoding Tests - Field Values
	% ==========================================================================

	test(amqp_encode_field_value_bool_true, true(Bytes == [116, 1])) :-
		amqp<<encode_field_value(bool(true), Bytes).

	test(amqp_encode_field_value_bool_false, true(Bytes == [116, 0])) :-
		amqp<<encode_field_value(bool(false), Bytes).

	test(amqp_encode_field_value_void, true(Bytes == [86])) :-
		amqp<<encode_field_value(void, Bytes).

	test(amqp_decode_field_value_bool_true, true(Value == bool(true))) :-
		amqp<<decode_field_value([116, 1], Value, _).

	test(amqp_decode_field_value_bool_false, true(Value == bool(false))) :-
		amqp<<decode_field_value([116, 0], Value, _).

	test(amqp_decode_field_value_void, true(Value == void)) :-
		amqp<<decode_field_value([86], Value, _).

	% ==========================================================================
	% Message Inspection Tests
	% ==========================================================================

	test(amqp_message_body_01, true(Body == 'Hello')) :-
		Message = message(deliver, 1, 'exchange', 'key', [], 'Hello'),
		amqp::message_body(Message, Body).

	test(amqp_message_delivery_tag_01, true(Tag == 42)) :-
		Message = message(deliver, 42, 'exchange', 'key', [], 'Body'),
		amqp::message_delivery_tag(Message, Tag).

	test(amqp_message_exchange_01, true(Exchange == 'my.exchange')) :-
		Message = message(deliver, 1, 'my.exchange', 'key', [], 'Body'),
		amqp::message_exchange(Message, Exchange).

	test(amqp_message_routing_key_01, true(Key == 'my.routing.key')) :-
		Message = message(deliver, 1, 'exchange', 'my.routing.key', [], 'Body'),
		amqp::message_routing_key(Message, Key).

	test(amqp_message_properties_01, true(Props == [content_type-'text/plain'])) :-
		Message = message(deliver, 1, 'exchange', 'key', [content_type-'text/plain'], 'Body'),
		amqp::message_properties(Message, Props).

	test(amqp_message_property_01, true(Value == 'text/plain')) :-
		Message = message(deliver, 1, 'exchange', 'key', [content_type-'text/plain'], 'Body'),
		amqp::message_property(Message, content_type, Value).

	test(amqp_message_property_02, fail) :-
		Message = message(deliver, 1, 'exchange', 'key', [], 'Body'),
		amqp::message_property(Message, content_type, _).

	% ==========================================================================
	% Connection Alive Test
	% ==========================================================================

	test(amqp_connection_alive_01, fail) :-
		% A fake connection should fail the alive test
		FakeConnection = connection(invalid_stream, invalid_stream, localhost, 5672, 0, 131072, 60),
		amqp::connection_alive(FakeConnection).

	% ==========================================================================
	% Frame Type Constants Tests
	% ==========================================================================

	test(amqp_frame_type_method, true(Type == 1)) :-
		amqp<<frame_type(method, Type).

	test(amqp_frame_type_header, true(Type == 2)) :-
		amqp<<frame_type(header, Type).

	test(amqp_frame_type_body, true(Type == 3)) :-
		amqp<<frame_type(body, Type).

	test(amqp_frame_type_heartbeat, true(Type == 8)) :-
		amqp<<frame_type(heartbeat, Type).

	% ==========================================================================
	% Class ID Constants Tests
	% ==========================================================================

	test(amqp_class_id_connection, true(Id == 10)) :-
		amqp<<class_id(connection, Id).

	test(amqp_class_id_channel, true(Id == 20)) :-
		amqp<<class_id(channel, Id).

	test(amqp_class_id_exchange, true(Id == 40)) :-
		amqp<<class_id(exchange, Id).

	test(amqp_class_id_queue, true(Id == 50)) :-
		amqp<<class_id(queue, Id).

	test(amqp_class_id_basic, true(Id == 60)) :-
		amqp<<class_id(basic, Id).

	test(amqp_class_id_tx, true(Id == 90)) :-
		amqp<<class_id(tx, Id).

	% ==========================================================================
	% Method ID Constants Tests
	% ==========================================================================

	test(amqp_method_id_connection_start, true(Id == 10)) :-
		amqp<<method_id(connection, start, Id).

	test(amqp_method_id_connection_tune, true(Id == 30)) :-
		amqp<<method_id(connection, tune, Id).

	test(amqp_method_id_channel_open, true(Id == 10)) :-
		amqp<<method_id(channel, open, Id).

	test(amqp_method_id_basic_publish, true(Id == 40)) :-
		amqp<<method_id(basic, publish, Id).

	test(amqp_method_id_basic_deliver, true(Id == 60)) :-
		amqp<<method_id(basic, deliver, Id).

	% ==========================================================================
	% Protocol Header Test
	% ==========================================================================

	test(amqp_protocol_header_01, true(Header == [65, 77, 81, 80, 0, 0, 9, 1])) :-
		amqp<<amqp_protocol_header(Header).

	% ==========================================================================
	% Frame End Marker Test
	% ==========================================================================

	test(amqp_frame_end_01, true(End == 206)) :-
		amqp<<frame_end(End).

	% ==========================================================================
	% Roundtrip Encoding Tests
	% ==========================================================================

	test(amqp_roundtrip_short_01, true(Value2 == 12345)) :-
		amqp<<encode_short(12345, Bytes),
		amqp<<decode_short(Bytes, Value2, _).

	test(amqp_roundtrip_long_01, true(Value2 == 123456789)) :-
		amqp<<encode_long(123456789, Bytes),
		amqp<<decode_long(Bytes, Value2, _).

	test(amqp_roundtrip_shortstr_01, true(Atom2 == 'test string')) :-
		amqp<<encode_shortstr('test string', Bytes),
		amqp<<decode_shortstr(Bytes, Atom2, _).

	test(amqp_roundtrip_longstr_01, true(Atom2 == 'longer test string')) :-
		amqp<<encode_longstr('longer test string', Bytes),
		amqp<<decode_longstr(Bytes, Atom2, _).

	% ==========================================================================
	% Float/Double Encoding Tests
	% ==========================================================================

	test(amqp_encode_float_01, true(is_list(Bytes))) :-
		amqp<<encode_float(3.14159, Bytes).

	test(amqp_decode_float_01, true(abs(Value - 3.14159) < 0.001)) :-
		amqp<<encode_float(3.14159, Bytes),
		amqp<<decode_float(Bytes, Value, _).

	test(amqp_roundtrip_float_01, true(abs(Value2 - 1.5) < 0.001)) :-
		amqp<<encode_float(1.5, Bytes),
		amqp<<decode_float(Bytes, Value2, _).

	test(amqp_encode_double_01, true(is_list(Bytes))) :-
		amqp<<encode_double(3.141592653589793, Bytes).

	test(amqp_decode_double_01, true(abs(Value - 3.141592653589793) < 0.0001)) :-
		amqp<<encode_double(3.141592653589793, Bytes),
		amqp<<decode_double(Bytes, Value, _).

	test(amqp_roundtrip_double_01, true(abs(Value2 - 2.718281828) < 0.0001)) :-
		amqp<<encode_double(2.718281828, Bytes),
		amqp<<decode_double(Bytes, Value2, _).

	% ==========================================================================
	% Array Encoding Tests
	% ==========================================================================

	test(amqp_encode_array_empty, true(Bytes == [0, 0, 0, 0])) :-
		amqp<<encode_array([], Bytes).

	test(amqp_encode_array_01, true(is_list(Bytes))) :-
		amqp<<encode_array([longstr(hello), longstr(world)], Bytes).

	test(amqp_decode_array_empty, true(Values == [])) :-
		amqp<<decode_array([0, 0, 0, 0], Values, _).

	test(amqp_roundtrip_array_01, true(Values2 == [longstr(test)])) :-
		amqp<<encode_array([longstr(test)], Bytes),
		amqp<<decode_array(Bytes, Values2, _).

	% ==========================================================================
	% More Field Value Encoding Tests
	% ==========================================================================

	test(amqp_encode_field_value_byte, true(Bytes == [98, 42])) :-
		amqp<<encode_field_value(byte(42), Bytes).

	test(amqp_encode_field_value_short, true(Bytes == [115, 1, 0])) :-
		amqp<<encode_field_value(short(256), Bytes).

	test(amqp_encode_field_value_int, true(Bytes == [73, 0, 1, 0, 0])) :-
		amqp<<encode_field_value(int(65536), Bytes).

	test(amqp_encode_field_value_long, true(is_list(Bytes))) :-
		amqp<<encode_field_value(long(123456789), Bytes).

	test(amqp_encode_field_value_timestamp, true(is_list(Bytes))) :-
		amqp<<encode_field_value(timestamp(1234567890), Bytes).

	test(amqp_encode_field_value_float, true(is_list(Bytes))) :-
		amqp<<encode_field_value(float(1.5), Bytes).

	test(amqp_encode_field_value_double, true(is_list(Bytes))) :-
		amqp<<encode_field_value(double(2.5), Bytes).

	test(amqp_encode_field_value_table, true(is_list(Bytes))) :-
		amqp<<encode_field_value(table([foo-longstr(bar)]), Bytes).

	test(amqp_encode_field_value_array, true(is_list(Bytes))) :-
		amqp<<encode_field_value(array([longstr(x)]), Bytes).

	test(amqp_decode_field_value_byte, true(Value == byte(42))) :-
		amqp<<decode_field_value([98, 42], Value, _).

	test(amqp_decode_field_value_short, true(Value == short(256))) :-
		amqp<<decode_field_value([115, 1, 0], Value, _).

	test(amqp_decode_field_value_int, true(Value == int(65536))) :-
		amqp<<decode_field_value([73, 0, 1, 0, 0], Value, _).

	test(amqp_decode_field_value_long, true(Value == long(123456789))) :-
		amqp<<encode_field_value(long(123456789), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	test(amqp_decode_field_value_timestamp, true(Value == timestamp(1234567890))) :-
		amqp<<encode_field_value(timestamp(1234567890), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	test(amqp_decode_field_value_float, subsumes(float(_), Value)) :-
		amqp<<encode_field_value(float(1.5), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	test(amqp_decode_field_value_double, subsumes(double(_), Value)) :-
		amqp<<encode_field_value(double(2.5), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	test(amqp_decode_field_value_table, subsumes(table(_), Value)) :-
		amqp<<encode_field_value(table([]), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	test(amqp_decode_field_value_array, subsumes(array(_), Value)) :-
		amqp<<encode_field_value(array([]), Bytes),
		amqp<<decode_field_value(Bytes, Value, _).

	% ==========================================================================
	% Frame Encoding/Decoding Tests
	% ==========================================================================

	test(amqp_encode_frame_heartbeat, true(is_list(Bytes))) :-
		amqp<<encode_frame(frame(heartbeat, 0, []), Bytes).

	test(amqp_encode_frame_body, true(is_list(Bytes))) :-
		amqp<<encode_frame(frame(body, 1, [72, 105]), Bytes).

	% ==========================================================================
	% Payload Encoding Tests
	% ==========================================================================

	test(amqp_encode_payload_heartbeat, true(Bytes == [])) :-
		amqp<<encode_payload(heartbeat, [], Bytes).

	test(amqp_encode_payload_body, true(Bytes == [72, 105])) :-
		amqp<<encode_payload(body, [72, 105], Bytes).

	% ==========================================================================
	% Bool to Int Helper Tests
	% ==========================================================================

	test(amqp_bool_to_int_1, true(Int == 1)) :-
		amqp<<bool_to_int(1, Int).

	test(amqp_bool_to_int_0, true(Int == 0)) :-
		amqp<<bool_to_int(0, Int).

	% ==========================================================================
	% Negotiate Value Helper Tests
	% ==========================================================================

	test(amqp_negotiate_value_client_zero, true(Value == 100)) :-
		amqp<<negotiate_value(0, 100, Value).

	test(amqp_negotiate_value_server_zero, true(Value == 50)) :-
		amqp<<negotiate_value(50, 0, Value).

	test(amqp_negotiate_value_both_nonzero, true(Value == 50)) :-
		amqp<<negotiate_value(50, 100, Value).

	test(amqp_negotiate_heartbeat_client_zero, true(Value == 0)) :-
		amqp<<negotiate_heartbeat(0, 60, Value).

	test(amqp_negotiate_heartbeat_server_zero, true(Value == 0)) :-
		amqp<<negotiate_heartbeat(60, 0, Value).

	test(amqp_negotiate_heartbeat_both_nonzero, true(Value == 30)) :-
		amqp<<negotiate_heartbeat(30, 60, Value).

	% ==========================================================================
	% Option Helper Tests
	% ==========================================================================

	test(amqp_option_3_found, true(Value == bar)) :-
		amqp<<option(foo(Value), [foo(bar)], default).

	test(amqp_option_3_default, true(Value == default)) :-
		amqp<<option(foo(Value), [], default).

	% ==========================================================================
	% Body to Bytes Helper Tests
	% ==========================================================================

	test(amqp_body_to_bytes_atom, true(Bytes == [72, 101, 108, 108, 111])) :-
		amqp<<body_to_bytes('Hello', Bytes).

	test(amqp_body_to_bytes_list, true(Bytes == [1, 2, 3])) :-
		amqp<<body_to_bytes([1, 2, 3], Bytes).

	% ==========================================================================
	% Decode Frame Tests
	% ==========================================================================

	test(amqp_decode_frame_heartbeat, true(Type == heartbeat)) :-
		% Heartbeat frame: type=8, channel=0, size=0, frame_end=206
		amqp<<decode_frame([8, 0, 0, 0, 0, 0, 0, 206], Frame),
		Frame = frame(Type, _, _, _, _).

	% ==========================================================================
	% Decode Typed Value Tests
	% ==========================================================================

	test(amqp_decode_typed_value_shortstr, true(Value == hello)) :-
		amqp<<decode_typed_value(shortstr, [5, 104, 101, 108, 108, 111], Value, _).

	test(amqp_decode_typed_value_longstr, true(Value == test)) :-
		amqp<<decode_typed_value(longstr, [0, 0, 0, 4, 116, 101, 115, 116], Value, _).

	test(amqp_decode_typed_value_octet, true(Value == 42)) :-
		amqp<<decode_typed_value(octet, [42], Value, _).

	test(amqp_decode_typed_value_short, true(Value == 256)) :-
		amqp<<decode_typed_value(short, [1, 0], Value, _).

	test(amqp_decode_typed_value_long, true(Value == 65536)) :-
		amqp<<decode_typed_value(long, [0, 1, 0, 0], Value, _).

	test(amqp_decode_typed_value_longlong, true(Value == 1)) :-
		amqp<<decode_typed_value(longlong, [0, 0, 0, 0, 0, 0, 0, 1], Value, _).

	test(amqp_decode_typed_value_table, true(is_list(Value))) :-
		% Empty table: size=0
		amqp<<decode_typed_value(table, [0, 0, 0, 0], Value, _).

	% ==========================================================================
	% Integration Tests (require running AMQP server)
	% These tests are skipped by default since they need an external server.
	% To run them, set up RabbitMQ on localhost:5672 with guest/guest credentials.
	% ==========================================================================

	test(amqp_connect_disconnect_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::connection_alive(Connection),
		amqp::close(Connection).

	test(amqp_connect_with_vhost_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [virtual_host('/')]),
		amqp::close(Connection).

	test(amqp_connect_with_heartbeat_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [heartbeat(30)]),
		amqp::close(Connection).

	% Reconnection tests
	test(amqp_connect_with_reconnect_disabled_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [reconnect(false)]),
		amqp::close(Connection).

	test(amqp_connect_with_reconnect_enabled_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [reconnect(true)]),
		amqp::close(Connection).

	test(amqp_connect_with_reconnect_options_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [reconnect(true), reconnect_attempts(5), reconnect_delay(0.5)]),
		amqp::close(Connection).

	test(amqp_connect_reconnect_failure_01, error(amqp_error(reconnect_failed))) :-
		% Try to connect to a port where nothing is listening
		% Should fail after reconnect_attempts
		amqp::connect(localhost, 59999, _, [reconnect(true), reconnect_attempts(2), reconnect_delay(0.1)]).

	test(amqp_connect_no_reconnect_failure_01, error(amqp_error(connection_failed))) :-
		% Without reconnect, should fail immediately with connection_failed
		amqp::connect(localhost, 59999, _, [reconnect(false)]).

	test(amqp_channel_open_close_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_multiple_channels_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel1),
		amqp::channel_open(Connection, 2, Channel2),
		amqp::channel_open(Connection, 3, Channel3),
		amqp::channel_close(Channel1),
		amqp::channel_close(Channel2),
		amqp::channel_close(Channel3),
		amqp::close(Connection).

	test(amqp_queue_declare_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_queue_declare_generated_name_01, true(atom(Queue)), [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, Queue, [exclusive(true)]),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_exchange_declare_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::exchange_declare(Channel, 'test-exchange', [type(direct), auto_delete(true)]),
		amqp::exchange_delete(Channel, 'test-exchange', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_queue_bind_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::exchange_declare(Channel, 'test-exchange', [type(direct), auto_delete(true)]),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::queue_bind(Channel, 'test-queue', 'test-exchange', [routing_key('test-key')]),
		amqp::queue_unbind(Channel, 'test-queue', 'test-exchange', [routing_key('test-key')]),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::exchange_delete(Channel, 'test-exchange', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_publish_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Hello, World!', [routing_key('test-queue')]),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_publish_with_properties_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', '{"data": "test"}', [
			routing_key('test-queue'),
			content_type('application/json'),
			delivery_mode(2)
		]),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_qos_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::basic_qos(Channel, [prefetch_count(10)]),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_consume_cancel_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::basic_consume(Channel, 'test-queue', [consumer_tag('test-consumer')]),
		amqp::basic_cancel(Channel, 'test-consumer', []),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_transaction_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::tx_select(Channel),
		amqp::basic_publish(Channel, '', 'Message 1', [routing_key('test-queue')]),
		amqp::basic_publish(Channel, '', 'Message 2', [routing_key('test-queue')]),
		amqp::tx_commit(Channel),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_transaction_rollback_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::tx_select(Channel),
		amqp::basic_publish(Channel, '', 'Message to rollback', [routing_key('test-queue')]),
		amqp::tx_rollback(Channel),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_confirm_select_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::confirm_select(Channel),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_send_heartbeat_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, [heartbeat(60)]),
		amqp::send_heartbeat(Connection),
		amqp::close(Connection).

	test(amqp_queue_purge_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-queue', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Message', [routing_key('test-queue')]),
		amqp::queue_purge(Channel, 'test-queue'),
		amqp::queue_delete(Channel, 'test-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_exchange_bind_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::exchange_declare(Channel, 'test-source', [type(fanout), auto_delete(true)]),
		amqp::exchange_declare(Channel, 'test-dest', [type(fanout), auto_delete(true)]),
		amqp::exchange_bind(Channel, 'test-dest', 'test-source', []),
		amqp::exchange_unbind(Channel, 'test-dest', 'test-source', []),
		amqp::exchange_delete(Channel, 'test-dest', []),
		amqp::exchange_delete(Channel, 'test-source', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_get_empty_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-get-empty-queue', [auto_delete(true)]),
		% Queue is empty, basic_get should fail
		(	amqp::basic_get(Channel, 'test-get-empty-queue', [no_ack(true)]) ->
			Got = true
		;	Got = false
		),
		Got == false,
		amqp::queue_delete(Channel, 'test-get-empty-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_get_with_message_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-get-msg-queue', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Test message', [routing_key('test-get-msg-queue')]),
		amqp::basic_get(Channel, 'test-get-msg-queue', [no_ack(true)]),
		% Skip queue delete - auto_delete handles it
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_ack_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-ack-q', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Ack test message', [routing_key('test-ack-q')]),
		amqp::basic_get(Channel, 'test-ack-q', [no_ack(false)]),
		% Ack delivery tag 1
		amqp::basic_ack(Channel, 1, []),
		% Skip queue delete - auto_delete handles it, and stream has pending frames
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_nack_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-nack-q', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Nack test message', [routing_key('test-nack-q')]),
		amqp::basic_get(Channel, 'test-nack-q', [no_ack(false)]),
		% Nack delivery tag 1, don't requeue
		amqp::basic_nack(Channel, 1, [requeue(false)]),
		% Skip queue delete - auto_delete handles it, and stream has pending frames
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_reject_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-reject-q', [auto_delete(true)]),
		amqp::basic_publish(Channel, '', 'Reject test message', [routing_key('test-reject-q')]),
		amqp::basic_get(Channel, 'test-reject-q', [no_ack(false)]),
		% Reject delivery tag 1
		amqp::basic_reject(Channel, 1, [requeue(false)]),
		% Skip queue delete - auto_delete handles it, and stream has pending frames
		amqp::channel_close(Channel),
		amqp::close(Connection).

	test(amqp_basic_recover_01, true, [condition(amqp_server_available)]) :-
		amqp::connect(localhost, 5672, Connection, []),
		amqp::channel_open(Connection, 1, Channel),
		amqp::queue_declare(Channel, 'test-recover-queue', [auto_delete(true)]),
		amqp::basic_recover(Channel, [requeue(true)]),
		amqp::queue_delete(Channel, 'test-recover-queue', []),
		amqp::channel_close(Channel),
		amqp::close(Connection).

	% Auxiliary predicate to check if AMQP server is available
	amqp_server_available :-
		catch(
			(	socket::client_open(localhost, 5672, Input, Output, []),
				socket::close(Input, Output)
			),
			_,
			fail
		).

:- end_object.
