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


:- object(probe_mqtt_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object used by the MQTT tests to verify scheme-derived defaults.'
	]).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	open_connection(Host, Port, probe_connection(Host, Port, Options), Options).

	close_connection(_Connection).

	connection_streams(_Connection, probe_input, probe_output).

:- end_object.


:- object(probe_mqtt_tcp_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object with plain HTTP scheme support only.'
	]).

	supported_request_scheme(http).

	open_connection(Host, Port, probe_connection(Host, Port, Options), Options).

	close_connection(_Connection).

	connection_streams(_Connection, probe_input, probe_output).

:- end_object.


:- object(probe_mqtt_file_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object that exposes file-backed binary streams for MQTT session tests.'
	]).

	:- uses(list, [
		member/2
	]).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	open_connection(Host, Port, probe_file_connection(Host, Port, Options, Input, Output), Options) :-
		connection_file(response_file, Options, ResponseFile),
		connection_file(request_file, Options, RequestFile),
		open(ResponseFile, read, Input, [type(binary)]),
		catch(
			open(RequestFile, write, Output, [type(binary)]),
			Error,
			( 	close(Input),
				throw(Error)
			)
		).

	close_connection(probe_file_connection(_Host, _Port, _Options, Input, Output)) :-
		catch(close(Input), _, true),
		catch(close(Output), _, true).

	connection_streams(probe_file_connection(_Host, _Port, _Options, Input, Output), Input, Output).

	connection_file(Name, Options, File) :-
		functor(Template, Name, 1),
		member(Template, Options),
		!,
		arg(1, Template, File).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Unit tests for the "mqtt" library.'
	]).

	cover(mqtt).

	% ==========================================================================
	% Binary Encoding Tests - Unsigned Integers
	% ==========================================================================

	test(mqtt_encode_uint8_2_01, deterministic(Bytes == [0])) :-
		mqtt<<encode_uint8(0, Bytes).

	test(mqtt_encode_uint8_2_02, deterministic(Bytes == [255])) :-
		mqtt<<encode_uint8(255, Bytes).

	test(mqtt_decode_uint8_3_01, deterministic((Value == 42, Rest == [1, 2]))) :-
		mqtt<<decode_uint8([42, 1, 2], Value, Rest).

	test(mqtt_encode_uint16_2_01, deterministic(Bytes == [0, 0])) :-
		mqtt<<encode_uint16(0, Bytes).

	test(mqtt_encode_uint16_2_02, deterministic(Bytes == [255, 255])) :-
		mqtt<<encode_uint16(65535, Bytes).

	test(mqtt_decode_uint16_3_01, deterministic((Value == 256, Rest == [99]))) :-
		mqtt<<decode_uint16([1, 0, 99], Value, Rest).

	test(mqtt_encode_uint32_2_01, deterministic(Bytes == [0, 0, 0, 0])) :-
		mqtt<<encode_uint32(0, Bytes).

	test(mqtt_encode_uint32_2_02, deterministic(Bytes == [255, 255, 255, 255])) :-
		mqtt<<encode_uint32(4294967295, Bytes).

	test(mqtt_decode_uint32_3_01, deterministic((Value == 16777216, Rest == [99]))) :-
		mqtt<<decode_uint32([1, 0, 0, 0, 99], Value, Rest).

	% ==========================================================================
	% Binary Encoding Tests - MQTT Variable Byte Integer
	% ==========================================================================

	test(mqtt_encode_varint_2_01, deterministic(Bytes == [0])) :-
		mqtt<<encode_varint(0, Bytes).

	test(mqtt_encode_varint_2_02, deterministic(Bytes == [127])) :-
		mqtt<<encode_varint(127, Bytes).

	test(mqtt_encode_varint_2_03, deterministic(Bytes == [128, 1])) :-
		mqtt<<encode_varint(128, Bytes).

	test(mqtt_encode_varint_2_04, deterministic(Bytes == [255, 127])) :-
		mqtt<<encode_varint(16383, Bytes).

	test(mqtt_encode_varint_2_05, deterministic(Bytes == [128, 128, 1])) :-
		mqtt<<encode_varint(16384, Bytes).

	test(mqtt_encode_varint_2_06, deterministic(Bytes == [255, 255, 127])) :-
		mqtt<<encode_varint(2097151, Bytes).

	test(mqtt_encode_varint_2_07, deterministic(Bytes == [128, 128, 128, 1])) :-
		mqtt<<encode_varint(2097152, Bytes).

	test(mqtt_encode_varint_2_08, deterministic(Bytes == [255, 255, 255, 127])) :-
		mqtt<<encode_varint(268435455, Bytes).

	test(mqtt_encode_varint_2_09, error(domain_error(between(0, 268435455), 268435456))) :-
		mqtt<<encode_varint(268435456, _Bytes).

	test(mqtt_decode_varint_3_01, deterministic((Value == 0, Rest == [99]))) :-
		mqtt<<decode_varint([0, 99], Value, Rest).

	test(mqtt_decode_varint_3_02, deterministic((Value == 128, Rest == [99]))) :-
		mqtt<<decode_varint([128, 1, 99], Value, Rest).

	test(mqtt_decode_varint_3_03, deterministic((Value == 268435455, Rest == [99]))) :-
		mqtt<<decode_varint([255, 255, 255, 127, 99], Value, Rest).

	test(mqtt_decode_varint_3_04, error(domain_error(mqtt_variable_byte_integer, [128, 0]))) :-
		mqtt<<decode_varint([128, 0], _Value, _Rest).

	test(mqtt_decode_varint_3_05, error(domain_error(mqtt_variable_byte_integer, [128, 128, 128, 128]))) :-
		mqtt<<decode_varint([128, 128, 128, 128, 0], _Value, _Rest).

	% ==========================================================================
	% Binary Encoding Tests - UTF-8 Strings and Binary Data
	% ==========================================================================

	test(mqtt_encode_utf8_string_2_01, deterministic(Bytes == [0, 0])) :-
		mqtt<<encode_utf8_string('', Bytes).

	test(mqtt_encode_utf8_string_2_02, deterministic(Bytes == [0, 5, 104, 101, 108, 108, 111])) :-
		mqtt<<encode_utf8_string(hello, Bytes).

	test(mqtt_decode_utf8_string_3_01, deterministic((String == hello, Rest == [99]))) :-
		mqtt<<decode_utf8_string([0, 5, 104, 101, 108, 108, 111, 99], String, Rest).

	test(mqtt_decode_utf8_string_3_02, error(domain_error(mqtt_utf8_string, [0]))) :-
		mqtt<<decode_utf8_string([0, 1, 0], _String, _Rest).

	test(mqtt_encode_binary_data_2_01, deterministic(Bytes == [0, 0])) :-
		mqtt<<encode_binary_data([], Bytes).

	test(mqtt_encode_binary_data_2_02, deterministic(Bytes == [0, 3, 0, 127, 255])) :-
		mqtt<<encode_binary_data([0, 127, 255], Bytes).

	test(mqtt_decode_binary_data_3_01, deterministic((Data == [0, 127, 255], Rest == [99]))) :-
		mqtt<<decode_binary_data([0, 3, 0, 127, 255, 99], Data, Rest).

	test(mqtt_encode_utf8_string_pair_2_01, deterministic(Bytes == [0, 4, 110, 97, 109, 101, 0, 5, 118, 97, 108, 117, 101])) :-
		mqtt<<encode_utf8_string_pair(name-value, Bytes).

	test(mqtt_decode_utf8_string_pair_3_01, deterministic((Pair == name-value, Rest == [99]))) :-
		mqtt<<decode_utf8_string_pair([0, 4, 110, 97, 109, 101, 0, 5, 118, 97, 108, 117, 101, 99], Pair, Rest).

	% ==========================================================================
	% Binary Encoding Tests - MQTT Properties
	% ==========================================================================

	test(mqtt_encode_properties_2_01, deterministic(Bytes == [0])) :-
		mqtt<<encode_properties([], Bytes).

	test(mqtt_decode_properties_3_01, deterministic((Properties == [], Rest == [99]))) :-
		mqtt<<decode_properties([0, 99], Properties, Rest).

	test(mqtt_encode_properties_2_02, deterministic(Bytes == [5, 31, 0, 2, 111, 107])) :-
		mqtt<<encode_properties([reason_string(ok)], Bytes).

	test(mqtt_decode_properties_3_02, deterministic((Properties == [reason_string(ok)], Rest == [99]))) :-
		mqtt<<decode_properties([5, 31, 0, 2, 111, 107, 99], Properties, Rest).

	test(mqtt_encode_properties_2_03, deterministic(Bytes == [32, 1, 1, 19, 0, 30, 2, 0, 0, 0, 60, 11, 128, 1, 3, 0, 4, 116, 101, 120, 116, 9, 0, 2, 1, 2, 38, 0, 1, 107, 0, 1, 118])) :-
		mqtt<<encode_properties([payload_format_indicator(1), server_keep_alive(30), message_expiry_interval(60), subscription_identifier(128), content_type(text), correlation_data([1, 2]), user_property(k-v)], Bytes).

	test(mqtt_decode_properties_3_03, deterministic((Properties == [payload_format_indicator(1), server_keep_alive(30), message_expiry_interval(60), subscription_identifier(128), content_type(text), correlation_data([1, 2]), user_property(k-v)], Rest == [99]))) :-
		mqtt<<decode_properties([32, 1, 1, 19, 0, 30, 2, 0, 0, 0, 60, 11, 128, 1, 3, 0, 4, 116, 101, 120, 116, 9, 0, 2, 1, 2, 38, 0, 1, 107, 0, 1, 118, 99], Properties, Rest).

	test(mqtt_encode_properties_2_04, deterministic(Bytes == [29, 38, 0, 4, 110, 97, 109, 101, 0, 5, 118, 97, 108, 117, 101, 38, 0, 5, 111, 116, 104, 101, 114, 0, 5, 101, 110, 116, 114, 121])) :-
		mqtt<<encode_properties([user_property(name-value), user_property(other-entry)], Bytes).

	test(mqtt_decode_properties_3_04, error(domain_error(mqtt_property_identifier, 7))) :-
		mqtt<<decode_properties([1, 7], _Properties, _Rest).

	test(mqtt_encode_properties_2_05, error(domain_error(mqtt_property, foo(bar)))) :-
		mqtt<<encode_properties([foo(bar)], _Bytes).

	test(mqtt_encode_properties_2_06, error(domain_error(boolean, maybe))) :-
		mqtt<<encode_properties([retain_available(maybe)], _Bytes).

	test(mqtt_decode_properties_3_05, error(domain_error(boolean_bit, 2))) :-
		mqtt<<decode_properties([2, 37, 2], _Properties, _Rest).

	test(mqtt_encode_properties_2_07, error(domain_error(mqtt_property_value, payload_format_indicator-2))) :-
		mqtt<<encode_properties([payload_format_indicator(2)], _Bytes).

	test(mqtt_encode_properties_2_08, error(domain_error(mqtt_property_value, maximum_qos-2))) :-
		mqtt<<encode_properties([maximum_qos(2)], _Bytes).

	test(mqtt_encode_properties_2_09, error(domain_error(mqtt_property_value, receive_maximum-0))) :-
		mqtt<<encode_properties([receive_maximum(0)], _Bytes).

	test(mqtt_encode_properties_2_10, error(domain_error(mqtt_property_value, maximum_packet_size-0))) :-
		mqtt<<encode_properties([maximum_packet_size(0)], _Bytes).

	test(mqtt_encode_properties_2_11, error(domain_error(mqtt_property_value, subscription_identifier-0))) :-
		mqtt<<encode_properties([subscription_identifier(0)], _Bytes).

	test(mqtt_encode_properties_2_12, error(domain_error(mqtt_topic_name, 'sensor/+'))) :-
		mqtt<<encode_properties([response_topic('sensor/+')], _Bytes).

	% ==========================================================================
	% Fixed Header Tests
	% ==========================================================================

	test(mqtt_packet_type_2_01, deterministic(Code == 1)) :-
		mqtt::packet_type(connect, Code).

	test(mqtt_packet_type_2_02, deterministic(Type == auth)) :-
		mqtt::packet_type(Type, 15).

	test(mqtt_encode_fixed_header_4_01, deterministic(Bytes == [16, 0])) :-
		mqtt<<encode_fixed_header(connect, 0, 0, Bytes).

	test(mqtt_encode_fixed_header_4_02, deterministic(Bytes == [192, 0])) :-
		mqtt<<encode_fixed_header(pingreq, 0, 0, Bytes).

	test(mqtt_encode_fixed_header_4_03, deterministic(Bytes == [130, 0])) :-
		mqtt<<encode_fixed_header(subscribe, 2, 0, Bytes).

	test(mqtt_encode_fixed_header_4_04, deterministic(Bytes == [51, 128, 1])) :-
		mqtt<<encode_fixed_header(publish, 3, 128, Bytes).

	test(mqtt_decode_fixed_header_5_01, deterministic((Type == connect, Flags == 0, RemainingLength == 0, Rest == [99]))) :-
		mqtt<<decode_fixed_header([16, 0, 99], Type, Flags, RemainingLength, Rest).

	test(mqtt_decode_fixed_header_5_02, deterministic((Type == publish, Flags == 3, RemainingLength == 128, Rest == [99]))) :-
		mqtt<<decode_fixed_header([51, 128, 1, 99], Type, Flags, RemainingLength, Rest).

	test(mqtt_encode_fixed_header_4_05, error(domain_error(mqtt_fixed_header_flags, subscribe-0))) :-
		mqtt<<encode_fixed_header(subscribe, 0, 0, _Bytes).

	test(mqtt_decode_fixed_header_5_03, error(domain_error(mqtt_fixed_header_flags, subscribe-0))) :-
		mqtt<<decode_fixed_header([128, 0], _Type, _Flags, _RemainingLength, _Rest).

	test(mqtt_decode_fixed_header_5_04, error(domain_error(mqtt_packet_type, 0))) :-
		mqtt<<decode_fixed_header([0, 0], _Type, _Flags, _RemainingLength, _Rest).

	test(mqtt_decode_fixed_header_5_05, error(domain_error(mqtt_fixed_header_flags, publish-6))) :-
		mqtt<<decode_fixed_header([54, 0], _Type, _Flags, _RemainingLength, _Rest).

	% ==========================================================================
	% Packet Encode/Decode Tests - Zero-Length Packets
	% ==========================================================================

	test(mqtt_encode_packet_2_01, deterministic(Bytes == [192, 0])) :-
		mqtt::encode_packet(packet(pingreq, []), Bytes).

	test(mqtt_encode_packet_2_02, deterministic(Bytes == [208, 0])) :-
		mqtt::encode_packet(packet(pingresp, []), Bytes).

	test(mqtt_decode_packet_2_01, deterministic(Packet == packet(pingreq, []))) :-
		mqtt::decode_packet([192, 0], Packet).

	test(mqtt_decode_packet_2_02, deterministic(Packet == packet(pingresp, []))) :-
		mqtt::decode_packet([208, 0], Packet).

	test(mqtt_decode_packet_2_03, error(domain_error(mqtt_packet_remaining_length, pingreq-1))) :-
		mqtt::decode_packet([192, 1, 0], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - CONNECT
	% ==========================================================================

	test(mqtt_encode_packet_2_04, deterministic(Bytes == [16, 19, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 0, 0, 6, 99, 108, 105, 101, 110, 116])) :-
		mqtt::encode_packet(packet(connect, [client_id(client), clean_start(true), keep_alive(60), properties([])]), Bytes).

	test(mqtt_encode_packet_2_05, deterministic(Bytes == [16, 13, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 0, 0, 0])) :-
		mqtt::encode_packet(packet(connect, []), Bytes).

	test(mqtt_decode_packet_2_04, deterministic(Packet == packet(connect, [client_id(client), clean_start(true), keep_alive(60), properties([])]))) :-
		mqtt::decode_packet([16, 19, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 0, 0, 6, 99, 108, 105, 101, 110, 116], Packet).

	test(mqtt_decode_packet_2_05, deterministic(Packet == packet(connect, [client_id(''), clean_start(false), keep_alive(0), properties([])]))) :-
		mqtt::decode_packet([16, 13, 0, 4, 77, 81, 84, 84, 5, 0, 0, 0, 0, 0, 0], Packet).

	test(mqtt_encode_packet_2_06a, deterministic(Bytes == [16, 34, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 15, 17, 0, 0, 0, 10, 33, 0, 32, 38, 0, 1, 107, 0, 1, 118, 0, 6, 99, 108, 105, 101, 110, 116])) :-
		mqtt::encode_packet(packet(connect, [client_id(client), clean_start(true), keep_alive(60), properties([session_expiry_interval(10), receive_maximum(32), user_property(k-v)])]), Bytes).

	test(mqtt_decode_packet_2_06a, deterministic(Packet == packet(connect, [client_id(client), clean_start(true), keep_alive(60), properties([session_expiry_interval(10), receive_maximum(32), user_property(k-v)])]))) :-
		mqtt::decode_packet([16, 34, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 15, 17, 0, 0, 0, 10, 33, 0, 32, 38, 0, 1, 107, 0, 1, 118, 0, 6, 99, 108, 105, 101, 110, 116], Packet).

	test(mqtt_encode_packet_2_06, error(domain_error(boolean, maybe))) :-
		mqtt::encode_packet(packet(connect, [clean_start(maybe)]), _Bytes).

	test(mqtt_decode_packet_2_06, error(domain_error(mqtt_connect_flags, 1))) :-
		mqtt::decode_packet([16, 13, 0, 4, 77, 81, 84, 84, 5, 1, 0, 60, 0, 0, 0], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - CONNACK
	% ==========================================================================

	test(mqtt_reason_code_3_01, true(Code == 0)) :-
		mqtt::reason_code(connack, success, Code).

	test(mqtt_reason_code_3_02, true(ReasonCode == server_unavailable)) :-
		mqtt::reason_code(connack, ReasonCode, 136).

	test(mqtt_encode_packet_2_07, deterministic(Bytes == [32, 3, 0, 0, 0])) :-
		mqtt::encode_packet(packet(connack, [session_present(false), reason_code(success), properties([])]), Bytes).

	test(mqtt_encode_packet_2_08, deterministic(Bytes == [32, 3, 1, 136, 0])) :-
		mqtt::encode_packet(packet(connack, [session_present(true), reason_code(server_unavailable), properties([])]), Bytes).

	test(mqtt_decode_packet_2_07, deterministic(Packet == packet(connack, [session_present(false), reason_code(success), properties([])]))) :-
		mqtt::decode_packet([32, 3, 0, 0, 0], Packet).

	test(mqtt_decode_packet_2_08, deterministic(Packet == packet(connack, [session_present(true), reason_code(server_unavailable), properties([])]))) :-
		mqtt::decode_packet([32, 3, 1, 136, 0], Packet).

	test(mqtt_encode_packet_2_08a, deterministic(Bytes == [32, 25, 0, 0, 22, 18, 0, 6, 115, 101, 114, 118, 101, 114, 39, 0, 0, 4, 0, 37, 1, 40, 0, 41, 1, 42, 0])) :-
		mqtt::encode_packet(packet(connack, [session_present(false), reason_code(success), properties([assigned_client_identifier(server), maximum_packet_size(1024), retain_available(true), wildcard_subscription_available(false), subscription_identifier_available(true), shared_subscription_available(false)])]), Bytes).

	test(mqtt_decode_packet_2_08a, deterministic(Packet == packet(connack, [session_present(false), reason_code(success), properties([assigned_client_identifier(server), maximum_packet_size(1024), retain_available(true), wildcard_subscription_available(false), subscription_identifier_available(true), shared_subscription_available(false)])]))) :-
		mqtt::decode_packet([32, 25, 0, 0, 22, 18, 0, 6, 115, 101, 114, 118, 101, 114, 39, 0, 0, 4, 0, 37, 1, 40, 0, 41, 1, 42, 0], Packet).

	test(mqtt_encode_packet_2_09, error(domain_error(boolean, maybe))) :-
		mqtt::encode_packet(packet(connack, [session_present(maybe)]), _Bytes).

	test(mqtt_encode_packet_2_10, error(domain_error(mqtt_reason_code, connack-unknown))) :-
		mqtt::encode_packet(packet(connack, [reason_code(unknown)]), _Bytes).

	test(mqtt_decode_packet_2_09, error(domain_error(mqtt_connack_flags, 2))) :-
		mqtt::decode_packet([32, 3, 2, 0, 0], _Packet).

	test(mqtt_decode_packet_2_10, error(domain_error(mqtt_reason_code, connack-255))) :-
		mqtt::decode_packet([32, 3, 0, 255, 0], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - DISCONNECT
	% ==========================================================================

	test(mqtt_reason_code_3_03, true(Code == 0)) :-
		mqtt::reason_code(disconnect, normal_disconnection, Code).

	test(mqtt_reason_code_3_04, true(ReasonCode == server_busy)) :-
		mqtt::reason_code(disconnect, ReasonCode, 137).

	test(mqtt_encode_packet_2_11, deterministic(Bytes == [224, 0])) :-
		mqtt::encode_packet(packet(disconnect, []), Bytes).

	test(mqtt_encode_packet_2_12, deterministic(Bytes == [224, 2, 137, 0])) :-
		mqtt::encode_packet(packet(disconnect, [reason_code(server_busy), properties([])]), Bytes).

	test(mqtt_decode_packet_2_11, deterministic(Packet == packet(disconnect, [reason_code(normal_disconnection), properties([])]))) :-
		mqtt::decode_packet([224, 0], Packet).

	test(mqtt_decode_packet_2_12, deterministic(Packet == packet(disconnect, [reason_code(disconnect_with_will_message), properties([])]))) :-
		mqtt::decode_packet([224, 1, 4], Packet).

	test(mqtt_decode_packet_2_13, deterministic(Packet == packet(disconnect, [reason_code(server_busy), properties([])]))) :-
		mqtt::decode_packet([224, 2, 137, 0], Packet).

	test(mqtt_encode_packet_2_13, error(domain_error(mqtt_reason_code, disconnect-unknown))) :-
		mqtt::encode_packet(packet(disconnect, [reason_code(unknown)]), _Bytes).

	test(mqtt_decode_packet_2_14, error(domain_error(mqtt_reason_code, disconnect-255))) :-
		mqtt::decode_packet([224, 1, 255], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - AUTH
	% ==========================================================================

	test(mqtt_reason_code_3_05, true(Code == 24)) :-
		mqtt::reason_code(auth, continue_authentication, Code).

	test(mqtt_reason_code_3_06, true(ReasonCode == re_authenticate)) :-
		mqtt::reason_code(auth, ReasonCode, 25).

	test(mqtt_encode_packet_2_14, deterministic(Bytes == [240, 0])) :-
		mqtt::encode_packet(packet(auth, []), Bytes).

	test(mqtt_encode_packet_2_15, deterministic(Bytes == [240, 2, 24, 0])) :-
		mqtt::encode_packet(packet(auth, [reason_code(continue_authentication), properties([])]), Bytes).

	test(mqtt_decode_packet_2_15, deterministic(Packet == packet(auth, [reason_code(success), properties([])]))) :-
		mqtt::decode_packet([240, 0], Packet).

	test(mqtt_decode_packet_2_16, deterministic(Packet == packet(auth, [reason_code(re_authenticate), properties([])]))) :-
		mqtt::decode_packet([240, 1, 25], Packet).

	test(mqtt_decode_packet_2_17, deterministic(Packet == packet(auth, [reason_code(continue_authentication), properties([])]))) :-
		mqtt::decode_packet([240, 2, 24, 0], Packet).

	test(mqtt_encode_packet_2_16, error(domain_error(mqtt_reason_code, auth-unknown))) :-
		mqtt::encode_packet(packet(auth, [reason_code(unknown)]), _Bytes).

	test(mqtt_decode_packet_2_18, error(domain_error(mqtt_reason_code, auth-255))) :-
		mqtt::decode_packet([240, 1, 255], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - Publish Acknowledgements
	% ==========================================================================

	test(mqtt_reason_code_3_07, true(Code == 16)) :-
		mqtt::reason_code(puback, no_matching_subscribers, Code).

	test(mqtt_reason_code_3_08, true(ReasonCode == packet_identifier_not_found)) :-
		mqtt::reason_code(pubrel, ReasonCode, 146).

	test(mqtt_encode_packet_2_17, deterministic(Bytes == [64, 2, 0, 7])) :-
		mqtt::encode_packet(packet(puback, [packet_identifier(7)]), Bytes).

	test(mqtt_encode_packet_2_18, deterministic(Bytes == [64, 4, 0, 7, 16, 0])) :-
		mqtt::encode_packet(packet(puback, [packet_identifier(7), reason_code(no_matching_subscribers), properties([])]), Bytes).

	test(mqtt_encode_packet_2_19, deterministic(Bytes == [80, 4, 0, 8, 128, 0])) :-
		mqtt::encode_packet(packet(pubrec, [packet_identifier(8), reason_code(unspecified_error), properties([])]), Bytes).

	test(mqtt_encode_packet_2_20, deterministic(Bytes == [98, 2, 0, 9])) :-
		mqtt::encode_packet(packet(pubrel, [packet_identifier(9)]), Bytes).

	test(mqtt_encode_packet_2_21, deterministic(Bytes == [112, 2, 0, 10])) :-
		mqtt::encode_packet(packet(pubcomp, [packet_identifier(10)]), Bytes).

	test(mqtt_decode_packet_2_19, deterministic(Packet == packet(puback, [packet_identifier(7), reason_code(success), properties([])]))) :-
		mqtt::decode_packet([64, 2, 0, 7], Packet).

	test(mqtt_decode_packet_2_20, deterministic(Packet == packet(puback, [packet_identifier(7), reason_code(no_matching_subscribers), properties([])]))) :-
		mqtt::decode_packet([64, 4, 0, 7, 16, 0], Packet).

	test(mqtt_decode_packet_2_21, deterministic(Packet == packet(pubrel, [packet_identifier(9), reason_code(packet_identifier_not_found), properties([])]))) :-
		mqtt::decode_packet([98, 4, 0, 9, 146, 0], Packet).

	test(mqtt_encode_packet_2_21a, deterministic(Bytes == [64, 16, 0, 7, 16, 12, 31, 0, 2, 111, 107, 38, 0, 1, 107, 0, 1, 118])) :-
		mqtt::encode_packet(packet(puback, [packet_identifier(7), reason_code(no_matching_subscribers), properties([reason_string(ok), user_property(k-v)])]), Bytes).

	test(mqtt_decode_packet_2_21a, deterministic(Packet == packet(puback, [packet_identifier(7), reason_code(no_matching_subscribers), properties([reason_string(ok), user_property(k-v)])]))) :-
		mqtt::decode_packet([64, 16, 0, 7, 16, 12, 31, 0, 2, 111, 107, 38, 0, 1, 107, 0, 1, 118], Packet).

	test(mqtt_encode_packet_2_22, error(domain_error(mqtt_packet_identifier, 0))) :-
		mqtt::encode_packet(packet(puback, [packet_identifier(0)]), _Bytes).

	test(mqtt_decode_packet_2_22, error(domain_error(mqtt_packet_identifier, 0))) :-
		mqtt::decode_packet([64, 2, 0, 0], _Packet).

	test(mqtt_encode_packet_2_23, error(domain_error(mqtt_reason_code, puback-packet_identifier_not_found))) :-
		mqtt::encode_packet(packet(puback, [packet_identifier(7), reason_code(packet_identifier_not_found)]), _Bytes).

	test(mqtt_decode_packet_2_23, error(domain_error(mqtt_reason_code, pubrec-255))) :-
		mqtt::decode_packet([80, 3, 0, 8, 255], _Packet).

	test(mqtt_decode_packet_2_24, error(domain_error(mqtt_fixed_header_flags, pubrel-0))) :-
		mqtt::decode_packet([96, 2, 0, 9], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - Subscription Acknowledgements
	% ==========================================================================

	test(mqtt_reason_code_3_09, true(Code == 162)) :-
		mqtt::reason_code(suback, wildcard_subscriptions_not_supported, Code).

	test(mqtt_reason_code_3_10, true(ReasonCode == no_subscription_existed)) :-
		mqtt::reason_code(unsuback, ReasonCode, 17).

	test(mqtt_encode_packet_2_24, deterministic(Bytes == [144, 6, 0, 7, 0, 0, 1, 128])) :-
		mqtt::encode_packet(packet(suback, [packet_identifier(7), properties([]), reason_codes([granted_qos_0, granted_qos_1, unspecified_error])]), Bytes).

	test(mqtt_encode_packet_2_25, deterministic(Bytes == [176, 5, 0, 8, 0, 0, 17])) :-
		mqtt::encode_packet(packet(unsuback, [packet_identifier(8), properties([]), reason_codes([success, no_subscription_existed])]), Bytes).

	test(mqtt_decode_packet_2_25, deterministic(Packet == packet(suback, [packet_identifier(7), properties([]), reason_codes([granted_qos_0, granted_qos_1, unspecified_error])]))) :-
		mqtt::decode_packet([144, 6, 0, 7, 0, 0, 1, 128], Packet).

	test(mqtt_decode_packet_2_26, deterministic(Packet == packet(unsuback, [packet_identifier(8), properties([]), reason_codes([success, no_subscription_existed])]))) :-
		mqtt::decode_packet([176, 5, 0, 8, 0, 0, 17], Packet).

	test(mqtt_encode_packet_2_26, error(domain_error(mqtt_reason_code_list, []))) :-
		mqtt::encode_packet(packet(suback, [packet_identifier(7), reason_codes([])]), _Bytes).

	test(mqtt_encode_packet_2_27, error(domain_error(mqtt_reason_code, suback-success))) :-
		mqtt::encode_packet(packet(suback, [packet_identifier(7), reason_codes([success])]), _Bytes).

	test(mqtt_decode_packet_2_27, error(domain_error(mqtt_reason_code_list, []))) :-
		mqtt::decode_packet([144, 3, 0, 7, 0], _Packet).

	test(mqtt_decode_packet_2_28, error(domain_error(mqtt_fixed_header_flags, unsuback-2))) :-
		mqtt::decode_packet([178, 5, 0, 8, 0, 0, 17], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - PUBLISH
	% ==========================================================================

	test(mqtt_encode_packet_2_28, deterministic(Bytes == [48, 16, 0, 11, 115, 101, 110, 115, 111, 114, 47, 116, 101, 109, 112, 0, 1, 2])) :-
		mqtt::encode_packet(packet(publish, [topic_name('sensor/temp'), payload([1, 2])]), Bytes).

	test(mqtt_encode_packet_2_29, deterministic(Bytes == [59, 7, 0, 1, 97, 0, 7, 0, 255])) :-
		mqtt::encode_packet(packet(publish, [dup(true), qos(1), retain(true), topic_name(a), packet_identifier(7), properties([]), payload([255])]), Bytes).

	test(mqtt_decode_packet_2_29, deterministic(Packet == packet(publish, [dup(false), qos(0), retain(false), topic_name('sensor/temp'), properties([]), payload([1, 2])]))) :-
		mqtt::decode_packet([48, 16, 0, 11, 115, 101, 110, 115, 111, 114, 47, 116, 101, 109, 112, 0, 1, 2], Packet).

	test(mqtt_decode_packet_2_30, deterministic(Packet == packet(publish, [dup(true), qos(1), retain(true), topic_name(a), packet_identifier(7), properties([]), payload([255])]))) :-
		mqtt::decode_packet([59, 7, 0, 1, 97, 0, 7, 0, 255], Packet).

	test(mqtt_encode_packet_2_30a, deterministic(Bytes == [48, 39, 0, 1, 97, 34, 1, 1, 3, 0, 4, 116, 101, 120, 116, 8, 0, 5, 114, 101, 112, 108, 121, 9, 0, 2, 1, 2, 2, 0, 0, 0, 60, 38, 0, 1, 107, 0, 1, 118, 255])) :-
		mqtt::encode_packet(packet(publish, [topic_name(a), properties([payload_format_indicator(1), content_type(text), response_topic(reply), correlation_data([1, 2]), message_expiry_interval(60), user_property(k-v)]), payload([255])]), Bytes).

	test(mqtt_decode_packet_2_30a, deterministic(Packet == packet(publish, [dup(false), qos(0), retain(false), topic_name(a), properties([payload_format_indicator(1), content_type(text), response_topic(reply), correlation_data([1, 2]), message_expiry_interval(60), user_property(k-v)]), payload([255])]))) :-
		mqtt::decode_packet([48, 39, 0, 1, 97, 34, 1, 1, 3, 0, 4, 116, 101, 120, 116, 8, 0, 5, 114, 101, 112, 108, 121, 9, 0, 2, 1, 2, 2, 0, 0, 0, 60, 38, 0, 1, 107, 0, 1, 118, 255], Packet).

	test(mqtt_encode_packet_2_30, error(domain_error(mqtt_qos, 3))) :-
		mqtt::encode_packet(packet(publish, [qos(3), topic_name(a)]), _Bytes).

	test(mqtt_encode_packet_2_31, error(domain_error(mqtt_topic_name, ''))) :-
		mqtt::encode_packet(packet(publish, [topic_name('')]), _Bytes).

	test(mqtt_encode_packet_2_32, error(domain_error(mqtt_topic_name, 'sensor/+'))) :-
		mqtt::encode_packet(packet(publish, [topic_name('sensor/+')]), _Bytes).

	test(mqtt_decode_packet_2_31, error(domain_error(mqtt_topic_name, ''))) :-
		mqtt::decode_packet([48, 3, 0, 0, 0], _Packet).

	% ==========================================================================
	% Packet Encode/Decode Tests - SUBSCRIBE and UNSUBSCRIBE
	% ==========================================================================

	test(mqtt_encode_packet_2_33, deterministic(Bytes == [130, 14, 0, 7, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43, 21])) :-
		mqtt::encode_packet(packet(subscribe, [packet_identifier(7), properties([]), subscriptions([subscription('sensor/+', [maximum_qos(1), no_local(true), retain_as_published(false), retain_handling(send_at_new_subscription)])])]), Bytes).

	test(mqtt_encode_packet_2_34, deterministic(Bytes == [162, 16, 0, 8, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43, 0, 1, 35])) :-
		mqtt::encode_packet(packet(unsubscribe, [packet_identifier(8), properties([]), topic_filters(['sensor/+', '#'])]), Bytes).

	test(mqtt_decode_packet_2_32, deterministic(Packet == packet(subscribe, [packet_identifier(7), properties([]), subscriptions([subscription('sensor/+', [maximum_qos(1), no_local(true), retain_as_published(false), retain_handling(send_at_new_subscription)])])]))) :-
		mqtt::decode_packet([130, 14, 0, 7, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43, 21], Packet).

	test(mqtt_decode_packet_2_33, deterministic(Packet == packet(unsubscribe, [packet_identifier(8), properties([]), topic_filters(['sensor/+', '#'])]))) :-
		mqtt::decode_packet([162, 16, 0, 8, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43, 0, 1, 35], Packet).

	test(mqtt_encode_packet_2_34a, deterministic(Bytes == [130, 10, 0, 7, 3, 11, 128, 1, 0, 1, 97, 0])) :-
		mqtt::encode_packet(packet(subscribe, [packet_identifier(7), properties([subscription_identifier(128)]), subscriptions([subscription(a, [])])]), Bytes).

	test(mqtt_decode_packet_2_34a, deterministic(Packet == packet(subscribe, [packet_identifier(7), properties([subscription_identifier(128)]), subscriptions([subscription(a, [maximum_qos(0), no_local(false), retain_as_published(false), retain_handling(send_at_subscribe)])])]))) :-
		mqtt::decode_packet([130, 10, 0, 7, 3, 11, 128, 1, 0, 1, 97, 0], Packet).

	test(mqtt_encode_packet_2_35, error(domain_error(mqtt_subscriptions, []))) :-
		mqtt::encode_packet(packet(subscribe, [packet_identifier(7), subscriptions([])]), _Bytes).

	test(mqtt_encode_packet_2_36, error(domain_error(mqtt_topic_filter, 'sensor/#/x'))) :-
		mqtt::encode_packet(packet(subscribe, [packet_identifier(7), subscriptions([subscription('sensor/#/x', [])])]), _Bytes).

	test(mqtt_encode_packet_2_37, error(domain_error(mqtt_topic_filters, []))) :-
		mqtt::encode_packet(packet(unsubscribe, [packet_identifier(8), topic_filters([])]), _Bytes).

	test(mqtt_decode_packet_2_34, error(domain_error(mqtt_subscription_options, 3))) :-
		mqtt::decode_packet([130, 7, 0, 7, 0, 0, 1, 97, 3], _Packet).

	test(mqtt_decode_packet_2_35, error(domain_error(mqtt_fixed_header_flags, unsubscribe-0))) :-
		mqtt::decode_packet([160, 5, 0, 8, 0, 0, 1], _Packet).

	% ==========================================================================
	% Packet Stream I/O Tests
	% ==========================================================================

	test(mqtt_write_packet_2_01, deterministic(Bytes == [192, 0])) :-
		write_packet_bytes(packet(pingreq, []), Bytes).

	test(mqtt_read_packet_2_01, deterministic(Packet == packet(pingresp, []))) :-
		read_packet_from_bytes([208, 0], Packet).

	test(mqtt_read_packet_2_02, error(domain_error(mqtt_packet_stream, unexpected_end_of_file))) :-
		read_packet_from_bytes([], _Packet).

	test(mqtt_send_pingreq_1_01, deterministic(Bytes == [192, 0])) :-
		send_pingreq_bytes(Bytes).

	test(mqtt_ping_2_01, deterministic(Bytes == [192, 0])) :-
		ping_bytes([208, 0], Bytes).

	test(mqtt_ping_2_02, error(domain_error(mqtt_packet, expected(pingresp, packet(disconnect, [reason_code(normal_disconnection), properties([])]))))) :-
		ping_bytes([224, 0], _Bytes).

	test(mqtt_disconnect_2_01, deterministic(Bytes == [224, 0])) :-
		disconnect_bytes(Bytes).

	% ==========================================================================
	% Public Operation Tests
	% ==========================================================================

	test(mqtt_publish_4_01, deterministic(Bytes == [48, 5, 0, 1, 97, 0, 255])) :-
		publish_bytes([], a, [255], [], Bytes).

	test(mqtt_publish_4_02, deterministic(Bytes == [50, 7, 0, 1, 97, 0, 7, 0, 255])) :-
		publish_bytes([64, 2, 0, 7], a, [255], [qos(1), packet_identifier(7)], Bytes).

	test(mqtt_publish_4_03, deterministic(Bytes == [52, 7, 0, 1, 97, 0, 7, 0, 255, 98, 2, 0, 7])) :-
		publish_bytes([80, 2, 0, 7, 112, 2, 0, 7], a, [255], [qos(2), packet_identifier(7)], Bytes).

	test(mqtt_publish_4_04, error(domain_error(mqtt_reason_code, puback-unspecified_error))) :-
		publish_bytes([64, 4, 0, 7, 128, 0], a, [255], [qos(1), packet_identifier(7)], _Bytes).

	test(mqtt_subscribe_4_01, deterministic((Result == [granted_qos_1], Bytes == [130, 14, 0, 7, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43, 21]))) :-
		subscribe_bytes([144, 4, 0, 7, 0, 1], ['sensor/+'], [packet_identifier(7), maximum_qos(1), no_local(true), retain_handling(send_at_new_subscription)], Bytes, Result).

	test(mqtt_unsubscribe_4_01, deterministic((Result == [no_subscription_existed], Bytes == [162, 13, 0, 8, 0, 0, 8, 115, 101, 110, 115, 111, 114, 47, 43]))) :-
		unsubscribe_bytes([176, 4, 0, 8, 0, 17], ['sensor/+'], [packet_identifier(8)], Bytes, Result).

	test(mqtt_resolve_address_4_01, deterministic((Scheme == mqtt, Host == 'example.com', Port == 1883))) :-
		mqtt<<resolve_address('mqtt://example.com', Scheme, Host, Port).

	test(mqtt_resolve_address_4_02, deterministic((Scheme == mqtt, Host == 'example.com', Port == 1884))) :-
		mqtt<<resolve_address('mqtt://example.com:1884', Scheme, Host, Port).

	test(mqtt_resolve_address_4_03, deterministic((Scheme == mqtts, Host == 'example.com', Port == 8883))) :-
		mqtt<<resolve_address('mqtts://example.com', Scheme, Host, Port).

	test(mqtt_resolve_address_4_04, deterministic((Scheme == mqtts, Host == 'example.com', Port == 8884))) :-
		mqtt<<resolve_address('mqtts://example.com:8884', Scheme, Host, Port).

	test(mqtt_append_tls_transport_3_01, deterministic(Options == [])) :-
		mqtt<<append_tls_transport(mqtt, [], Options).

	test(mqtt_append_tls_transport_3_02, deterministic(Options == [connection_transport(tls)])) :-
		mqtt<<append_tls_transport(mqtts, [], Options).

	test(mqtt_append_tls_transport_3_03, deterministic(Options == [connection_transport(tcp)])) :-
		mqtt<<append_tls_transport(mqtts, [connection_transport(tcp)], Options).

	test(mqtt_connect_3_01, deterministic((RequestBytes == [16, 13, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 0, 0, 0, 224, 0], Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [request_file(_), response_file(_)], _Input, _Output), _Input, _Output, 'example.com', 1883, mqtt)))) :-
		connect_address_request_bytes('mqtt://example.com', [], [], [32, 3, 0, 0, 0], RequestBytes, Connection).

	test(mqtt_connect_3_02, deterministic((RequestBytes == [16, 13, 0, 4, 77, 81, 84, 84, 5, 2, 0, 60, 0, 0, 0, 224, 0], Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 8883, [connection_transport(tls), request_file(_), response_file(_)], _Input, _Output), _Input, _Output, 'example.com', 8883, mqtts)))) :-
		connect_address_request_bytes('mqtts://example.com', [], [], [32, 3, 0, 0, 0], RequestBytes, Connection).

	test(mqtt_connect_3_03, deterministic(connected_to(Connection, 'example.com', 8883, mqtts, [request_file(_), response_file(_), connection_transport(tcp), server_name('example.com')]))) :-
		connect_address_request_bytes('mqtts://example.com', [], [connection_transport(tcp), server_name('example.com')], [32, 3, 0, 0, 0], _RequestBytes, Connection).

	test(mqtt_connect_3_04, error(consistency_error(mqtt_options, scheme(mqtts), transport(probe_mqtt_tcp_transport)))) :-
		mqtt::connect('mqtts://example.com', _Connection, [transport(probe_mqtt_tcp_transport)]).

	test(mqtt_connect_3_05, error(domain_error(mqtt_connack_reason_code, server_unavailable))) :-
		connect_address_request_bytes('mqtt://example.com', [], [], [32, 3, 0, 136, 0], _RequestBytes, _Connection).

	test(mqtt_connect_4_01, deterministic((RequestBytes == [16, 16, 0, 4, 77, 81, 84, 84, 5, 0, 0, 30, 0, 0, 3, 102, 111, 111, 224, 0], Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [request_file(_), response_file(_)], _Input, _Output), _Input, _Output, 'example.com', 1883, mqtt)))) :-
		connect_host_request_bytes('example.com', 1883, [client_id(foo), clean_start(false), keep_alive(30)], [], [32, 3, 0, 0, 0], RequestBytes, Connection).

	test(mqtt_connect_4_02, deterministic(connected_to(Connection, 'example.com', 8883, mqtts, [connection_transport(tls), request_file(_), response_file(_)]))) :-
		connect_host_request_bytes('example.com', 8883, [scheme(mqtts)], [], [32, 3, 0, 0, 0], _RequestBytes, Connection).

	% auxiliary predicates

	connected_to(
		mqtt_connection(probe_mqtt_file_transport, probe_file_connection(Host, Port, Options, Input, Output), Input, Output, Host, Port, Scheme),
		Host,
		Port,
		Scheme,
		Options
	).

	connect_address_request_bytes(Address, Options, ConnectionOptions, ResponseBytes, RequestBytes, Connection) :-
		connect_request_files(InputFile, OutputFile, ResponseBytes),
		AllConnectionOptions = [request_file(OutputFile), response_file(InputFile)| ConnectionOptions],
		AllOptions = [transport(probe_mqtt_file_transport), connection_options(AllConnectionOptions)| Options],
		catch(
			mqtt::connect(Address, Connection, AllOptions),
			Error,
			( 	^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		mqtt::disconnect(Connection, []),
		read_file_bytes(OutputFile, RequestBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	connect_host_request_bytes(Host, Port, Options, ConnectionOptions, ResponseBytes, RequestBytes, Connection) :-
		connect_request_files(InputFile, OutputFile, ResponseBytes),
		AllConnectionOptions = [request_file(OutputFile), response_file(InputFile)| ConnectionOptions],
		AllOptions = [transport(probe_mqtt_file_transport), connection_options(AllConnectionOptions)| Options],
		catch(
			mqtt::connect(Host, Port, Connection, AllOptions),
			Error,
			( 	^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		mqtt::disconnect(Connection, []),
		read_file_bytes(OutputFile, RequestBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	connect_request_files(InputFile, OutputFile, ResponseBytes) :-
		^^file_path('test_mqtt_connect_input.tmp', InputFile),
		^^file_path('test_mqtt_connect_output.tmp', OutputFile),
		write_file_bytes(InputFile, ResponseBytes),
		^^clean_file(OutputFile).

	write_packet_bytes(Packet, Bytes) :-
		^^file_path('test_mqtt_packet_output.tmp', File),
		^^clean_file(File),
		open(File, write, Output, [type(binary)]),
		catch(
			mqtt<<write_packet(Output, Packet),
			Error,
			( 	close(Output),
				throw(Error)
			)
		),
		close(Output),
		read_file_bytes(File, Bytes),
		^^clean_file(File).

	read_packet_from_bytes(Bytes, Packet) :-
		^^file_path('test_mqtt_packet_input.tmp', File),
		write_file_bytes(File, Bytes),
		open(File, read, Input, [type(binary)]),
		catch(
			mqtt<<read_packet(Input, Packet),
			Error,
			( 	close(Input),
				^^clean_file(File),
				throw(Error)
			)
		),
		close(Input),
		^^clean_file(File).

	send_pingreq_bytes(Bytes) :-
		^^file_path('test_mqtt_ping_output.tmp', OutputFile),
		^^clean_file(OutputFile),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_transport, probe_connection('example.com', 1883, []), unused_input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::send_pingreq(Connection),
			Error,
			( 	close(Output),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		close(Output),
		read_file_bytes(OutputFile, Bytes),
		^^clean_file(OutputFile).

	ping_bytes(InputBytes, OutputBytes) :-
		^^file_path('test_mqtt_ping_input.tmp', InputFile),
		^^file_path('test_mqtt_ping_output.tmp', OutputFile),
		write_file_bytes(InputFile, InputBytes),
		^^clean_file(OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_transport, probe_connection('example.com', 1883, []), Input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::ping(Connection, []),
			Error,
			( 	close(Input),
				close(Output),
				^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		close(Input),
		close(Output),
		read_file_bytes(OutputFile, OutputBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	disconnect_bytes(OutputBytes) :-
		operation_files(InputFile, OutputFile, []),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [], Input, Output), Input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::disconnect(Connection, []),
			Error,
			( 	catch(close(Input), _, true),
				catch(close(Output), _, true),
				^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		read_file_bytes(OutputFile, OutputBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	publish_bytes(InputBytes, Topic, Payload, Options, OutputBytes) :-
		operation_files(InputFile, OutputFile, InputBytes),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [], Input, Output), Input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::publish(Connection, Topic, Payload, Options),
			Error,
			( 	close(Input),
				close(Output),
				^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		close(Input),
		close(Output),
		read_file_bytes(OutputFile, OutputBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	subscribe_bytes(InputBytes, TopicFilters, Options, OutputBytes, Result) :-
		operation_files(InputFile, OutputFile, InputBytes),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [], Input, Output), Input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::subscribe(Connection, TopicFilters, Result, Options),
			Error,
			( 	close(Input),
				close(Output),
				^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		close(Input),
		close(Output),
		read_file_bytes(OutputFile, OutputBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	unsubscribe_bytes(InputBytes, TopicFilters, Options, OutputBytes, Result) :-
		operation_files(InputFile, OutputFile, InputBytes),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		Connection = mqtt_connection(probe_mqtt_file_transport, probe_file_connection('example.com', 1883, [], Input, Output), Input, Output, 'example.com', 1883, mqtt),
		catch(
			mqtt::unsubscribe(Connection, TopicFilters, Result, Options),
			Error,
			( 	close(Input),
				close(Output),
				^^clean_file(InputFile),
				^^clean_file(OutputFile),
				throw(Error)
			)
		),
		close(Input),
		close(Output),
		read_file_bytes(OutputFile, OutputBytes),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	operation_files(InputFile, OutputFile, InputBytes) :-
		^^file_path('test_mqtt_operation_input.tmp', InputFile),
		^^file_path('test_mqtt_operation_output.tmp', OutputFile),
		write_file_bytes(InputFile, InputBytes),
		^^clean_file(OutputFile).

	write_file_bytes(File, Bytes) :-
		^^clean_file(File),
		open(File, write, Output, [type(binary)]),
		write_bytes(Bytes, Output),
		close(Output).

	read_file_bytes(File, Bytes) :-
		open(File, read, Input, [type(binary)]),
		read_bytes(Input, Bytes),
		close(Input).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	read_bytes(Input, Bytes) :-
		get_byte(Input, Byte),
		read_bytes(Input, Byte, Bytes).

	read_bytes(_Input, -1, []) :-
		!.
	read_bytes(Input, Byte, [Byte| Bytes]) :-
		get_byte(Input, NextByte),
		read_bytes(Input, NextByte, Bytes).

:- end_object.
