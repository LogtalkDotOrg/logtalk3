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


:- object(mqtt,
	imports([options, http_origin_site_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Portable MQTT 5 client predicates using ``http_transport_protocol`` implementations.'
	]).

	:- public(connect/3).
	:- mode(connect(+atom, --compound, +list), one_or_error).
	:- info(connect/3, [
		comment is 'Opens a transport connection for an MQTT or MQTT-over-TLS address and completes the MQTT CONNECT/CONNACK exchange.',
		argnames is ['Address', 'Connection', 'Options']
	]).

	:- public(connect/4).
	:- mode(connect(+atom, +integer, --compound, +list), one_or_error).
	:- info(connect/4, [
		comment is 'Opens a transport connection to the given host and port and completes the MQTT CONNECT/CONNACK exchange. The ``scheme/1`` option selects MQTT over TCP or TLS.',
		argnames is ['Host', 'Port', 'Connection', 'Options']
	]).

	:- public(disconnect/2).
	:- mode(disconnect(+compound, +list), one_or_error).
	:- info(disconnect/2, [
		comment is 'Closes an MQTT transport connection. MQTT DISCONNECT packet exchange will be layered on this transport operation.',
		argnames is ['Connection', 'Options']
	]).

	:- public(connection_alive/1).
	:- mode(connection_alive(+compound), zero_or_one).
	:- info(connection_alive/1, [
		comment is 'True if the term is an MQTT connection handle created by this object.',
		argnames is ['Connection']
	]).

	:- public(packet_type/2).
	:- mode(packet_type(?atom, ?integer), zero_or_more).
	:- info(packet_type/2, [
		comment is 'Table of MQTT control packet type names and numeric codes.',
		argnames is ['Type', 'Code']
	]).

	:- public(reason_code/3).
	:- mode(reason_code(?atom, ?atom, ?integer), zero_or_more).
	:- info(reason_code/3, [
		comment is 'Table of MQTT reason code names and numeric codes by packet type.',
		argnames is ['PacketType', 'Name', 'Code']
	]).

	:- public(publish/4).
	:- mode(publish(+compound, +atom, +term, +list), one_or_error).
	:- info(publish/4, [
		comment is 'Publishes a message and waits synchronously for the required MQTT acknowledgement.',
		argnames is ['Connection', 'Topic', 'Payload', 'Options']
	]).

	:- public(subscribe/4).
	:- mode(subscribe(+compound, +list, --list, +list), one_or_error).
	:- info(subscribe/4, [
		comment is 'Subscribes to topic filters and waits synchronously for the matching SUBACK.',
		argnames is ['Connection', 'TopicFilters', 'Result', 'Options']
	]).

	:- public(unsubscribe/4).
	:- mode(unsubscribe(+compound, +list, --list, +list), one_or_error).
	:- info(unsubscribe/4, [
		comment is 'Unsubscribes from topic filters and waits synchronously for the matching UNSUBACK.',
		argnames is ['Connection', 'TopicFilters', 'Result', 'Options']
	]).

	:- public(receive/3).
	:- mode(receive(+compound, --compound, +list), one_or_error).
	:- info(receive/3, [
		comment is 'Receives the next MQTT packet.',
		argnames is ['Connection', 'Message', 'Options']
	]).

	:- public(send_pingreq/1).
	:- mode(send_pingreq(+compound), one_or_error).
	:- info(send_pingreq/1, [
		comment is 'Sends a PINGREQ packet.',
		argnames is ['Connection']
	]).

	:- public(ping/2).
	:- mode(ping(+compound, +list), one_or_error).
	:- info(ping/2, [
		comment is 'Sends PINGREQ and waits synchronously for PINGRESP.',
		argnames is ['Connection', 'Options']
	]).

	:- public(encode_packet/2).
	:- mode(encode_packet(+compound, --list(integer)), one_or_error).
	:- info(encode_packet/2, [
		comment is 'Encodes a normalized MQTT packet term.',
		argnames is ['Packet', 'Bytes']
	]).

	:- public(decode_packet/2).
	:- mode(decode_packet(+list(integer), --compound), one_or_error).
	:- info(decode_packet/2, [
		comment is 'Decodes bytes into a normalized MQTT packet term.',
		argnames is ['Bytes', 'Packet']
	]).

	:- private(resolve_address/4).
	:- mode(resolve_address(+atom, -atom, -atom, -integer), one_or_error).
	:- info(resolve_address/4, [
		comment is 'Resolves an MQTT address into scheme, host, and port.',
		argnames is ['Address', 'Scheme', 'Host', 'Port']
	]).

	:- private(resolve_transport/3).
	:- mode(resolve_transport(+atom, +object_identifier, -object_identifier), one_or_error).
	:- info(resolve_transport/3, [
		comment is 'Resolves an explicit or default transport object for the given MQTT scheme.',
		argnames is ['Scheme', 'Transport0', 'Transport']
	]).

	:- private(append_tls_transport/3).
	:- mode(append_tls_transport(+atom, +list, -list), one_or_error).
	:- info(append_tls_transport/3, [
		comment is 'Adds ``connection_transport(tls)`` to connection options for MQTT-over-TLS unless already specified.',
		argnames is ['Scheme', 'Options', 'OptionsWithTransport']
	]).

	:- private(write_packet/2).
	:- mode(write_packet(+stream, +compound), one_or_error).
	:- info(write_packet/2, [
		comment is 'Encodes and writes an MQTT packet to a binary output stream.',
		argnames is ['Output', 'Packet']
	]).

	:- private(read_packet/2).
	:- mode(read_packet(+stream, --compound), one_or_error).
	:- info(read_packet/2, [
		comment is 'Reads and decodes one MQTT packet from a binary input stream.',
		argnames is ['Input', 'Packet']
	]).

	:- private(encode_uint8/2).
	:- mode(encode_uint8(+integer, -list(integer)), one_or_error).
	:- info(encode_uint8/2, [
		comment is 'Encodes an unsigned 8-bit integer.',
		argnames is ['Value', 'Bytes']
	]).

	:- private(decode_uint8/3).
	:- mode(decode_uint8(+list(integer), -integer, -list(integer)), one_or_error).
	:- info(decode_uint8/3, [
		comment is 'Decodes an unsigned 8-bit integer.',
		argnames is ['Bytes', 'Value', 'Rest']
	]).

	:- private(encode_uint16/2).
	:- mode(encode_uint16(+integer, -list(integer)), one_or_error).
	:- info(encode_uint16/2, [
		comment is 'Encodes an unsigned 16-bit integer in network byte order.',
		argnames is ['Value', 'Bytes']
	]).

	:- private(decode_uint16/3).
	:- mode(decode_uint16(+list(integer), -integer, -list(integer)), one_or_error).
	:- info(decode_uint16/3, [
		comment is 'Decodes an unsigned 16-bit integer in network byte order.',
		argnames is ['Bytes', 'Value', 'Rest']
	]).

	:- private(encode_uint32/2).
	:- mode(encode_uint32(+integer, -list(integer)), one_or_error).
	:- info(encode_uint32/2, [
		comment is 'Encodes an unsigned 32-bit integer in network byte order.',
		argnames is ['Value', 'Bytes']
	]).

	:- private(decode_uint32/3).
	:- mode(decode_uint32(+list(integer), -integer, -list(integer)), one_or_error).
	:- info(decode_uint32/3, [
		comment is 'Decodes an unsigned 32-bit integer in network byte order.',
		argnames is ['Bytes', 'Value', 'Rest']
	]).

	:- private(encode_varint/2).
	:- mode(encode_varint(+integer, -list(integer)), one_or_error).
	:- info(encode_varint/2, [
		comment is 'Encodes an MQTT variable byte integer.',
		argnames is ['Value', 'Bytes']
	]).

	:- private(decode_varint/3).
	:- mode(decode_varint(+list(integer), -integer, -list(integer)), one_or_error).
	:- info(decode_varint/3, [
		comment is 'Decodes an MQTT variable byte integer, rejecting non-minimal encodings and overflows.',
		argnames is ['Bytes', 'Value', 'Rest']
	]).

	:- private(encode_utf8_string/2).
	:- mode(encode_utf8_string(+atom, -list(integer)), one_or_error).
	:- info(encode_utf8_string/2, [
		comment is 'Encodes an MQTT UTF-8 encoded string with a two-byte length prefix.',
		argnames is ['String', 'Bytes']
	]).

	:- private(decode_utf8_string/3).
	:- mode(decode_utf8_string(+list(integer), -atom, -list(integer)), one_or_error).
	:- info(decode_utf8_string/3, [
		comment is 'Decodes an MQTT UTF-8 encoded string with a two-byte length prefix.',
		argnames is ['Bytes', 'String', 'Rest']
	]).

	:- private(encode_binary_data/2).
	:- mode(encode_binary_data(+list(integer), -list(integer)), one_or_error).
	:- info(encode_binary_data/2, [
		comment is 'Encodes MQTT binary data with a two-byte length prefix.',
		argnames is ['Data', 'Bytes']
	]).

	:- private(decode_binary_data/3).
	:- mode(decode_binary_data(+list(integer), -list(integer), -list(integer)), one_or_error).
	:- info(decode_binary_data/3, [
		comment is 'Decodes MQTT binary data with a two-byte length prefix.',
		argnames is ['Bytes', 'Data', 'Rest']
	]).

	:- private(encode_utf8_string_pair/2).
	:- mode(encode_utf8_string_pair(+compound, -list(integer)), one_or_error).
	:- info(encode_utf8_string_pair/2, [
		comment is 'Encodes an MQTT UTF-8 string pair as two MQTT UTF-8 encoded strings.',
		argnames is ['Pair', 'Bytes']
	]).

	:- private(decode_utf8_string_pair/3).
	:- mode(decode_utf8_string_pair(+list(integer), -compound, -list(integer)), one_or_error).
	:- info(decode_utf8_string_pair/3, [
		comment is 'Decodes an MQTT UTF-8 string pair from two MQTT UTF-8 encoded strings.',
		argnames is ['Bytes', 'Pair', 'Rest']
	]).

	:- private(encode_properties/2).
	:- mode(encode_properties(+list, -list(integer)), one_or_error).
	:- info(encode_properties/2, [
		comment is 'Encodes an MQTT property list with its variable byte integer length prefix.',
		argnames is ['Properties', 'Bytes']
	]).

	:- private(decode_properties/3).
	:- mode(decode_properties(+list(integer), -list, -list(integer)), one_or_error).
	:- info(decode_properties/3, [
		comment is 'Decodes an MQTT property list and returns the remaining packet bytes.',
		argnames is ['Bytes', 'Properties', 'Rest']
	]).

	:- private(encode_property_list/2).
	:- mode(encode_property_list(+list, -list(integer)), one_or_error).
	:- info(encode_property_list/2, [
		comment is 'Encodes MQTT properties without the property length prefix.',
		argnames is ['Properties', 'Bytes']
	]).

	:- private(decode_property_list/2).
	:- mode(decode_property_list(+list(integer), -list), one_or_error).
	:- info(decode_property_list/2, [
		comment is 'Decodes MQTT properties from a property body byte list.',
		argnames is ['Bytes', 'Properties']
	]).

	:- private(encode_property/2).
	:- mode(encode_property(+compound, -list(integer)), one_or_error).
	:- info(encode_property/2, [
		comment is 'Encodes a single MQTT property.',
		argnames is ['Property', 'Bytes']
	]).

	:- private(decode_property/3).
	:- mode(decode_property(+list(integer), -compound, -list(integer)), one_or_error).
	:- info(decode_property/3, [
		comment is 'Decodes a single MQTT property.',
		argnames is ['Bytes', 'Property', 'Rest']
	]).

	:- private(mqtt_property/3).
	:- mode(mqtt_property(?atom, ?integer, ?atom), zero_or_more).
	:- info(mqtt_property/3, [
		comment is 'Table of MQTT property names, identifiers, and value types.',
		argnames is ['Name', 'Identifier', 'Type']
	]).

	:- private(encode_fixed_header/4).
	:- mode(encode_fixed_header(+atom, +integer, +integer, -list(integer)), one_or_error).
	:- info(encode_fixed_header/4, [
		comment is 'Encodes an MQTT fixed header from packet type, flags, and remaining length.',
		argnames is ['Type', 'Flags', 'RemainingLength', 'Bytes']
	]).

	:- private(decode_fixed_header/5).
	:- mode(decode_fixed_header(+list(integer), -atom, -integer, -integer, -list(integer)), one_or_error).
	:- info(decode_fixed_header/5, [
		comment is 'Decodes an MQTT fixed header into packet type, flags, remaining length, and remaining bytes.',
		argnames is ['Bytes', 'Type', 'Flags', 'RemainingLength', 'Rest']
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, take/4, valid/1 as proper_list/1
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atom_concat/3, atomic_list_concat/2
	]).

	connect(Address, Connection, Options) :-
		parse_connection_options(Options, Transport0, ConnectionOptions0, _SchemeOption, ConnectFields),
		resolve_address(Address, Scheme, Host, Port),
		establish_mqtt_connection(Scheme, Host, Port, Transport0, ConnectionOptions0, ConnectFields, Connection).

	connect(Host, Port, Connection, Options) :-
		parse_connection_options(Options, Transport0, ConnectionOptions0, Scheme, ConnectFields),
		validate_endpoint(Host, Port),
		establish_mqtt_connection(Scheme, Host, Port, Transport0, ConnectionOptions0, ConnectFields, Connection).

	disconnect(mqtt_connection(Transport, RawConnection, _Input, Output, _Host, _Port, _Scheme), Options) :-
		^^check_options(Options),
		catch(
			write_packet(Output, packet(disconnect, [])),
			Error,
			( 	catch(Transport::close_connection(RawConnection), _, true),
				throw(Error)
			)
		),
		Transport::close_connection(RawConnection).

	connection_alive(mqtt_connection(_Transport, _RawConnection, _Input, _Output, _Host, _Port, _Scheme)).

	packet_type(connect, 1).
	packet_type(connack, 2).
	packet_type(publish, 3).
	packet_type(puback, 4).
	packet_type(pubrec, 5).
	packet_type(pubrel, 6).
	packet_type(pubcomp, 7).
	packet_type(subscribe, 8).
	packet_type(suback, 9).
	packet_type(unsubscribe, 10).
	packet_type(unsuback, 11).
	packet_type(pingreq, 12).
	packet_type(pingresp, 13).
	packet_type(disconnect, 14).
	packet_type(auth, 15).

	reason_code(connack, success, 0).
	reason_code(connack, unspecified_error, 128).
	reason_code(connack, malformed_packet, 129).
	reason_code(connack, protocol_error, 130).
	reason_code(connack, implementation_specific_error, 131).
	reason_code(connack, unsupported_protocol_version, 132).
	reason_code(connack, client_identifier_not_valid, 133).
	reason_code(connack, bad_user_name_or_password, 134).
	reason_code(connack, not_authorized, 135).
	reason_code(connack, server_unavailable, 136).
	reason_code(connack, server_busy, 137).
	reason_code(connack, banned, 138).
	reason_code(connack, bad_authentication_method, 140).
	reason_code(connack, topic_name_invalid, 144).
	reason_code(connack, packet_too_large, 149).
	reason_code(connack, quota_exceeded, 151).
	reason_code(connack, payload_format_invalid, 153).
	reason_code(connack, retain_not_supported, 154).
	reason_code(connack, qos_not_supported, 155).
	reason_code(connack, use_another_server, 156).
	reason_code(connack, server_moved, 157).
	reason_code(connack, connection_rate_exceeded, 159).

	reason_code(disconnect, normal_disconnection, 0).
	reason_code(disconnect, disconnect_with_will_message, 4).
	reason_code(disconnect, unspecified_error, 128).
	reason_code(disconnect, malformed_packet, 129).
	reason_code(disconnect, protocol_error, 130).
	reason_code(disconnect, implementation_specific_error, 131).
	reason_code(disconnect, not_authorized, 135).
	reason_code(disconnect, server_busy, 137).
	reason_code(disconnect, server_shutting_down, 139).
	reason_code(disconnect, keep_alive_timeout, 141).
	reason_code(disconnect, session_taken_over, 142).
	reason_code(disconnect, topic_filter_invalid, 143).
	reason_code(disconnect, topic_name_invalid, 144).
	reason_code(disconnect, receive_maximum_exceeded, 147).
	reason_code(disconnect, topic_alias_invalid, 148).
	reason_code(disconnect, packet_too_large, 149).
	reason_code(disconnect, message_rate_too_high, 150).
	reason_code(disconnect, quota_exceeded, 151).
	reason_code(disconnect, administrative_action, 152).
	reason_code(disconnect, payload_format_invalid, 153).
	reason_code(disconnect, retain_not_supported, 154).
	reason_code(disconnect, qos_not_supported, 155).
	reason_code(disconnect, use_another_server, 156).
	reason_code(disconnect, server_moved, 157).
	reason_code(disconnect, shared_subscriptions_not_supported, 158).
	reason_code(disconnect, connection_rate_exceeded, 159).
	reason_code(disconnect, maximum_connect_time, 160).
	reason_code(disconnect, subscription_identifiers_not_supported, 161).
	reason_code(disconnect, wildcard_subscriptions_not_supported, 162).

	reason_code(auth, success, 0).
	reason_code(auth, continue_authentication, 24).
	reason_code(auth, re_authenticate, 25).

	reason_code(puback, success, 0).
	reason_code(puback, no_matching_subscribers, 16).
	reason_code(puback, unspecified_error, 128).
	reason_code(puback, implementation_specific_error, 131).
	reason_code(puback, not_authorized, 135).
	reason_code(puback, topic_name_invalid, 144).
	reason_code(puback, packet_identifier_in_use, 145).
	reason_code(puback, quota_exceeded, 151).
	reason_code(puback, payload_format_invalid, 153).

	reason_code(pubrec, success, 0).
	reason_code(pubrec, no_matching_subscribers, 16).
	reason_code(pubrec, unspecified_error, 128).
	reason_code(pubrec, implementation_specific_error, 131).
	reason_code(pubrec, not_authorized, 135).
	reason_code(pubrec, topic_name_invalid, 144).
	reason_code(pubrec, packet_identifier_in_use, 145).
	reason_code(pubrec, quota_exceeded, 151).
	reason_code(pubrec, payload_format_invalid, 153).

	reason_code(pubrel, success, 0).
	reason_code(pubrel, packet_identifier_not_found, 146).

	reason_code(pubcomp, success, 0).
	reason_code(pubcomp, packet_identifier_not_found, 146).

	reason_code(suback, granted_qos_0, 0).
	reason_code(suback, granted_qos_1, 1).
	reason_code(suback, granted_qos_2, 2).
	reason_code(suback, unspecified_error, 128).
	reason_code(suback, implementation_specific_error, 131).
	reason_code(suback, not_authorized, 135).
	reason_code(suback, topic_filter_invalid, 143).
	reason_code(suback, packet_identifier_in_use, 145).
	reason_code(suback, quota_exceeded, 151).
	reason_code(suback, shared_subscriptions_not_supported, 158).
	reason_code(suback, subscription_identifiers_not_supported, 161).
	reason_code(suback, wildcard_subscriptions_not_supported, 162).

	reason_code(unsuback, success, 0).
	reason_code(unsuback, no_subscription_existed, 17).
	reason_code(unsuback, unspecified_error, 128).
	reason_code(unsuback, implementation_specific_error, 131).
	reason_code(unsuback, not_authorized, 135).
	reason_code(unsuback, topic_filter_invalid, 143).
	reason_code(unsuback, packet_identifier_in_use, 145).

	mqtt_property(payload_format_indicator, 1, byte_0_1).
	mqtt_property(message_expiry_interval, 2, uint32).
	mqtt_property(content_type, 3, utf8_string).
	mqtt_property(response_topic, 8, topic_name).
	mqtt_property(correlation_data, 9, binary_data).
	mqtt_property(subscription_identifier, 11, positive_varint).
	mqtt_property(session_expiry_interval, 17, uint32).
	mqtt_property(assigned_client_identifier, 18, utf8_string).
	mqtt_property(server_keep_alive, 19, uint16).
	mqtt_property(authentication_method, 21, utf8_string).
	mqtt_property(authentication_data, 22, binary_data).
	mqtt_property(request_problem_information, 23, boolean).
	mqtt_property(will_delay_interval, 24, uint32).
	mqtt_property(request_response_information, 25, boolean).
	mqtt_property(response_information, 26, utf8_string).
	mqtt_property(server_reference, 28, utf8_string).
	mqtt_property(reason_string, 31, utf8_string).
	mqtt_property(receive_maximum, 33, positive_uint16).
	mqtt_property(topic_alias_maximum, 34, uint16).
	mqtt_property(topic_alias, 35, positive_uint16).
	mqtt_property(maximum_qos, 36, byte_0_1).
	mqtt_property(retain_available, 37, boolean).
	mqtt_property(user_property, 38, utf8_string_pair).
	mqtt_property(maximum_packet_size, 39, positive_uint32).
	mqtt_property(wildcard_subscription_available, 40, boolean).
	mqtt_property(subscription_identifier_available, 41, boolean).
	mqtt_property(shared_subscription_available, 42, boolean).

	publish(mqtt_connection(_Transport, _RawConnection, Input, Output, _Host, _Port, _Scheme), Topic, Payload, Options) :-
		parse_operation_options(Options, PacketIdentifier, Properties, OperationOptions),
		^^option(qos(QoS), OperationOptions),
		^^option(dup(Dup), OperationOptions),
		^^option(retain(Retain), OperationOptions),
		PublishFields = [dup(Dup), qos(QoS), retain(Retain), topic_name(Topic), packet_identifier(PacketIdentifier), properties(Properties), payload(Payload)],
		write_packet(Output, packet(publish, PublishFields)),
		wait_for_publish_acknowledgement(QoS, PacketIdentifier, Input, Output).

	subscribe(mqtt_connection(_Transport, _RawConnection, Input, Output, _Host, _Port, _Scheme), TopicFilters, Result, Options) :-
		parse_operation_options(Options, PacketIdentifier, Properties, OperationOptions),
		default_subscription_options(OperationOptions, SubscriptionOptions),
		build_subscriptions(TopicFilters, SubscriptionOptions, Subscriptions),
		write_packet(Output, packet(subscribe, [packet_identifier(PacketIdentifier), properties(Properties), subscriptions(Subscriptions)])),
		read_packet(Input, Packet),
		accept_subscription_acknowledgement(suback, PacketIdentifier, Packet, Result).

	unsubscribe(mqtt_connection(_Transport, _RawConnection, Input, Output, _Host, _Port, _Scheme), TopicFilters, Result, Options) :-
		parse_operation_options(Options, PacketIdentifier, Properties, _OperationOptions),
		write_packet(Output, packet(unsubscribe, [packet_identifier(PacketIdentifier), properties(Properties), topic_filters(TopicFilters)])),
		read_packet(Input, Packet),
		accept_subscription_acknowledgement(unsuback, PacketIdentifier, Packet, Result).

	receive(mqtt_connection(_Transport, _RawConnection, Input, _Output, _Host, _Port, _Scheme), Message, _Options) :-
		read_packet(Input, Message).

	send_pingreq(mqtt_connection(_Transport, _RawConnection, _Input, Output, _Host, _Port, _Scheme)) :-
		write_packet(Output, packet(pingreq, [])).

	ping(mqtt_connection(_Transport, _RawConnection, Input, Output, _Host, _Port, _Scheme), _Options) :-
		write_packet(Output, packet(pingreq, [])),
		read_packet(Input, Packet),
		( 	Packet == packet(pingresp, []) ->
			true
		; 	domain_error(mqtt_packet, expected(pingresp, Packet))
		).

	encode_packet(packet(connect, Fields), Bytes) :-
		!,
		encode_connect_packet(Fields, Bytes).
	encode_packet(packet(connack, Fields), Bytes) :-
		!,
		encode_connack_packet(Fields, Bytes).
	encode_packet(packet(disconnect, Fields), Bytes) :-
		!,
		encode_disconnect_packet(Fields, Bytes).
	encode_packet(packet(auth, Fields), Bytes) :-
		!,
		encode_auth_packet(Fields, Bytes).
	encode_packet(packet(publish, Fields), Bytes) :-
		!,
		encode_publish_packet(Fields, Bytes).
	encode_packet(packet(subscribe, Fields), Bytes) :-
		!,
		encode_subscribe_packet(Fields, Bytes).
	encode_packet(packet(unsubscribe, Fields), Bytes) :-
		!,
		encode_unsubscribe_packet(Fields, Bytes).
	encode_packet(packet(Type, Fields), Bytes) :-
		ack_packet_type(Type),
		!,
		encode_ack_packet(Type, Fields, Bytes).
	encode_packet(packet(Type, Fields), Bytes) :-
		subscription_ack_packet_type(Type),
		!,
		encode_subscription_ack_packet(Type, Fields, Bytes).
	encode_packet(packet(Type, []), Bytes) :-
		zero_length_packet_type(Type),
		!,
		encode_fixed_header(Type, 0, 0, Bytes).
	encode_packet(_Packet, _Bytes) :-
		pending_packet_support.

	decode_packet(Bytes, Packet) :-
		decode_fixed_header(Bytes, Type, Flags, RemainingLength, Rest0),
		take(RemainingLength, Rest0, Body, Rest),
		( 	Rest == [] ->
			decode_packet_body(Type, Flags, RemainingLength, Body, Packet)
		; 	domain_error(mqtt_packet_trailing_bytes, Rest)
		).

	zero_length_packet_type(pingreq).
	zero_length_packet_type(pingresp).

	decode_packet_body(connect, 0, _RemainingLength, Body, packet(connect, [client_id(ClientId), clean_start(CleanStart), keep_alive(KeepAlive), properties(Properties)])) :-
		!,
		decode_utf8_string(Body, 'MQTT', Rest1),
		decode_uint8(Rest1, 5, Rest2),
		decode_uint8(Rest2, ConnectFlags, Rest3),
		decode_connect_flags(ConnectFlags, CleanStart),
		decode_uint16(Rest3, KeepAlive, Rest4),
		decode_properties(Rest4, Properties, Rest5),
		decode_utf8_string(Rest5, ClientId, []).
	decode_packet_body(connect, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, connect-Flags).
	decode_packet_body(connack, 0, _RemainingLength, Body, packet(connack, [session_present(SessionPresent), reason_code(ReasonCode), properties(Properties)])) :-
		!,
		decode_uint8(Body, AcknowledgeFlags, Rest1),
		decode_connack_flags(AcknowledgeFlags, SessionPresent),
		decode_reason_code(connack, Rest1, ReasonCode, Rest2),
		decode_properties(Rest2, Properties, []).
	decode_packet_body(connack, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, connack-Flags).
	decode_packet_body(disconnect, 0, 0, [], packet(disconnect, [reason_code(normal_disconnection), properties([])])) :-
		!.
	decode_packet_body(disconnect, 0, _RemainingLength, Body, packet(disconnect, [reason_code(ReasonCode), properties(Properties)])) :-
		!,
		decode_reason_code(disconnect, Body, ReasonCode, Rest),
		( 	Rest == [] ->
			Properties = []
		; 	decode_properties(Rest, Properties, [])
		).
	decode_packet_body(disconnect, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, disconnect-Flags).
	decode_packet_body(auth, 0, 0, [], packet(auth, [reason_code(success), properties([])])) :-
		!.
	decode_packet_body(auth, 0, _RemainingLength, Body, packet(auth, [reason_code(ReasonCode), properties(Properties)])) :-
		!,
		decode_reason_code(auth, Body, ReasonCode, Rest),
		( 	Rest == [] ->
			Properties = []
		; 	decode_properties(Rest, Properties, [])
		).
	decode_packet_body(auth, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, auth-Flags).
	decode_packet_body(publish, Flags, _RemainingLength, Body, Packet) :-
		!,
		decode_publish_packet_body(Flags, Body, Packet).
	decode_packet_body(subscribe, 2, _RemainingLength, Body, Packet) :-
		!,
		decode_subscribe_packet_body(Body, Packet).
	decode_packet_body(subscribe, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, subscribe-Flags).
	decode_packet_body(unsubscribe, 2, _RemainingLength, Body, Packet) :-
		!,
		decode_unsubscribe_packet_body(Body, Packet).
	decode_packet_body(unsubscribe, Flags, _RemainingLength, _Body, _Packet) :-
		domain_error(mqtt_fixed_header_flags, unsubscribe-Flags).
	decode_packet_body(Type, _Flags, _RemainingLength, Body, Packet) :-
		ack_packet_type(Type),
		!,
		decode_ack_packet_body(Type, Body, Packet).
	decode_packet_body(Type, 0, _RemainingLength, Body, Packet) :-
		subscription_ack_packet_type(Type),
		!,
		decode_subscription_ack_packet_body(Type, Body, Packet).
	decode_packet_body(Type, Flags, _RemainingLength, _Body, _Packet) :-
		subscription_ack_packet_type(Type),
		domain_error(mqtt_fixed_header_flags, Type-Flags).

	decode_packet_body(Type, 0, 0, [], packet(Type, [])) :-
		zero_length_packet_type(Type),
		!.
	decode_packet_body(Type, _Flags, RemainingLength, _Rest, _Packet) :-
		zero_length_packet_type(Type),
		domain_error(mqtt_packet_remaining_length, Type-RemainingLength).
	decode_packet_body(_Type, _Flags, _RemainingLength, _Rest, _Packet) :-
		pending_packet_support.

	encode_connect_packet(Fields, Bytes) :-
		field_option(client_id, Fields, ClientId, ''),
		field_option(clean_start, Fields, CleanStart, true),
		field_option(keep_alive, Fields, KeepAlive, 60),
		field_option(properties, Fields, Properties, []),
		encode_connect_flags(CleanStart, ConnectFlags),
		encode_utf8_string('MQTT', ProtocolNameBytes),
		encode_uint8(5, ProtocolLevelBytes),
		encode_uint8(ConnectFlags, ConnectFlagsBytes),
		encode_uint16(KeepAlive, KeepAliveBytes),
		encode_properties(Properties, PropertyBytes),
		encode_utf8_string(ClientId, ClientIdBytes),
		append(ProtocolNameBytes, ProtocolLevelBytes, Bytes0),
		append(Bytes0, ConnectFlagsBytes, Bytes1),
		append(Bytes1, KeepAliveBytes, Bytes2),
		append(Bytes2, PropertyBytes, Bytes3),
		append(Bytes3, ClientIdBytes, VariableHeaderAndPayload),
		length(VariableHeaderAndPayload, RemainingLength),
		encode_fixed_header(connect, 0, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeaderAndPayload, Bytes).

	encode_connack_packet(Fields, Bytes) :-
		field_option(session_present, Fields, SessionPresent, false),
		field_option(reason_code, Fields, ReasonCode, success),
		field_option(properties, Fields, Properties, []),
		encode_connack_flags(SessionPresent, AcknowledgeFlags),
		encode_reason_code(connack, ReasonCode, ReasonCodeBytes),
		encode_properties(Properties, PropertyBytes),
		append([AcknowledgeFlags| ReasonCodeBytes], PropertyBytes, VariableHeader),
		length(VariableHeader, RemainingLength),
		encode_fixed_header(connack, 0, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeader, Bytes).

	encode_disconnect_packet([], Bytes) :-
		!,
		encode_fixed_header(disconnect, 0, 0, Bytes).
	encode_disconnect_packet(Fields, Bytes) :-
		field_option(reason_code, Fields, ReasonCode, normal_disconnection),
		field_option(properties, Fields, Properties, []),
		encode_disconnect_variable_header(ReasonCode, Properties, VariableHeader),
		length(VariableHeader, RemainingLength),
		encode_fixed_header(disconnect, 0, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeader, Bytes).

	encode_disconnect_variable_header(normal_disconnection, [], []) :-
		!.
	encode_disconnect_variable_header(ReasonCode, Properties, VariableHeader) :-
		encode_reason_code(disconnect, ReasonCode, ReasonCodeBytes),
		encode_properties(Properties, PropertyBytes),
		append(ReasonCodeBytes, PropertyBytes, VariableHeader).

	encode_auth_packet([], Bytes) :-
		!,
		encode_fixed_header(auth, 0, 0, Bytes).
	encode_auth_packet(Fields, Bytes) :-
		field_option(reason_code, Fields, ReasonCode, success),
		field_option(properties, Fields, Properties, []),
		encode_auth_variable_header(ReasonCode, Properties, VariableHeader),
		length(VariableHeader, RemainingLength),
		encode_fixed_header(auth, 0, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeader, Bytes).

	encode_auth_variable_header(success, [], []) :-
		!.
	encode_auth_variable_header(ReasonCode, Properties, VariableHeader) :-
		encode_reason_code(auth, ReasonCode, ReasonCodeBytes),
		encode_properties(Properties, PropertyBytes),
		append(ReasonCodeBytes, PropertyBytes, VariableHeader).

	encode_publish_packet(Fields, Bytes) :-
		field_option(dup, Fields, Dup, false),
		field_option(qos, Fields, QoS, 0),
		field_option(retain, Fields, Retain, false),
		field_option(topic_name, Fields, TopicName, ''),
		field_option(packet_identifier, Fields, PacketIdentifier, 0),
		field_option(properties, Fields, Properties, []),
		field_option(payload, Fields, Payload, []),
		encode_publish_flags(Dup, QoS, Retain, Flags),
		encode_publish_variable_header(QoS, TopicName, PacketIdentifier, Properties, VariableHeader),
		validate_byte_list(Payload),
		append(VariableHeader, Payload, VariableHeaderAndPayload),
		length(VariableHeaderAndPayload, RemainingLength),
		encode_fixed_header(publish, Flags, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeaderAndPayload, Bytes).

	decode_publish_packet_body(Flags, Body, packet(publish, Fields)) :-
		decode_publish_flags(Flags, Dup, QoS, Retain),
		decode_utf8_string(Body, TopicName, Rest0),
		validate_topic_name(TopicName),
		decode_publish_packet_identifier(QoS, Rest0, PacketIdentifierFields, Rest1),
		decode_properties(Rest1, Properties, Payload),
		validate_byte_list(Payload),
		Fields0 = [dup(Dup), qos(QoS), retain(Retain), topic_name(TopicName)],
		append(Fields0, PacketIdentifierFields, Fields1),
		append(Fields1, [properties(Properties), payload(Payload)], Fields).

	encode_publish_variable_header(QoS, TopicName, PacketIdentifier, Properties, VariableHeader) :-
		validate_topic_name(TopicName),
		encode_utf8_string(TopicName, TopicNameBytes),
		encode_publish_packet_identifier(QoS, PacketIdentifier, PacketIdentifierBytes),
		encode_properties(Properties, PropertyBytes),
		append(TopicNameBytes, PacketIdentifierBytes, Bytes0),
		append(Bytes0, PropertyBytes, VariableHeader).

	encode_publish_packet_identifier(0, _PacketIdentifier, []) :-
		!.
	encode_publish_packet_identifier(_QoS, PacketIdentifier, Bytes) :-
		encode_packet_identifier(PacketIdentifier, Bytes).

	decode_publish_packet_identifier(0, Bytes, [], Bytes) :-
		!.
	decode_publish_packet_identifier(_QoS, Bytes, [packet_identifier(PacketIdentifier)], Rest) :-
		decode_packet_identifier(Bytes, PacketIdentifier, Rest).

	encode_publish_flags(Dup, QoS, Retain, Flags) :-
		encode_boolean_bit(Dup, DupBit),
		validate_qos(QoS),
		encode_boolean_bit(Retain, RetainBit),
		Flags is DupBit * 8 + QoS * 2 + RetainBit.

	decode_publish_flags(Flags, Dup, QoS, Retain) :-
		DupBit is Flags // 8,
		QoS is (Flags // 2) mod 4,
		RetainBit is Flags mod 2,
		decode_boolean_bit(DupBit, Dup),
		validate_qos(QoS),
		decode_boolean_bit(RetainBit, Retain).

	encode_boolean_bit(false, 0) :-
		!.
	encode_boolean_bit(true, 1) :-
		!.
	encode_boolean_bit(Boolean, _Bit) :-
		domain_error(boolean, Boolean).

	decode_boolean_bit(0, false) :-
		!.
	decode_boolean_bit(1, true) :-
		!.
	decode_boolean_bit(Bit, _Boolean) :-
		domain_error(boolean_bit, Bit).

	encode_subscribe_packet(Fields, Bytes) :-
		field_option(packet_identifier, Fields, PacketIdentifier, 0),
		field_option(properties, Fields, Properties, []),
		field_option(subscriptions, Fields, Subscriptions, []),
		encode_packet_identifier(PacketIdentifier, PacketIdentifierBytes),
		encode_properties(Properties, PropertyBytes),
		encode_subscriptions(Subscriptions, SubscriptionBytes),
		append(PacketIdentifierBytes, PropertyBytes, Bytes0),
		append(Bytes0, SubscriptionBytes, VariableHeaderAndPayload),
		length(VariableHeaderAndPayload, RemainingLength),
		encode_fixed_header(subscribe, 2, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeaderAndPayload, Bytes).

	decode_subscribe_packet_body(Body, packet(subscribe, [packet_identifier(PacketIdentifier), properties(Properties), subscriptions(Subscriptions)])) :-
		decode_packet_identifier(Body, PacketIdentifier, Rest0),
		decode_properties(Rest0, Properties, Rest1),
		decode_subscriptions(Rest1, Subscriptions).

	encode_subscriptions([], _Bytes) :-
		domain_error(mqtt_subscriptions, []).
	encode_subscriptions([Subscription| Subscriptions], Bytes) :-
		encode_subscription(Subscription, SubscriptionBytes),
		encode_subscriptions_tail(Subscriptions, TailBytes),
		append(SubscriptionBytes, TailBytes, Bytes).

	encode_subscriptions_tail([], []) :-
		!.
	encode_subscriptions_tail([Subscription| Subscriptions], Bytes) :-
		encode_subscription(Subscription, SubscriptionBytes),
		encode_subscriptions_tail(Subscriptions, TailBytes),
		append(SubscriptionBytes, TailBytes, Bytes).

	encode_subscription(subscription(TopicFilter, Options), Bytes) :-
		!,
		validate_topic_filter(TopicFilter),
		encode_utf8_string(TopicFilter, TopicFilterBytes),
		encode_subscription_options(Options, SubscriptionOptionsBytes),
		append(TopicFilterBytes, SubscriptionOptionsBytes, Bytes).
	encode_subscription(Subscription, _Bytes) :-
		domain_error(mqtt_subscription, Subscription).

	decode_subscriptions([], _Subscriptions) :-
		domain_error(mqtt_subscriptions, []).
	decode_subscriptions(Bytes, Subscriptions) :-
		decode_subscriptions(Bytes, [], ReversedSubscriptions),
		reverse(ReversedSubscriptions, Subscriptions).

	decode_subscriptions([], Subscriptions, Subscriptions) :-
		!.
	decode_subscriptions(Bytes, Subscriptions0, Subscriptions) :-
		decode_utf8_string(Bytes, TopicFilter, Rest0),
		validate_topic_filter(TopicFilter),
		decode_subscription_options(Rest0, Options, Rest1),
		decode_subscriptions(Rest1, [subscription(TopicFilter, Options)| Subscriptions0], Subscriptions).

	encode_subscription_options(Options, [Byte]) :-
		field_option(maximum_qos, Options, MaximumQoS, 0),
		field_option(no_local, Options, NoLocal, false),
		field_option(retain_as_published, Options, RetainAsPublished, false),
		field_option(retain_handling, Options, RetainHandling, send_at_subscribe),
		validate_qos(MaximumQoS),
		encode_boolean_bit(NoLocal, NoLocalBit),
		encode_boolean_bit(RetainAsPublished, RetainAsPublishedBit),
		retain_handling_code(RetainHandling, RetainHandlingCode),
		Byte is RetainHandlingCode * 16 + RetainAsPublishedBit * 8 + NoLocalBit * 4 + MaximumQoS.

	decode_subscription_options([Byte| Rest], [maximum_qos(MaximumQoS), no_local(NoLocal), retain_as_published(RetainAsPublished), retain_handling(RetainHandling)], Rest) :-
		validate_byte(Byte),
		MaximumQoS is Byte mod 4,
		NoLocalBit is (Byte // 4) mod 2,
		RetainAsPublishedBit is (Byte // 8) mod 2,
		RetainHandlingCode is (Byte // 16) mod 4,
		ReservedBits is Byte // 64,
		( 	ReservedBits =:= 0,
			MaximumQoS =\= 3,
			RetainHandlingCode =\= 3 ->
			decode_boolean_bit(NoLocalBit, NoLocal),
			decode_boolean_bit(RetainAsPublishedBit, RetainAsPublished),
			once(retain_handling_code(RetainHandling, RetainHandlingCode))
		; 	domain_error(mqtt_subscription_options, Byte)
		).

	retain_handling_code(send_at_subscribe, 0).
	retain_handling_code(send_at_new_subscription, 1).
	retain_handling_code(do_not_send, 2).

	encode_unsubscribe_packet(Fields, Bytes) :-
		field_option(packet_identifier, Fields, PacketIdentifier, 0),
		field_option(properties, Fields, Properties, []),
		field_option(topic_filters, Fields, TopicFilters, []),
		encode_packet_identifier(PacketIdentifier, PacketIdentifierBytes),
		encode_properties(Properties, PropertyBytes),
		encode_topic_filters(TopicFilters, TopicFilterBytes),
		append(PacketIdentifierBytes, PropertyBytes, Bytes0),
		append(Bytes0, TopicFilterBytes, VariableHeaderAndPayload),
		length(VariableHeaderAndPayload, RemainingLength),
		encode_fixed_header(unsubscribe, 2, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeaderAndPayload, Bytes).

	decode_unsubscribe_packet_body(Body, packet(unsubscribe, [packet_identifier(PacketIdentifier), properties(Properties), topic_filters(TopicFilters)])) :-
		decode_packet_identifier(Body, PacketIdentifier, Rest0),
		decode_properties(Rest0, Properties, Rest1),
		decode_topic_filters(Rest1, TopicFilters).

	encode_topic_filters([], _Bytes) :-
		domain_error(mqtt_topic_filters, []).
	encode_topic_filters([TopicFilter| TopicFilters], Bytes) :-
		validate_topic_filter(TopicFilter),
		encode_utf8_string(TopicFilter, TopicFilterBytes),
		encode_topic_filters_tail(TopicFilters, TailBytes),
		append(TopicFilterBytes, TailBytes, Bytes).

	encode_topic_filters_tail([], []) :-
		!.
	encode_topic_filters_tail([TopicFilter| TopicFilters], Bytes) :-
		validate_topic_filter(TopicFilter),
		encode_utf8_string(TopicFilter, TopicFilterBytes),
		encode_topic_filters_tail(TopicFilters, TailBytes),
		append(TopicFilterBytes, TailBytes, Bytes).

	decode_topic_filters([], _TopicFilters) :-
		domain_error(mqtt_topic_filters, []).
	decode_topic_filters(Bytes, TopicFilters) :-
		decode_topic_filters(Bytes, [], ReversedTopicFilters),
		reverse(ReversedTopicFilters, TopicFilters).

	decode_topic_filters([], TopicFilters, TopicFilters) :-
		!.
	decode_topic_filters(Bytes, TopicFilters0, TopicFilters) :-
		decode_utf8_string(Bytes, TopicFilter, Rest),
		validate_topic_filter(TopicFilter),
		decode_topic_filters(Rest, [TopicFilter| TopicFilters0], TopicFilters).

	ack_packet_type(puback).
	ack_packet_type(pubrec).
	ack_packet_type(pubrel).
	ack_packet_type(pubcomp).

	encode_ack_packet(Type, Fields, Bytes) :-
		field_option(packet_identifier, Fields, PacketIdentifier, 0),
		field_option(reason_code, Fields, ReasonCode, success),
		field_option(properties, Fields, Properties, []),
		encode_ack_variable_header(Type, PacketIdentifier, ReasonCode, Properties, VariableHeader),
		length(VariableHeader, RemainingLength),
		ack_packet_flags(Type, Flags),
		encode_fixed_header(Type, Flags, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeader, Bytes).

	encode_ack_variable_header(_Type, PacketIdentifier, success, [], PacketIdentifierBytes) :-
		!,
		encode_packet_identifier(PacketIdentifier, PacketIdentifierBytes).
	encode_ack_variable_header(Type, PacketIdentifier, ReasonCode, Properties, VariableHeader) :-
		encode_packet_identifier(PacketIdentifier, PacketIdentifierBytes),
		encode_reason_code(Type, ReasonCode, ReasonCodeBytes),
		encode_properties(Properties, PropertyBytes),
		append(PacketIdentifierBytes, ReasonCodeBytes, Bytes0),
		append(Bytes0, PropertyBytes, VariableHeader).

	decode_ack_packet_body(Type, Body, packet(Type, [packet_identifier(PacketIdentifier), reason_code(ReasonCode), properties(Properties)])) :-
		decode_packet_identifier(Body, PacketIdentifier, Rest0),
		( 	Rest0 == [] ->
			ReasonCode = success,
			Properties = []
		; 	decode_reason_code(Type, Rest0, ReasonCode, Rest1),
			( 	Rest1 == [] ->
				Properties = []
			; 	decode_properties(Rest1, Properties, [])
			)
		).

	ack_packet_flags(puback, 0).
	ack_packet_flags(pubrec, 0).
	ack_packet_flags(pubrel, 2).
	ack_packet_flags(pubcomp, 0).

	encode_packet_identifier(PacketIdentifier, Bytes) :-
		validate_packet_identifier(PacketIdentifier),
		encode_uint16(PacketIdentifier, Bytes).

	decode_packet_identifier(Bytes, PacketIdentifier, Rest) :-
		decode_uint16(Bytes, PacketIdentifier, Rest),
		validate_packet_identifier(PacketIdentifier).

	subscription_ack_packet_type(suback).
	subscription_ack_packet_type(unsuback).

	encode_subscription_ack_packet(Type, Fields, Bytes) :-
		field_option(packet_identifier, Fields, PacketIdentifier, 0),
		field_option(properties, Fields, Properties, []),
		field_option(reason_codes, Fields, ReasonCodes, []),
		encode_packet_identifier(PacketIdentifier, PacketIdentifierBytes),
		encode_properties(Properties, PropertyBytes),
		encode_reason_code_list(Type, ReasonCodes, ReasonCodeBytes),
		append(PacketIdentifierBytes, PropertyBytes, Bytes0),
		append(Bytes0, ReasonCodeBytes, VariableHeaderAndPayload),
		length(VariableHeaderAndPayload, RemainingLength),
		encode_fixed_header(Type, 0, RemainingLength, FixedHeaderBytes),
		append(FixedHeaderBytes, VariableHeaderAndPayload, Bytes).

	decode_subscription_ack_packet_body(Type, Body, packet(Type, [packet_identifier(PacketIdentifier), properties(Properties), reason_codes(ReasonCodes)])) :-
		decode_packet_identifier(Body, PacketIdentifier, Rest0),
		decode_properties(Rest0, Properties, Rest1),
		decode_reason_code_list(Type, Rest1, ReasonCodes).

	encode_reason_code_list(_Type, [], _Bytes) :-
		domain_error(mqtt_reason_code_list, []).
	encode_reason_code_list(Type, [ReasonCode| ReasonCodes], Bytes) :-
		encode_reason_code(Type, ReasonCode, ReasonCodeBytes),
		encode_reason_code_list_tail(Type, ReasonCodes, TailBytes),
		append(ReasonCodeBytes, TailBytes, Bytes).

	encode_reason_code_list_tail(_Type, [], []) :-
		!.
	encode_reason_code_list_tail(Type, [ReasonCode| ReasonCodes], Bytes) :-
		encode_reason_code(Type, ReasonCode, ReasonCodeBytes),
		encode_reason_code_list_tail(Type, ReasonCodes, TailBytes),
		append(ReasonCodeBytes, TailBytes, Bytes).

	decode_reason_code_list(_Type, [], _ReasonCodes) :-
		domain_error(mqtt_reason_code_list, []).
	decode_reason_code_list(Type, Bytes, ReasonCodes) :-
		decode_reason_code_list(Type, Bytes, [], ReversedReasonCodes),
		reverse(ReversedReasonCodes, ReasonCodes).

	decode_reason_code_list(_Type, [], ReasonCodes, ReasonCodes) :-
		!.
	decode_reason_code_list(Type, Bytes, ReasonCodes0, ReasonCodes) :-
		decode_reason_code(Type, Bytes, ReasonCode, Rest),
		decode_reason_code_list(Type, Rest, [ReasonCode| ReasonCodes0], ReasonCodes).

	parse_operation_options(Options, PacketIdentifier, Properties, MergedOptions) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(packet_identifier(PacketIdentifier), MergedOptions),
		^^option(properties(Properties), MergedOptions).

	wait_for_publish_acknowledgement(0, _PacketIdentifier, _Input, _Output) :-
		!.
	wait_for_publish_acknowledgement(1, PacketIdentifier, Input, _Output) :-
		!,
		read_packet(Input, Packet),
		accept_publish_acknowledgement(puback, PacketIdentifier, Packet).
	wait_for_publish_acknowledgement(2, PacketIdentifier, Input, Output) :-
		!,
		read_packet(Input, PubrecPacket),
		accept_publish_acknowledgement(pubrec, PacketIdentifier, PubrecPacket),
		write_packet(Output, packet(pubrel, [packet_identifier(PacketIdentifier)])),
		read_packet(Input, PubcompPacket),
		accept_publish_acknowledgement(pubcomp, PacketIdentifier, PubcompPacket).

	accept_publish_acknowledgement(Type, PacketIdentifier, packet(Type, Fields)) :-
		member(packet_identifier(PacketIdentifier), Fields),
		member(reason_code(ReasonCode), Fields),
		!,
		accept_reason_code(Type, ReasonCode).
	accept_publish_acknowledgement(Type, PacketIdentifier, Packet) :-
		domain_error(mqtt_packet, expected(Type-PacketIdentifier, Packet)).

	accept_reason_code(Type, ReasonCode) :-
		reason_code(Type, ReasonCode, Code),
		!,
		( 	Code < 128 ->
			true
		; 	domain_error(mqtt_reason_code, Type-ReasonCode)
		).
	accept_reason_code(Type, ReasonCode) :-
		domain_error(mqtt_reason_code, Type-ReasonCode).

	default_subscription_options(Options, [maximum_qos(MaximumQoS), no_local(NoLocal), retain_as_published(RetainAsPublished), retain_handling(RetainHandling)]) :-
		^^option(maximum_qos(MaximumQoS), Options),
		^^option(no_local(NoLocal), Options),
		^^option(retain_as_published(RetainAsPublished), Options),
		^^option(retain_handling(RetainHandling), Options).

	build_subscriptions([], _SubscriptionOptions, _Subscriptions) :-
		!,
		domain_error(mqtt_subscriptions, []).
	build_subscriptions([TopicFilter| TopicFilters], SubscriptionOptions, Subscriptions) :-
		build_subscriptions([TopicFilter| TopicFilters], SubscriptionOptions, [], ReversedSubscriptions),
		reverse(ReversedSubscriptions, Subscriptions).

	build_subscriptions([], _SubscriptionOptions, Subscriptions, Subscriptions) :-
		!.
	build_subscriptions([subscription(TopicFilter, Options)| TopicFilters], SubscriptionOptions, Subscriptions0, Subscriptions) :-
		!,
		build_subscriptions(TopicFilters, SubscriptionOptions, [subscription(TopicFilter, Options)| Subscriptions0], Subscriptions).
	build_subscriptions([TopicFilter| TopicFilters], SubscriptionOptions, Subscriptions0, Subscriptions) :-
		build_subscriptions(TopicFilters, SubscriptionOptions, [subscription(TopicFilter, SubscriptionOptions)| Subscriptions0], Subscriptions).

	accept_subscription_acknowledgement(Type, PacketIdentifier, packet(Type, Fields), Result) :-
		member(packet_identifier(PacketIdentifier), Fields),
		member(reason_codes(Result), Fields),
		!.
	accept_subscription_acknowledgement(Type, PacketIdentifier, Packet, _Result) :-
		domain_error(mqtt_packet, expected(Type-PacketIdentifier, Packet)).

	write_packet(Output, Packet) :-
		encode_packet(Packet, Bytes),
		write_bytes(Bytes, Output),
		flush_output(Output).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	read_packet(Input, Packet) :-
		get_byte(Input, HeaderByte),
		( 	HeaderByte =:= -1 ->
			domain_error(mqtt_packet_stream, unexpected_end_of_file)
		; 	validate_byte(HeaderByte),
			read_remaining_length_bytes(Input, RemainingLengthBytes),
			decode_varint(RemainingLengthBytes, RemainingLength, []),
			read_exact_bytes(Input, RemainingLength, Body),
			append([HeaderByte| RemainingLengthBytes], Body, Bytes),
			decode_packet(Bytes, Packet)
		).

	read_remaining_length_bytes(Input, Bytes) :-
		read_remaining_length_bytes(Input, 0, [], Bytes).

	read_remaining_length_bytes(Input, Count0, Bytes0, Bytes) :-
		get_byte(Input, Byte),
		( 	Byte =:= -1 ->
			domain_error(mqtt_packet_stream, unexpected_end_of_file)
		; 	validate_byte(Byte),
			Count is Count0 + 1,
			Bytes1 = [Byte| Bytes0],
			( 	Byte < 128 ->
				reverse(Bytes1, Bytes)
			; 	Count =:= 4 ->
				reverse(Bytes1, Consumed),
				domain_error(mqtt_variable_byte_integer, Consumed)
			; 	read_remaining_length_bytes(Input, Count, Bytes1, Bytes)
			)
		).

	read_exact_bytes(Input, Length, Bytes) :-
		read_exact_bytes(Input, Length, Bytes, []).

	read_exact_bytes(_Input, 0, Bytes, Bytes) :-
		!.
	read_exact_bytes(Input, Length, [Byte| Bytes0], Bytes) :-
		Length > 0,
		get_byte(Input, Byte),
		( 	Byte =:= -1 ->
			domain_error(mqtt_packet_stream, unexpected_end_of_file)
		; 	NextLength is Length - 1,
			read_exact_bytes(Input, NextLength, Bytes0, Bytes)
		).

	field_option(Name, Fields, Value, Default) :-
		functor(Template, Name, 1),
		( 	member(Template, Fields) ->
			arg(1, Template, Value)
		; 	Value = Default
		).

	encode_connect_flags(true, 2) :-
		!.
	encode_connect_flags(false, 0) :-
		!.
	encode_connect_flags(CleanStart, _Flags) :-
		domain_error(boolean, CleanStart).

	decode_connect_flags(0, false) :-
		!.
	decode_connect_flags(2, true) :-
		!.
	decode_connect_flags(Flags, _CleanStart) :-
		domain_error(mqtt_connect_flags, Flags).

	encode_connack_flags(true, 1) :-
		!.
	encode_connack_flags(false, 0) :-
		!.
	encode_connack_flags(SessionPresent, _Flags) :-
		domain_error(boolean, SessionPresent).

	decode_connack_flags(0, false) :-
		!.
	decode_connack_flags(1, true) :-
		!.
	decode_connack_flags(Flags, _SessionPresent) :-
		domain_error(mqtt_connack_flags, Flags).

	encode_reason_code(PacketType, ReasonCode, [Code]) :-
		( 	reason_code(PacketType, ReasonCode, Code) ->
			true
		; 	domain_error(mqtt_reason_code, PacketType-ReasonCode)
		).

	decode_reason_code(PacketType, Bytes, ReasonCode, Rest) :-
		decode_uint8(Bytes, Code, Rest),
		( 	reason_code(PacketType, ReasonCode, Code) ->
			true
		; 	domain_error(mqtt_reason_code, PacketType-Code)
		).

	encode_properties(Properties, Bytes) :-
		( 	var(Properties) ->
			instantiation_error
		; 	\+ proper_list(Properties) ->
			type_error(list, Properties)
		; 	encode_property_list(Properties, PropertyBytes),
			length(PropertyBytes, PropertyLength),
			encode_varint(PropertyLength, PropertyLengthBytes),
			append(PropertyLengthBytes, PropertyBytes, Bytes)
		).

	decode_properties(Bytes, Properties, Rest) :-
		decode_varint(Bytes, PropertyLength, Rest0),
		( 	take(PropertyLength, Rest0, PropertyBytes, Rest) ->
			decode_property_list(PropertyBytes, Properties)
		; 	domain_error(mqtt_properties, Bytes)
		).

	encode_property_list([], []) :-
		!.
	encode_property_list([Property| Properties], Bytes) :-
		encode_property(Property, PropertyBytes),
		encode_property_list(Properties, TailBytes),
		append(PropertyBytes, TailBytes, Bytes).

	decode_property_list([], []) :-
		!.
	decode_property_list(Bytes, [Property| Properties]) :-
		decode_property(Bytes, Property, Rest),
		decode_property_list(Rest, Properties).

	encode_property(Property, Bytes) :-
		compound(Property),
		functor(Property, Name, 1),
		mqtt_property(Name, Identifier, Type),
		!,
		arg(1, Property, Value),
		encode_uint8(Identifier, IdentifierBytes),
		encode_property_value(Name, Type, Value, ValueBytes),
		append(IdentifierBytes, ValueBytes, Bytes).
	encode_property(Property, _Bytes) :-
		domain_error(mqtt_property, Property).

	decode_property(Bytes, Property, Rest) :-
		decode_uint8(Bytes, Identifier, Rest0),
		( 	mqtt_property(Name, Identifier, Type) ->
			!,
			decode_property_value(Name, Type, Rest0, Value, Rest),
			functor(Property, Name, 1),
			arg(1, Property, Value)
		; 	domain_error(mqtt_property_identifier, Identifier)
		).

	encode_property_value(_Name, uint16, Value, Bytes) :-
		!,
		encode_uint16(Value, Bytes).
	encode_property_value(_Name, uint32, Value, Bytes) :-
		!,
		encode_uint32(Value, Bytes).
	encode_property_value(_Name, utf8_string, Value, Bytes) :-
		!,
		encode_utf8_string(Value, Bytes).
	encode_property_value(_Name, binary_data, Value, Bytes) :-
		!,
		encode_binary_data(Value, Bytes).
	encode_property_value(_Name, utf8_string_pair, Value, Bytes) :-
		!,
		encode_utf8_string_pair(Value, Bytes).
	encode_property_value(_Name, boolean, Value, Bytes) :-
		!,
		encode_boolean_bit(Value, Code),
		encode_uint8(Code, Bytes).
	encode_property_value(Name, byte_0_1, Value, Bytes) :-
		!,
		validate_property_integer(Name, Value, 0, 1),
		encode_uint8(Value, Bytes).
	encode_property_value(Name, positive_uint16, Value, Bytes) :-
		!,
		validate_property_integer(Name, Value, 1, 65535),
		encode_uint16(Value, Bytes).
	encode_property_value(Name, positive_uint32, Value, Bytes) :-
		!,
		validate_property_integer(Name, Value, 1, 4294967295),
		encode_uint32(Value, Bytes).
	encode_property_value(Name, positive_varint, Value, Bytes) :-
		!,
		validate_property_integer(Name, Value, 1, 268435455),
		encode_varint(Value, Bytes).
	encode_property_value(_Name, topic_name, Value, Bytes) :-
		!,
		validate_topic_name(Value),
		encode_utf8_string(Value, Bytes).

	decode_property_value(_Name, uint16, Bytes, Value, Rest) :-
		!,
		decode_uint16(Bytes, Value, Rest).
	decode_property_value(_Name, uint32, Bytes, Value, Rest) :-
		!,
		decode_uint32(Bytes, Value, Rest).
	decode_property_value(_Name, utf8_string, Bytes, Value, Rest) :-
		!,
		decode_utf8_string(Bytes, Value, Rest).
	decode_property_value(_Name, binary_data, Bytes, Value, Rest) :-
		!,
		decode_binary_data(Bytes, Value, Rest).
	decode_property_value(_Name, utf8_string_pair, Bytes, Value, Rest) :-
		!,
		decode_utf8_string_pair(Bytes, Value, Rest).
	decode_property_value(_Name, boolean, Bytes, Value, Rest) :-
		!,
		decode_uint8(Bytes, Code, Rest),
		decode_boolean_bit(Code, Value).
	decode_property_value(Name, byte_0_1, Bytes, Value, Rest) :-
		!,
		decode_uint8(Bytes, Value, Rest),
		validate_property_integer(Name, Value, 0, 1).
	decode_property_value(Name, positive_uint16, Bytes, Value, Rest) :-
		!,
		decode_uint16(Bytes, Value, Rest),
		validate_property_integer(Name, Value, 1, 65535).
	decode_property_value(Name, positive_uint32, Bytes, Value, Rest) :-
		!,
		decode_uint32(Bytes, Value, Rest),
		validate_property_integer(Name, Value, 1, 4294967295).
	decode_property_value(Name, positive_varint, Bytes, Value, Rest) :-
		!,
		decode_varint(Bytes, Value, Rest),
		validate_property_integer(Name, Value, 1, 268435455).
	decode_property_value(_Name, topic_name, Bytes, Value, Rest) :-
		!,
		decode_utf8_string(Bytes, Value, Rest),
		validate_topic_name(Value).

	validate_property_integer(Name, Value, Minimum, Maximum) :-
		( 	var(Value) ->
			instantiation_error
		; 	\+ integer(Value) ->
			type_error(integer, Value)
		; 	Value < Minimum ->
			domain_error(mqtt_property_value, Name-Value)
		; 	Value > Maximum ->
			domain_error(mqtt_property_value, Name-Value)
		; 	true
		).

	encode_uint8(Value, [Value]) :-
		validate_unsigned_integer(Value, 255).

	decode_uint8([Byte| Rest], Byte, Rest) :-
		validate_byte(Byte).

	encode_uint16(Value, [Hi, Lo]) :-
		validate_unsigned_integer(Value, 65535),
		Hi is Value // 256,
		Lo is Value mod 256.

	decode_uint16([Hi, Lo| Rest], Value, Rest) :-
		validate_byte(Hi),
		validate_byte(Lo),
		Value is Hi * 256 + Lo.

	encode_uint32(Value, [B3, B2, B1, B0]) :-
		validate_unsigned_integer(Value, 4294967295),
		B3 is Value // 16777216,
		R3 is Value mod 16777216,
		B2 is R3 // 65536,
		R2 is R3 mod 65536,
		B1 is R2 // 256,
		B0 is R2 mod 256.

	decode_uint32([B3, B2, B1, B0| Rest], Value, Rest) :-
		validate_byte(B3),
		validate_byte(B2),
		validate_byte(B1),
		validate_byte(B0),
		Value is B3 * 16777216 + B2 * 65536 + B1 * 256 + B0.

	encode_varint(Value, Bytes) :-
		validate_unsigned_integer(Value, 268435455),
		encode_varint_bytes(Value, Bytes).

	decode_varint(Bytes, Value, Rest) :-
		decode_varint_bytes(Bytes, 1, 0, 0, Value, Rest, [], ReversedConsumed),
		reverse(ReversedConsumed, Consumed),
		encode_varint(Value, MinimalConsumed),
		( 	Consumed == MinimalConsumed ->
			true
		; 	domain_error(mqtt_variable_byte_integer, Consumed)
		).

	encode_varint_bytes(Value, [Encoded]) :-
		Value < 128,
		!,
		Encoded is Value.
	encode_varint_bytes(Value, [Encoded| Bytes]) :-
		Encoded is Value mod 128 + 128,
		Next is Value // 128,
		encode_varint_bytes(Next, Bytes).

	decode_varint_bytes([Byte| Rest], Multiplier, Value0, Count0, Value, RestOut, Consumed0, Consumed) :-
		validate_byte(Byte),
		Count is Count0 + 1,
		( 	Count =< 4 ->
			Digit is Byte mod 128,
			Value1 is Value0 + Digit * Multiplier,
			Consumed1 = [Byte| Consumed0],
			( 	Byte < 128 ->
				Value = Value1,
				RestOut = Rest,
				Consumed = Consumed1
			; 	Count =:= 4 ->
				domain_error(mqtt_variable_byte_integer, Consumed1)
			; 	NextMultiplier is Multiplier * 128,
				decode_varint_bytes(Rest, NextMultiplier, Value1, Count, Value, RestOut, Consumed1, Consumed)
			)
		; 	domain_error(mqtt_variable_byte_integer, [Byte| Rest])
		).

	encode_utf8_string(String, Bytes) :-
		validate_utf8_atom(String, Codes),
		length(Codes, Length),
		encode_uint16(Length, LengthBytes),
		append(LengthBytes, Codes, Bytes).

	decode_utf8_string(Bytes, String, Rest) :-
		decode_uint16(Bytes, Length, AfterLength),
		take(Length, AfterLength, Codes, Rest),
		validate_utf8_codes(Codes),
		atom_codes(String, Codes).

	encode_binary_data(Data, Bytes) :-
		validate_byte_list(Data),
		length(Data, Length),
		encode_uint16(Length, LengthBytes),
		append(LengthBytes, Data, Bytes).

	decode_binary_data(Bytes, Data, Rest) :-
		decode_uint16(Bytes, Length, AfterLength),
		take(Length, AfterLength, Data, Rest).

	encode_utf8_string_pair(Name-Value, Bytes) :-
		encode_utf8_string(Name, NameBytes),
		encode_utf8_string(Value, ValueBytes),
		append(NameBytes, ValueBytes, Bytes).

	decode_utf8_string_pair(Bytes, Name-Value, Rest) :-
		decode_utf8_string(Bytes, Name, AfterName),
		decode_utf8_string(AfterName, Value, Rest).

	encode_fixed_header(Type, Flags, RemainingLength, Bytes) :-
		( 	packet_type(Type, Code) ->
			true
		; 	domain_error(mqtt_packet_type, Type)
		),
		validate_fixed_header_flags(Type, Flags),
		encode_varint(RemainingLength, RemainingLengthBytes),
		HeaderByte is Code * 16 + Flags,
		Bytes = [HeaderByte| RemainingLengthBytes].

	decode_fixed_header([HeaderByte| Bytes], Type, Flags, RemainingLength, Rest) :-
		validate_byte(HeaderByte),
		Code is HeaderByte // 16,
		Flags is HeaderByte mod 16,
		( 	packet_type(Type, Code) ->
			true
		; 	domain_error(mqtt_packet_type, Code)
		),
		validate_fixed_header_flags(Type, Flags),
		decode_varint(Bytes, RemainingLength, Rest).

	validate_fixed_header_flags(publish, Flags) :-
		!,
		validate_unsigned_integer(Flags, 15),
		QoS is (Flags // 2) mod 4,
		( 	QoS =:= 3 ->
			domain_error(mqtt_fixed_header_flags, publish-Flags)
		; 	true
		).
	validate_fixed_header_flags(pubrel, Flags) :-
		!,
		validate_reserved_fixed_header_flags(pubrel, Flags, 2).
	validate_fixed_header_flags(subscribe, Flags) :-
		!,
		validate_reserved_fixed_header_flags(subscribe, Flags, 2).
	validate_fixed_header_flags(unsubscribe, Flags) :-
		!,
		validate_reserved_fixed_header_flags(unsubscribe, Flags, 2).
	validate_fixed_header_flags(Type, Flags) :-
		validate_reserved_fixed_header_flags(Type, Flags, 0).

	validate_reserved_fixed_header_flags(Type, Flags, ExpectedFlags) :-
		validate_unsigned_integer(Flags, 15),
		( 	Flags =:= ExpectedFlags ->
			true
		; 	domain_error(mqtt_fixed_header_flags, Type-Flags)
		).

	validate_utf8_atom(String, Codes) :-
		( 	var(String) ->
			instantiation_error
		; 	\+ atom(String) ->
			type_error(atom, String)
		; 	atom_codes(String, Codes),
			validate_utf8_codes(Codes)
		).

	validate_utf8_codes(Codes) :-
		( 	member(0, Codes) ->
			domain_error(mqtt_utf8_string, Codes)
		; 	validate_byte_list(Codes)
		).

	validate_byte_list(Bytes) :-
		valid(list(byte), Bytes).

	validate_byte(Byte) :-
		valid(byte, Byte).

	validate_unsigned_integer(Value, Maximum) :-
		( 	var(Value) ->
			instantiation_error
		; 	\+ integer(Value) ->
			type_error(integer, Value)
		; 	Value < 0 ->
			domain_error(non_negative_integer, Value)
		; 	Value > Maximum ->
			domain_error(between(0, Maximum), Value)
		; 	true
		).

	validate_packet_identifier(PacketIdentifier) :-
		( 	var(PacketIdentifier) ->
			instantiation_error
		; 	\+ integer(PacketIdentifier) ->
			type_error(integer, PacketIdentifier)
		; 	PacketIdentifier < 1 ->
			domain_error(mqtt_packet_identifier, PacketIdentifier)
		; 	PacketIdentifier > 65535 ->
			domain_error(mqtt_packet_identifier, PacketIdentifier)
		; 	true
		).

	validate_qos(QoS) :-
		( 	var(QoS) ->
			instantiation_error
		; 	\+ integer(QoS) ->
			type_error(integer, QoS)
		; 	QoS < 0 ->
			domain_error(mqtt_qos, QoS)
		; 	QoS > 2 ->
			domain_error(mqtt_qos, QoS)
		; 	true
		).

	validate_topic_name(TopicName) :-
		validate_utf8_atom(TopicName, Codes),
		( 	Codes == [] ->
			domain_error(mqtt_topic_name, TopicName)
		; 	( member(0'+, Codes); member(0'#, Codes) ) ->
			domain_error(mqtt_topic_name, TopicName)
		; 	true
		).

	validate_topic_filter(TopicFilter) :-
		validate_utf8_atom(TopicFilter, Codes),
		( 	Codes == [] ->
			domain_error(mqtt_topic_filter, TopicFilter)
		; 	validate_topic_filter_codes(Codes, true, TopicFilter)
		).

	validate_topic_filter_codes([], _PreviousSeparator, _TopicFilter).
	validate_topic_filter_codes([0'#| Rest], PreviousSeparator, TopicFilter) :-
		!,
		( 	PreviousSeparator == true,
			Rest == [] ->
			true
		; 	domain_error(mqtt_topic_filter, TopicFilter)
		).
	validate_topic_filter_codes([0'+| Rest], PreviousSeparator, TopicFilter) :-
		!,
		( 	PreviousSeparator == true,
			( Rest == [] ; Rest = [0'/| _] ) ->
			validate_topic_filter_codes(Rest, false, TopicFilter)
		; 	domain_error(mqtt_topic_filter, TopicFilter)
		).
	validate_topic_filter_codes([Code| Rest], _PreviousSeparator, TopicFilter) :-
		( 	Code =:= 0'/ ->
			NextPreviousSeparator = true
		; 	NextPreviousSeparator = false
		),
		validate_topic_filter_codes(Rest, NextPreviousSeparator, TopicFilter).

	parse_connection_options(Options, Transport, ConnectionOptions, Scheme, ConnectFields) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(transport(Transport), MergedOptions),
		^^option(connection_options(ConnectionOptions), MergedOptions),
		^^option(scheme(Scheme), MergedOptions),
		^^option(client_id(ClientId), MergedOptions),
		^^option(clean_start(CleanStart), MergedOptions),
		^^option(keep_alive(KeepAlive), MergedOptions),
		^^option(properties(Properties), MergedOptions),
		ConnectFields = [client_id(ClientId), clean_start(CleanStart), keep_alive(KeepAlive), properties(Properties)].

	resolve_address(Address, Scheme, Host, Port) :-
		( 	var(Address) ->
			instantiation_error
		; 	atom(Address) ->
			mqtt_address_components(Address, Scheme, Components),
			address_endpoint(Scheme, Components, Host, Port)
		; 	type_error(atom, Address)
		).

	mqtt_address_components(Address, Scheme, Components) :-
		( 	atom_concat('mqtt://', Rest, Address) ->
			Scheme = mqtt,
			atom_concat('http://', Rest, ContextAddress)
		; 	atom_concat('mqtts://', Rest, Address) ->
			Scheme = mqtts,
			atom_concat('https://', Rest, ContextAddress)
		; 	domain_error(mqtt_address, Address)
		),
		( 	url(atom)::parse(ContextAddress, Components) ->
			true
		; 	domain_error(mqtt_address, Address)
		).

	address_endpoint(Scheme, Components, Host, Port) :-
		( 	member(authority(Authority), Components) ->
			mqtt_authority_endpoint(Scheme, Authority, Host, Port)
		; 	domain_error(mqtt_address, Components)
		).

	mqtt_authority_endpoint(Scheme, Authority, Host, Port) :-
		mqtt_context_scheme(Scheme, ContextScheme),
		atomic_list_concat([ContextScheme, '://', Authority, '/'], URL),
		( 	^^absolute_url_context(URL, http_url_context(ContextScheme, Host, ParsedPort, '/')) ->
			( 	authority_explicit_port(Authority) ->
				Port = ParsedPort
			; 	default_mqtt_port(Scheme, Port)
			)
		; 	domain_error(mqtt_address_authority, Authority)
		).

	authority_explicit_port(Authority) :-
		sub_atom(Authority, _Before, 1, After, ':'),
		After > 0,
		sub_atom(Authority, _, After, 0, PortAtom),
		PortAtom \== '',
		atom_codes(PortAtom, PortCodes),
		catch(number_codes(Port, PortCodes), _, fail),
		integer(Port).

	mqtt_context_scheme(mqtt, http).
	mqtt_context_scheme(mqtts, https).

	default_mqtt_port(mqtt, 1883).
	default_mqtt_port(mqtts, 8883).

	establish_mqtt_connection(Scheme, Host, Port, Transport0, ConnectionOptions0, ConnectFields, Connection) :-
		open_mqtt_connection(Scheme, Host, Port, Transport0, ConnectionOptions0, Connection),
		catch(
			connect_mqtt_session(Connection, ConnectFields),
			Error,
			( 	close_mqtt_connection(Connection),
				throw(Error)
			)
		).

	connect_mqtt_session(mqtt_connection(_Transport, _RawConnection, Input, Output, _Host, _Port, _Scheme), ConnectFields) :-
		write_packet(Output, packet(connect, ConnectFields)),
		read_packet(Input, Packet),
		accept_connack(Packet).

	accept_connack(packet(connack, Fields)) :-
		member(reason_code(success), Fields),
		!.
	accept_connack(packet(connack, Fields)) :-
		member(reason_code(ReasonCode), Fields),
		!,
		domain_error(mqtt_connack_reason_code, ReasonCode).
	accept_connack(Packet) :-
		domain_error(mqtt_packet, expected(connack, Packet)).

	close_mqtt_connection(mqtt_connection(Transport, RawConnection, _Input, _Output, _Host, _Port, _Scheme)) :-
		catch(Transport::close_connection(RawConnection), _, true).

	open_mqtt_connection(Scheme, Host, Port, Transport0, ConnectionOptions0, Connection) :-
		resolve_transport(Scheme, Transport0, Transport),
		append_tls_transport(Scheme, ConnectionOptions0, ConnectionOptions),
		Transport::open_connection(Host, Port, RawConnection, ConnectionOptions),
		catch(
			Transport::connection_streams(RawConnection, Input, Output),
			Error,
			( 	catch(Transport::close_connection(RawConnection), _, true),
				throw(Error)
			)
		),
		Connection = mqtt_connection(Transport, RawConnection, Input, Output, Host, Port, Scheme).

	resolve_transport(Scheme, default, Transport) :-
		!,
		default_transport(Scheme, Transport).
	resolve_transport(Scheme, Transport, Transport) :-
		validate_transport(Transport),
		( 	transport_supports_mqtt_scheme(Transport, Scheme) ->
			true
		; 	consistency_error(mqtt_options, scheme(Scheme), transport(Transport))
		).

	default_transport(mqtt, http_socket_transport).
	default_transport(mqtts, http_process_transport).

	transport_supports_mqtt_scheme(Transport, Scheme) :-
		mqtt_context_scheme(Scheme, ContextScheme),
		Transport::supported_request_scheme(ContextScheme).

	validate_transport(Transport) :-
		( 	var(Transport) ->
			instantiation_error
		; 	current_object(Transport) ->
			( 	conforms_to_protocol(Transport, http_transport_protocol) ->
				true
			; 	domain_error(http_transport_protocol_object, Transport)
			)
		; 	existence_error(object, Transport)
		).

	append_tls_transport(mqtts, Options, OptionsWithTransport) :-
		!,
		append_tls_transport(Options, OptionsWithTransport).
	append_tls_transport(_Scheme, Options, Options).

	append_tls_transport(Options, Options) :-
		member(connection_transport(_), Options),
		!.
	append_tls_transport(Options, [connection_transport(tls)| Options]).

	validate_endpoint(Host, Port) :-
		( 	var(Host) ->
			instantiation_error
		; 	\+ atom(Host) ->
			type_error(atom, Host)
		; 	var(Port) ->
			instantiation_error
		; 	\+ integer(Port) ->
			type_error(integer, Port)
		; 	Port < 0 ->
			domain_error(non_negative_integer, Port)
		; 	true
		).

	validate_mqtt_scheme(Scheme) :-
		( 	var(Scheme) ->
			instantiation_error
		;	mqtt_context_scheme(Scheme, _ContextScheme) ->
			true
		; 	domain_error(mqtt_scheme, Scheme)
		).

	pending_packet_support :-
		resource_error(mqtt_packet_support).

	valid_option(transport(Transport)) :-
		( 	Transport == default ->
			true
		; 	validate_transport(Transport)
		).
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions).
	valid_option(scheme(Scheme)) :-
		validate_mqtt_scheme(Scheme).
	valid_option(timeout(Timeout)) :-
		integer(Timeout),
		Timeout >= 0.
	valid_option(client_id(ClientId)) :-
		validate_utf8_atom(ClientId, _Codes).
	valid_option(clean_start(CleanStart)) :-
		ground(CleanStart),
		encode_connect_flags(CleanStart, _Flags).
	valid_option(keep_alive(KeepAlive)) :-
		validate_unsigned_integer(KeepAlive, 65535).
	valid_option(properties(Properties)) :-
		ground(Properties),
		encode_properties(Properties, _Bytes).
	valid_option(packet_identifier(PacketIdentifier)) :-
		validate_packet_identifier(PacketIdentifier).
	valid_option(qos(QoS)) :-
		validate_qos(QoS).
	valid_option(dup(Dup)) :-
		ground(Dup),
		encode_boolean_bit(Dup, _Bit).
	valid_option(retain(Retain)) :-
		ground(Retain),
		encode_boolean_bit(Retain, _Bit).
	valid_option(maximum_qos(MaximumQoS)) :-
		validate_qos(MaximumQoS).
	valid_option(no_local(NoLocal)) :-
		ground(NoLocal),
		encode_boolean_bit(NoLocal, _Bit).
	valid_option(retain_as_published(RetainAsPublished)) :-
		ground(RetainAsPublished),
		encode_boolean_bit(RetainAsPublished, _Bit).
	valid_option(retain_handling(RetainHandling)) :-
		ground(RetainHandling),
		retain_handling_code(RetainHandling, _Code).

	default_option(transport(default)).
	default_option(connection_options([])).
	default_option(scheme(mqtt)).
	default_option(timeout(0)).
	default_option(client_id('')).
	default_option(clean_start(true)).
	default_option(keep_alive(60)).
	default_option(properties([])).
	default_option(packet_identifier(1)).
	default_option(qos(0)).
	default_option(dup(false)).
	default_option(retain(false)).
	default_option(maximum_qos(0)).
	default_option(no_local(false)).
	default_option(retain_as_published(false)).
	default_option(retain_handling(send_at_subscribe)).

:- end_object.
