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


:- object(amqp).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-11,
		comment is 'Portable AMQP 0-9-1 (Advanced Message Queuing Protocol) client. Uses the sockets library for TCP communication.',
		remarks is [
			'Supported backends' - 'ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM (same as the sockets library).',
			'Protocol version' - 'Implements AMQP 0-9-1 specification.',
			'Binary protocol' - 'AMQP is a binary protocol with typed frame encoding.',
			'Channels' - 'Supports multiple concurrent channels over a single connection.',
			'Exchanges and queues' - 'Full support for declaring exchanges, queues, and bindings.',
			'Content' - 'Supports message publishing and consuming with content headers.',
			'Transactions' - 'Supports AMQP transactions with tx.select, tx.commit, and tx.rollback.',
			'Publisher confirms' - 'Support for publisher confirms can be added.',
			'Heartbeat' - 'Supports heartbeat negotiation to keep connections alive.',
			'Reconnection' - 'Automatic reconnection with configurable retry attempts and delays.'
		]
	]).

	% ==========================================================================
	% Public API - Connection Management
	% ==========================================================================

	:- public(connect/4).
	:- mode(connect(+atom, +integer, --compound, +list), one_or_error).
	:- info(connect/4, [
		comment is 'Connects to an AMQP 0-9-1 server and performs the protocol handshake. Returns a connection handle for subsequent operations. Supports automatic reconnection on connection failures.',
		argnames is ['Host', 'Port', 'Connection', 'Options'],
		remarks is [
			'Option username(Username)' - 'Username for authentication. Default is guest.',
			'Option password(Password)' - 'Password for authentication. Default is guest.',
			'Option virtual_host(VHost)' - 'Virtual host name. Default is /.',
			'Option heartbeat(Seconds)' - 'Heartbeat interval in seconds. Default is 60.',
			'Option channel_max(Max)' - 'Maximum number of channels. Default is 0 (no limit).',
			'Option frame_max(Max)' - 'Maximum frame size. Default is 131072.',
			'Option reconnect(Boolean)' - 'Enable automatic reconnection on connection failure. Default is false.',
			'Option reconnect_attempts(N)' - 'Maximum number of reconnection attempts. Default is 3. Only used when reconnect(true).',
			'Option reconnect_delay(Seconds)' - 'Delay between reconnection attempts in seconds. Default is 1. Only used when reconnect(true).'
		],
		exceptions is [
			'Connection refused or network error' - amqp_error(connection_failed),
			'Server rejected connection' - amqp_error(protocol_error('Message')),
			'Authentication failed' - amqp_error(auth_failed),
			'All reconnection attempts failed' - amqp_error(reconnect_failed)
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Gracefully closes the AMQP connection. Closes all channels and the connection itself.',
		argnames is ['Connection']
	]).

	:- public(close/3).
	:- mode(close(+compound, +integer, +atom), one_or_error).
	:- info(close/3, [
		comment is 'Closes the AMQP connection with a specific reply code and reason.',
		argnames is ['Connection', 'ReplyCode', 'ReplyText']
	]).

	:- public(connection_alive/1).
	:- mode(connection_alive(+compound), zero_or_one).
	:- info(connection_alive/1, [
		comment is 'Checks if the connection is still open and valid.',
		argnames is ['Connection']
	]).

	% ==========================================================================
	% Public API - Channel Management
	% ==========================================================================

	:- public(channel_open/3).
	:- mode(channel_open(+compound, +integer, --compound), one_or_error).
	:- info(channel_open/3, [
		comment is 'Opens a new channel on the connection. Returns a channel handle.',
		argnames is ['Connection', 'ChannelNumber', 'Channel'],
		exceptions is [
			'Channel already open' - amqp_error(channel_error('Message'))
		]
	]).

	:- public(channel_close/1).
	:- mode(channel_close(+compound), one_or_error).
	:- info(channel_close/1, [
		comment is 'Closes a channel.',
		argnames is ['Channel']
	]).

	:- public(channel_close/3).
	:- mode(channel_close(+compound, +integer, +atom), one_or_error).
	:- info(channel_close/3, [
		comment is 'Closes a channel with a specific reply code and reason.',
		argnames is ['Channel', 'ReplyCode', 'ReplyText']
	]).

	% ==========================================================================
	% Public API - Exchange Operations
	% ==========================================================================

	:- public(exchange_declare/3).
	:- mode(exchange_declare(+compound, +atom, +list), one_or_error).
	:- info(exchange_declare/3, [
		comment is 'Declares an exchange on the server.',
		argnames is ['Channel', 'Exchange', 'Options'],
		remarks is [
			'Option type(Type)' - 'Exchange type: direct, fanout, topic, headers. Default is direct.',
			'Option durable(Boolean)' - 'Survive server restart. Default is false.',
			'Option auto_delete(Boolean)' - 'Delete when unused. Default is false.',
			'Option internal(Boolean)' - 'Internal exchange. Default is false.',
			'Option arguments(Arguments)' - 'Additional arguments as key-value pairs.'
		]
	]).

	:- public(exchange_delete/3).
	:- mode(exchange_delete(+compound, +atom, +list), one_or_error).
	:- info(exchange_delete/3, [
		comment is 'Deletes an exchange.',
		argnames is ['Channel', 'Exchange', 'Options'],
		remarks is [
			'Option if_unused(Boolean)' - 'Only delete if unused. Default is false.'
		]
	]).

	:- public(exchange_bind/4).
	:- mode(exchange_bind(+compound, +atom, +atom, +list), one_or_error).
	:- info(exchange_bind/4, [
		comment is 'Binds an exchange to another exchange.',
		argnames is ['Channel', 'Destination', 'Source', 'Options'],
		remarks is [
			'Option routing_key(Key)' - 'Routing key for binding. Default is empty.',
			'Option arguments(Arguments)' - 'Additional arguments.'
		]
	]).

	:- public(exchange_unbind/4).
	:- mode(exchange_unbind(+compound, +atom, +atom, +list), one_or_error).
	:- info(exchange_unbind/4, [
		comment is 'Unbinds an exchange from another exchange.',
		argnames is ['Channel', 'Destination', 'Source', 'Options']
	]).

	% ==========================================================================
	% Public API - Queue Operations
	% ==========================================================================

	:- public(queue_declare/3).
	:- mode(queue_declare(+compound, ?atom, +list), one_or_error).
	:- info(queue_declare/3, [
		comment is 'Declares a queue on the server. If Queue is a variable, the server generates a unique name.',
		argnames is ['Channel', 'Queue', 'Options'],
		remarks is [
			'Option durable(Boolean)' - 'Survive server restart. Default is false.',
			'Option exclusive(Boolean)' - 'Exclusive to this connection. Default is false.',
			'Option auto_delete(Boolean)' - 'Delete when unused. Default is false.',
			'Option arguments(Arguments)' - 'Additional arguments (e.g., message TTL, dead letter exchange).'
		]
	]).

	:- public(queue_delete/3).
	:- mode(queue_delete(+compound, +atom, +list), one_or_error).
	:- info(queue_delete/3, [
		comment is 'Deletes a queue.',
		argnames is ['Channel', 'Queue', 'Options'],
		remarks is [
			'Option if_unused(Boolean)' - 'Only delete if unused. Default is false.',
			'Option if_empty(Boolean)' - 'Only delete if empty. Default is false.'
		]
	]).

	:- public(queue_bind/4).
	:- mode(queue_bind(+compound, +atom, +atom, +list), one_or_error).
	:- info(queue_bind/4, [
		comment is 'Binds a queue to an exchange.',
		argnames is ['Channel', 'Queue', 'Exchange', 'Options'],
		remarks is [
			'Option routing_key(Key)' - 'Routing key for binding. Default is empty.',
			'Option arguments(Arguments)' - 'Additional arguments.'
		]
	]).

	:- public(queue_unbind/4).
	:- mode(queue_unbind(+compound, +atom, +atom, +list), one_or_error).
	:- info(queue_unbind/4, [
		comment is 'Unbinds a queue from an exchange.',
		argnames is ['Channel', 'Queue', 'Exchange', 'Options']
	]).

	:- public(queue_purge/2).
	:- mode(queue_purge(+compound, +atom), one_or_error).
	:- info(queue_purge/2, [
		comment is 'Purges all messages from a queue.',
		argnames is ['Channel', 'Queue']
	]).

	% ==========================================================================
	% Public API - Basic (Message) Operations
	% ==========================================================================

	:- public(basic_publish/4).
	:- mode(basic_publish(+compound, +atom, +term, +list), one_or_error).
	:- info(basic_publish/4, [
		comment is 'Publishes a message to an exchange.',
		argnames is ['Channel', 'Exchange', 'Body', 'Options'],
		remarks is [
			'Option routing_key(Key)' - 'Routing key for message. Default is empty.',
			'Option mandatory(Boolean)' - 'Return if not routable. Default is false.',
			'Option immediate(Boolean)' - 'Return if not deliverable. Default is false (deprecated in RabbitMQ).',
			'Option content_type(Type)' - 'MIME content type.',
			'Option content_encoding(Enc)' - 'Content encoding.',
			'Option correlation_id(Id)' - 'Correlation identifier.',
			'Option reply_to(Queue)' - 'Reply queue name.',
			'Option expiration(Ms)' - 'Message TTL in milliseconds.',
			'Option message_id(Id)' - 'Application message identifier.',
			'Option timestamp(Ts)' - 'Message timestamp.',
			'Option type(Type)' - 'Message type name.',
			'Option user_id(Id)' - 'Creating user ID.',
			'Option app_id(Id)' - 'Creating application ID.',
			'Option delivery_mode(Mode)' - '1 for non-persistent, 2 for persistent.',
			'Option priority(P)' - 'Message priority (0-9).',
			'Option headers(H)' - 'Application headers as key-value pairs.'
		]
	]).

	:- public(basic_consume/3).
	:- mode(basic_consume(+compound, +atom, +list), one_or_error).
	:- info(basic_consume/3, [
		comment is 'Starts consuming messages from a queue.',
		argnames is ['Channel', 'Queue', 'Options'],
		remarks is [
			'Option consumer_tag(Tag)' - 'Consumer identifier. Server generates if not provided.',
			'Option no_local(Boolean)' - 'Do not receive own messages. Default is false.',
			'Option no_ack(Boolean)' - 'No acknowledgment required. Default is false.',
			'Option exclusive(Boolean)' - 'Exclusive consumer. Default is false.',
			'Option arguments(Arguments)' - 'Additional arguments.'
		]
	]).

	:- public(basic_cancel/3).
	:- mode(basic_cancel(+compound, +atom, +list), one_or_error).
	:- info(basic_cancel/3, [
		comment is 'Cancels a consumer.',
		argnames is ['Channel', 'ConsumerTag', 'Options']
	]).

	:- public(basic_get/3).
	:- mode(basic_get(+compound, +atom, +list), one_or_error).
	:- info(basic_get/3, [
		comment is 'Synchronously gets a message from a queue.',
		argnames is ['Channel', 'Queue', 'Options'],
		remarks is [
			'Option no_ack(Boolean)' - 'No acknowledgment required. Default is false.'
		]
	]).

	:- public(basic_ack/3).
	:- mode(basic_ack(+compound, +integer, +list), one_or_error).
	:- info(basic_ack/3, [
		comment is 'Acknowledges a message.',
		argnames is ['Channel', 'DeliveryTag', 'Options'],
		remarks is [
			'Option multiple(Boolean)' - 'Acknowledge all up to this tag. Default is false.'
		]
	]).

	:- public(basic_nack/3).
	:- mode(basic_nack(+compound, +integer, +list), one_or_error).
	:- info(basic_nack/3, [
		comment is 'Negatively acknowledges a message (RabbitMQ extension).',
		argnames is ['Channel', 'DeliveryTag', 'Options'],
		remarks is [
			'Option multiple(Boolean)' - 'Reject all up to this tag. Default is false.',
			'Option requeue(Boolean)' - 'Requeue the message. Default is true.'
		]
	]).

	:- public(basic_reject/3).
	:- mode(basic_reject(+compound, +integer, +list), one_or_error).
	:- info(basic_reject/3, [
		comment is 'Rejects a message.',
		argnames is ['Channel', 'DeliveryTag', 'Options'],
		remarks is [
			'Option requeue(Boolean)' - 'Requeue the message. Default is true.'
		]
	]).

	:- public(basic_qos/2).
	:- mode(basic_qos(+compound, +list), one_or_error).
	:- info(basic_qos/2, [
		comment is 'Sets quality of service (prefetch) settings.',
		argnames is ['Channel', 'Options'],
		remarks is [
			'Option prefetch_size(Size)' - 'Prefetch window size in bytes. Default is 0 (no limit).',
			'Option prefetch_count(Count)' - 'Prefetch window in messages. Default is 0 (no limit).',
			'Option global(Boolean)' - 'Apply to entire connection. Default is false.'
		]
	]).

	:- public(basic_recover/2).
	:- mode(basic_recover(+compound, +list), one_or_error).
	:- info(basic_recover/2, [
		comment is 'Asks the server to redeliver unacknowledged messages.',
		argnames is ['Channel', 'Options'],
		remarks is [
			'Option requeue(Boolean)' - 'Requeue messages. Default is false.'
		]
	]).

	% ==========================================================================
	% Public API - Receiving Messages
	% ==========================================================================

	:- public(receive/3).
	:- mode(receive(+compound, -compound, +list), zero_or_one_or_error).
	:- info(receive/3, [
		comment is 'Receives a message or method from the server. Blocks until data is available or timeout.',
		argnames is ['Channel', 'Message', 'Options'],
		remarks is [
			'Option timeout(Milliseconds)' - 'Timeout in milliseconds. 0 for non-blocking, -1 for infinite. Default is -1.'
		]
	]).

	% ==========================================================================
	% Public API - Transaction Operations
	% ==========================================================================

	:- public(tx_select/1).
	:- mode(tx_select(+compound), one_or_error).
	:- info(tx_select/1, [
		comment is 'Enables transaction mode on a channel.',
		argnames is ['Channel']
	]).

	:- public(tx_commit/1).
	:- mode(tx_commit(+compound), one_or_error).
	:- info(tx_commit/1, [
		comment is 'Commits the current transaction.',
		argnames is ['Channel']
	]).

	:- public(tx_rollback/1).
	:- mode(tx_rollback(+compound), one_or_error).
	:- info(tx_rollback/1, [
		comment is 'Rolls back the current transaction.',
		argnames is ['Channel']
	]).

	% ==========================================================================
	% Public API - Confirm Operations (Publisher Confirms)
	% ==========================================================================

	:- public(confirm_select/1).
	:- mode(confirm_select(+compound), one_or_error).
	:- info(confirm_select/1, [
		comment is 'Enables publisher confirms on a channel (RabbitMQ extension).',
		argnames is ['Channel']
	]).

	% ==========================================================================
	% Public API - Heartbeat
	% ==========================================================================

	:- public(send_heartbeat/1).
	:- mode(send_heartbeat(+compound), one_or_error).
	:- info(send_heartbeat/1, [
		comment is 'Sends a heartbeat frame to the server.',
		argnames is ['Connection']
	]).

	% ==========================================================================
	% Public API - Message Inspection
	% ==========================================================================

	:- public(message_body/2).
	:- mode(message_body(+compound, -term), one).
	:- info(message_body/2, [
		comment is 'Extracts the body from a message.',
		argnames is ['Message', 'Body']
	]).

	:- public(message_properties/2).
	:- mode(message_properties(+compound, -list), one).
	:- info(message_properties/2, [
		comment is 'Extracts the properties from a message as a list.',
		argnames is ['Message', 'Properties']
	]).

	:- public(message_property/3).
	:- mode(message_property(+compound, +atom, -term), zero_or_one).
	:- info(message_property/3, [
		comment is 'Extracts a specific property from a message. Fails if not present.',
		argnames is ['Message', 'PropertyName', 'Value']
	]).

	:- public(message_delivery_tag/2).
	:- mode(message_delivery_tag(+compound, -integer), one).
	:- info(message_delivery_tag/2, [
		comment is 'Extracts the delivery tag from a message.',
		argnames is ['Message', 'DeliveryTag']
	]).

	:- public(message_exchange/2).
	:- mode(message_exchange(+compound, -atom), one).
	:- info(message_exchange/2, [
		comment is 'Extracts the exchange name from a message.',
		argnames is ['Message', 'Exchange']
	]).

	:- public(message_routing_key/2).
	:- mode(message_routing_key(+compound, -atom), one).
	:- info(message_routing_key/2, [
		comment is 'Extracts the routing key from a message.',
		argnames is ['Message', 'RoutingKey']
	]).

	% ==========================================================================
	% Public API - Frame Encoding/Decoding (Low-level)
	% ==========================================================================

	:- public(encode_frame/2).
	:- mode(encode_frame(+compound, -list), one).
	:- info(encode_frame/2, [
		comment is 'Encodes an AMQP frame to a list of bytes.',
		argnames is ['Frame', 'Bytes']
	]).

	:- public(decode_frame/2).
	:- mode(decode_frame(+list, -compound), one_or_error).
	:- info(decode_frame/2, [
		comment is 'Decodes a list of bytes to an AMQP frame.',
		argnames is ['Bytes', 'Frame']
	]).

	% ==========================================================================
	% Uses directives
	% ==========================================================================

	:- uses(list, [
		append/2, append/3, length/2, member/2, reverse/2, valid/1 as is_list/1
	]).

	:- uses(term_io, [
		write_to_codes/2
	]).

	% ==========================================================================
	% AMQP Constants
	% ==========================================================================

	% Protocol header
	amqp_protocol_header([65, 77, 81, 80, 0, 0, 9, 1]).  % "AMQP" 0 0 9 1

	% Frame types
	frame_type(method,    1).
	frame_type(header,    2).
	frame_type(body,      3).
	frame_type(heartbeat, 8).

	% Frame end marker
	frame_end(206).  % 0xCE

	% Class IDs
	class_id(connection, 10).
	class_id(channel,    20).
	class_id(exchange,   40).
	class_id(queue,      50).
	class_id(basic,      60).
	class_id(tx,         90).
	class_id(confirm,    85).

	% Connection method IDs
	method_id(connection, start,     10).
	method_id(connection, start_ok,  11).
	method_id(connection, secure,    20).
	method_id(connection, secure_ok, 21).
	method_id(connection, tune,      30).
	method_id(connection, tune_ok,   31).
	method_id(connection, open,      40).
	method_id(connection, open_ok,   41).
	method_id(connection, close,     50).
	method_id(connection, close_ok,  51).

	% Channel method IDs
	method_id(channel, open,     10).
	method_id(channel, open_ok,  11).
	method_id(channel, flow,     20).
	method_id(channel, flow_ok,  21).
	method_id(channel, close,    40).
	method_id(channel, close_ok, 41).

	% Exchange method IDs
	method_id(exchange, declare,    10).
	method_id(exchange, declare_ok, 11).
	method_id(exchange, delete,     20).
	method_id(exchange, delete_ok,  21).
	method_id(exchange, bind,       30).
	method_id(exchange, bind_ok,    31).
	method_id(exchange, unbind,     40).
	method_id(exchange, unbind_ok,  51).

	% Queue method IDs
	method_id(queue, declare,    10).
	method_id(queue, declare_ok, 11).
	method_id(queue, bind,       20).
	method_id(queue, bind_ok,    21).
	method_id(queue, unbind,     50).
	method_id(queue, unbind_ok,  51).
	method_id(queue, purge,      30).
	method_id(queue, purge_ok,   31).
	method_id(queue, delete,     40).
	method_id(queue, delete_ok,  41).

	% Basic method IDs
	method_id(basic, qos,         10).
	method_id(basic, qos_ok,      11).
	method_id(basic, consume,     20).
	method_id(basic, consume_ok,  21).
	method_id(basic, cancel,      30).
	method_id(basic, cancel_ok,   31).
	method_id(basic, publish,     40).
	method_id(basic, return,      50).
	method_id(basic, deliver,     60).
	method_id(basic, get,         70).
	method_id(basic, get_ok,      71).
	method_id(basic, get_empty,   72).
	method_id(basic, ack,         80).
	method_id(basic, reject,      90).
	method_id(basic, recover_async, 100).
	method_id(basic, recover,     110).
	method_id(basic, recover_ok,  111).
	method_id(basic, nack,        120).

	% Tx method IDs
	method_id(tx, select,      10).
	method_id(tx, select_ok,   11).
	method_id(tx, commit,      20).
	method_id(tx, commit_ok,   21).
	method_id(tx, rollback,    30).
	method_id(tx, rollback_ok, 31).

	% Confirm method IDs
	method_id(confirm, select,    10).
	method_id(confirm, select_ok, 11).

	% ==========================================================================
	% Implementation - Connection Management
	% ==========================================================================

	connect(Host, Port, Connection, Options) :-
		context(Context),
		option(reconnect(Reconnect), Options, false),
		(	Reconnect == true ->
			option(reconnect_attempts(MaxAttempts), Options, 3),
			option(reconnect_delay(Delay), Options, 1),
			connect_with_retry(Host, Port, Connection, Options, Context, MaxAttempts, Delay)
		;	catch(
				connect_(Host, Port, Connection, Options, Context),
				Error,
				throw(Error)
			)
		).

	% Connect with automatic retry on failure
	connect_with_retry(Host, Port, Connection, Options, Context, MaxAttempts, Delay) :-
		connect_with_retry_(Host, Port, Connection, Options, Context, MaxAttempts, Delay, 1).

	connect_with_retry_(Host, Port, Connection, Options, Context, MaxAttempts, Delay, Attempt) :-
		catch(
			connect_(Host, Port, Connection, Options, Context),
			Error,
			(	Attempt < MaxAttempts ->
				% Wait before retrying
				os::sleep(Delay),
				NextAttempt is Attempt + 1,
				connect_with_retry_(Host, Port, Connection, Options, Context, MaxAttempts, Delay, NextAttempt)
			;	% All attempts exhausted
				(	Error = error(amqp_error(_), _) ->
					throw(error(amqp_error(reconnect_failed), Context))
				;	throw(Error)
				)
			)
		).

	connect_(Host, Port, Connection, Options, Context) :-
		% Open TCP connection
		catch(
			socket::client_open(Host, Port, Input, Output, []),
			_,
			throw(error(amqp_error(connection_failed), Context))
		),
		% Send protocol header
		amqp_protocol_header(Header),
		write_bytes(Header, Output),
		flush_output(Output),
		% Receive Connection.Start
		read_frame(Input, StartFrame, Context),
		(	StartFrame = frame(method, 0, connection, start, StartArguments) ->
			true
		;	socket::close(Input, Output),
			throw(error(amqp_error(protocol_error('Expected Connection.Start')), Context))
		),
		% Extract server capabilities
		member(mechanisms-Mechanisms, StartArguments),
		(	member(locales-_Locales, StartArguments) -> true ; true),
		% Send Connection.Start-Ok
		option(username(Username), Options, 'guest'),
		option(password(Password), Options, 'guest'),
		build_sasl_response(Username, Password, Mechanisms, SaslResponse),
		StartOkArguments = [
			client_properties-[
				product-longstr('Logtalk AMQP'),
				version-longstr('1.0.0'),
				platform-longstr('Logtalk'),
				capabilities-table([
					publisher_confirms-bool(true),
					consumer_cancel_notify-bool(true),
					exchange_exchange_bindings-bool(true),
					'basic.nack'-bool(true),
					'connection.blocked'-bool(true)
				])
			],
			mechanism-'PLAIN',
			response-SaslResponse,
			locale-'en_US'
		],
		send_method(Output, 0, connection, start_ok, StartOkArguments),
		% Receive Connection.Tune
		read_frame(Input, TuneFrame, Context),
		(	TuneFrame = frame(method, 0, connection, tune, TuneArguments) ->
			true
		;	socket::close(Input, Output),
			throw(error(amqp_error(protocol_error('Expected Connection.Tune')), Context))
		),
		% Negotiate parameters
		member(channel_max-ServerChannelMax, TuneArguments),
		member(frame_max-ServerFrameMax, TuneArguments),
		member(heartbeat-ServerHeartbeat, TuneArguments),
		option(channel_max(ClientChannelMax), Options, 0),
		option(frame_max(ClientFrameMax), Options, 131072),
		option(heartbeat(ClientHeartbeat), Options, 60),
		negotiate_value(ClientChannelMax, ServerChannelMax, NegotiatedChannelMax),
		negotiate_value(ClientFrameMax, ServerFrameMax, NegotiatedFrameMax),
		negotiate_heartbeat(ClientHeartbeat, ServerHeartbeat, NegotiatedHeartbeat),
		% Send Connection.Tune-Ok
		TuneOkArguments = [
			channel_max-NegotiatedChannelMax,
			frame_max-NegotiatedFrameMax,
			heartbeat-NegotiatedHeartbeat
		],
		send_method(Output, 0, connection, tune_ok, TuneOkArguments),
		% Send Connection.Open
		option(virtual_host(VHost), Options, '/'),
		OpenArguments = [
			virtual_host-VHost,
			reserved_1-'',
			reserved_2-0
		],
		send_method(Output, 0, connection, open, OpenArguments),
		% Receive Connection.Open-Ok
		read_frame(Input, OpenOkFrame, Context),
		(	OpenOkFrame = frame(method, 0, connection, open_ok, _) ->
			Connection = connection(Input, Output, Host, Port, NegotiatedChannelMax, NegotiatedFrameMax, NegotiatedHeartbeat, Options)
		;	socket::close(Input, Output),
			throw(error(amqp_error(protocol_error('Expected Connection.Open-Ok')), Context))
		).

	close(Connection) :-
		close(Connection, 200, 'Normal shutdown').

	close(Connection, ReplyCode, ReplyText) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		CloseArguments = [
			reply_code-ReplyCode,
			reply_text-ReplyText,
			class_id-0,
			method_id-0
		],
		catch(
			(	send_method(Output, 0, connection, close, CloseArguments),
				read_frame(Input, CloseOkFrame, Context),
				(	CloseOkFrame = frame(method, 0, connection, close_ok, _) ->
					true
				;	true
				)
			),
			_,
			true
		),
		catch(socket::close(Input, Output), _, true).

	connection_alive(Connection) :-
		arg(1, Connection, Input),
		catch(
			stream_property(Input, _),
			_,
			fail
		).

	% ==========================================================================
	% Implementation - Channel Management
	% ==========================================================================

	channel_open(Connection, ChannelNumber, Channel) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		OpenArguments = [reserved_1-''],
		send_method(Output, ChannelNumber, channel, open, OpenArguments),
		read_frame(Input, OpenOkFrame, Context),
		(	OpenOkFrame = frame(method, ChannelNumber, channel, open_ok, _) ->
			Channel = channel(Connection, ChannelNumber)
		;	throw(error(amqp_error(channel_error('Channel open failed')), Context))
		).

	channel_close(Channel) :-
		channel_close(Channel, 200, 'Normal shutdown').

	channel_close(Channel, ReplyCode, ReplyText) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		CloseArguments = [
			reply_code-ReplyCode,
			reply_text-ReplyText,
			class_id-0,
			method_id-0
		],
		catch(
			(	send_method(Output, ChannelNumber, channel, close, CloseArguments),
				read_frame(Input, CloseOkFrame, Context),
				(	CloseOkFrame = frame(method, ChannelNumber, channel, close_ok, _) ->
					true
				;	true
				)
			),
			_,
			true
		).

	% ==========================================================================
	% Implementation - Exchange Operations
	% ==========================================================================

	exchange_declare(Channel, Exchange, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(type(Type), Options, direct),
		option(durable(Durable), Options, false),
		option(auto_delete(AutoDelete), Options, false),
		option(internal(Internal), Options, false),
		option(arguments(Arguments), Options, []),
		boolean_to_integer(Durable, DurableInt),
		boolean_to_integer(AutoDelete, AutoDeleteInt),
		boolean_to_integer(Internal, InternalInt),
		% Flags: reserved1=0, durable, auto_delete, internal, nowait=0
		Flags is (DurableInt << 1) \/ (AutoDeleteInt << 2) \/ (InternalInt << 3),
		DeclareArguments = [
			reserved_1-0,
			exchange-Exchange,
			type-Type,
			flags-Flags,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, exchange, declare, DeclareArguments),
		read_frame(Input, DeclareOkFrame, Context),
		(	DeclareOkFrame = frame(method, ChannelNumber, exchange, declare_ok, _) ->
			true
		;	throw(error(amqp_error(exchange_error('Exchange declare failed')), Context))
		).

	exchange_delete(Channel, Exchange, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(if_unused(IfUnused), Options, false),
		boolean_to_integer(IfUnused, IfUnusedInt),
		Flags is (IfUnusedInt << 0),
		DeleteArguments = [
			reserved_1-0,
			exchange-Exchange,
			flags-Flags
		],
		send_method(Output, ChannelNumber, exchange, delete, DeleteArguments),
		read_frame(Input, DeleteOkFrame, Context),
		(	DeleteOkFrame = frame(method, ChannelNumber, exchange, delete_ok, _) ->
			true
		;	throw(error(amqp_error(exchange_error('Exchange delete failed')), Context))
		).

	exchange_bind(Channel, Destination, Source, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(routing_key(RoutingKey), Options, ''),
		option(arguments(Arguments), Options, []),
		BindArguments = [
			reserved_1-0,
			destination-Destination,
			source-Source,
			routing_key-RoutingKey,
			nowait-0,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, exchange, bind, BindArguments),
		read_frame(Input, BindOkFrame, Context),
		(	BindOkFrame = frame(method, ChannelNumber, exchange, bind_ok, _) ->
			true
		;	throw(error(amqp_error(exchange_error('Exchange bind failed')), Context))
		).

	exchange_unbind(Channel, Destination, Source, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(routing_key(RoutingKey), Options, ''),
		option(arguments(Arguments), Options, []),
		UnbindArguments = [
			reserved_1-0,
			destination-Destination,
			source-Source,
			routing_key-RoutingKey,
			nowait-0,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, exchange, unbind, UnbindArguments),
		read_frame(Input, UnbindOkFrame, Context),
		(	UnbindOkFrame = frame(method, ChannelNumber, exchange, unbind_ok, _) ->
			true
		;	throw(error(amqp_error(exchange_error('Exchange unbind failed')), Context))
		).

	% ==========================================================================
	% Implementation - Queue Operations
	% ==========================================================================

	queue_declare(Channel, Queue, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(durable(Durable), Options, false),
		option(exclusive(Exclusive), Options, false),
		option(auto_delete(AutoDelete), Options, false),
		option(arguments(Arguments), Options, []),
		boolean_to_integer(Durable, DurableInt),
		boolean_to_integer(Exclusive, ExclusiveInt),
		boolean_to_integer(AutoDelete, AutoDeleteInt),
		% Flags: reserved1=0, durable, exclusive, auto_delete, nowait=0
		Flags is (DurableInt << 1) \/ (ExclusiveInt << 2) \/ (AutoDeleteInt << 3),
		(	var(Queue) ->
			QueueName = ''
		;	QueueName = Queue
		),
		DeclareArguments = [
			reserved_1-0,
			queue-QueueName,
			flags-Flags,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, queue, declare, DeclareArguments),
		read_frame(Input, DeclareOkFrame, Context),
		(	DeclareOkFrame = frame(method, ChannelNumber, queue, declare_ok, DeclareOkArguments) ->
			(	var(Queue) ->
				member(queue-Queue, DeclareOkArguments)
			;	true
			)
		;	throw(error(amqp_error(queue_error('Queue declare failed')), Context))
		).

	queue_delete(Channel, Queue, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(if_unused(IfUnused), Options, false),
		option(if_empty(IfEmpty), Options, false),
		boolean_to_integer(IfUnused, IfUnusedInt),
		boolean_to_integer(IfEmpty, IfEmptyInt),
		Flags is (IfUnusedInt << 0) \/ (IfEmptyInt << 1),
		DeleteArguments = [
			reserved_1-0,
			queue-Queue,
			flags-Flags
		],
		send_method(Output, ChannelNumber, queue, delete, DeleteArguments),
		read_frame(Input, DeleteOkFrame, Context),
		(	DeleteOkFrame = frame(method, ChannelNumber, queue, delete_ok, _) ->
			true
		;	throw(error(amqp_error(queue_error('Queue delete failed')), Context))
		).

	queue_bind(Channel, Queue, Exchange, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(routing_key(RoutingKey), Options, ''),
		option(arguments(Arguments), Options, []),
		BindArguments = [
			reserved_1-0,
			queue-Queue,
			exchange-Exchange,
			routing_key-RoutingKey,
			nowait-0,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, queue, bind, BindArguments),
		read_frame(Input, BindOkFrame, Context),
		(	BindOkFrame = frame(method, ChannelNumber, queue, bind_ok, _) ->
			true
		;	throw(error(amqp_error(queue_error('Queue bind failed')), Context))
		).

	queue_unbind(Channel, Queue, Exchange, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(routing_key(RoutingKey), Options, ''),
		option(arguments(Arguments), Options, []),
		UnbindArguments = [
			reserved_1-0,
			queue-Queue,
			exchange-Exchange,
			routing_key-RoutingKey,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, queue, unbind, UnbindArguments),
		read_frame(Input, UnbindOkFrame, Context),
		(	UnbindOkFrame = frame(method, ChannelNumber, queue, unbind_ok, _) ->
			true
		;	throw(error(amqp_error(queue_error('Queue unbind failed')), Context))
		).

	queue_purge(Channel, Queue) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		PurgeArguments = [
			reserved_1-0,
			queue-Queue,
			nowait-0
		],
		send_method(Output, ChannelNumber, queue, purge, PurgeArguments),
		read_frame(Input, PurgeOkFrame, Context),
		(	PurgeOkFrame = frame(method, ChannelNumber, queue, purge_ok, _) ->
			true
		;	throw(error(amqp_error(queue_error('Queue purge failed')), Context))
		).

	% ==========================================================================
	% Implementation - Basic Operations
	% ==========================================================================

	basic_publish(Channel, Exchange, Body, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(2, Connection, Output),
		option(routing_key(RoutingKey), Options, ''),
		option(mandatory(Mandatory), Options, false),
		option(immediate(Immediate), Options, false),
		boolean_to_integer(Mandatory, MandatoryInt),
		boolean_to_integer(Immediate, ImmediateInt),
		Flags is (MandatoryInt << 0) \/ (ImmediateInt << 1),
		PublishArguments = [
			reserved_1-0,
			exchange-Exchange,
			routing_key-RoutingKey,
			flags-Flags
		],
		% Send method frame
		send_method(Output, ChannelNumber, basic, publish, PublishArguments),
		% Build content properties
		build_content_properties(Options, Properties, PropertyFlags),
		% Convert body to bytes
		body_to_bytes(Body, BodyBytes),
		length(BodyBytes, BodySize),
		% Send header frame
		send_content_header(Output, ChannelNumber, BodySize, PropertyFlags, Properties, Context),
		% Send body frame(s)
		arg(6, Connection, FrameMax),
		MaxBodySize is FrameMax - 8,  % Frame overhead
		send_content_body(Output, ChannelNumber, BodyBytes, MaxBodySize, Context).

	basic_consume(Channel, Queue, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(consumer_tag(ConsumerTag), Options, ''),
		option(no_local(NoLocal), Options, false),
		option(no_ack(NoAck), Options, false),
		option(exclusive(Exclusive), Options, false),
		option(arguments(Arguments), Options, []),
		boolean_to_integer(NoLocal, NoLocalInt),
		boolean_to_integer(NoAck, NoAckInt),
		boolean_to_integer(Exclusive, ExclusiveInt),
		Flags is (NoLocalInt << 0) \/ (NoAckInt << 1) \/ (ExclusiveInt << 2),
		ConsumeArguments = [
			reserved_1-0,
			queue-Queue,
			consumer_tag-ConsumerTag,
			flags-Flags,
			arguments-Arguments
		],
		send_method(Output, ChannelNumber, basic, consume, ConsumeArguments),
		read_frame(Input, ConsumeOkFrame, Context),
		(	ConsumeOkFrame = frame(method, ChannelNumber, basic, consume_ok, _) ->
			true
		;	throw(error(amqp_error(basic_error('Basic consume failed')), Context))
		).

	basic_cancel(Channel, ConsumerTag, _Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		CancelArguments = [
			consumer_tag-ConsumerTag,
			nowait-0
		],
		send_method(Output, ChannelNumber, basic, cancel, CancelArguments),
		read_frame(Input, CancelOkFrame, Context),
		(	CancelOkFrame = frame(method, ChannelNumber, basic, cancel_ok, _) ->
			true
		;	throw(error(amqp_error(basic_error('Basic cancel failed')), Context))
		).

	basic_get(Channel, Queue, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(no_ack(NoAck), Options, false),
		boolean_to_integer(NoAck, NoAckInt),
		GetArguments = [
			reserved_1-0,
			queue-Queue,
			no_ack-NoAckInt
		],
		send_method(Output, ChannelNumber, basic, get, GetArguments),
		read_frame(Input, GetFrame, Context),
		(	GetFrame = frame(method, ChannelNumber, basic, get_ok, _) ->
			true
		;	GetFrame = frame(method, ChannelNumber, basic, get_empty, _) ->
			fail
		;	throw(error(amqp_error(basic_error('Basic get failed')), Context))
		).

	basic_ack(Channel, DeliveryTag, Options) :-
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(2, Connection, Output),
		option(multiple(Multiple), Options, false),
		boolean_to_integer(Multiple, MultipleInt),
		AckArguments = [
			delivery_tag-DeliveryTag,
			multiple-MultipleInt
		],
		send_method(Output, ChannelNumber, basic, ack, AckArguments).

	basic_nack(Channel, DeliveryTag, Options) :-
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(2, Connection, Output),
		option(multiple(Multiple), Options, false),
		option(requeue(Requeue), Options, true),
		boolean_to_integer(Multiple, MultipleInt),
		boolean_to_integer(Requeue, RequeueInt),
		Flags is (MultipleInt << 0) \/ (RequeueInt << 1),
		NackArguments = [
			delivery_tag-DeliveryTag,
			flags-Flags
		],
		send_method(Output, ChannelNumber, basic, nack, NackArguments).

	basic_reject(Channel, DeliveryTag, Options) :-
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(2, Connection, Output),
		option(requeue(Requeue), Options, true),
		boolean_to_integer(Requeue, RequeueInt),
		RejectArguments = [
			delivery_tag-DeliveryTag,
			requeue-RequeueInt
		],
		send_method(Output, ChannelNumber, basic, reject, RejectArguments).

	basic_qos(Channel, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(prefetch_size(PrefetchSize), Options, 0),
		option(prefetch_count(PrefetchCount), Options, 0),
		option(global(Global), Options, false),
		boolean_to_integer(Global, GlobalInt),
		QosArguments = [
			prefetch_size-PrefetchSize,
			prefetch_count-PrefetchCount,
			global-GlobalInt
		],
		send_method(Output, ChannelNumber, basic, qos, QosArguments),
		read_frame(Input, QosOkFrame, Context),
		(	QosOkFrame = frame(method, ChannelNumber, basic, qos_ok, _) ->
			true
		;	throw(error(amqp_error(basic_error('Basic qos failed')), Context))
		).

	basic_recover(Channel, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		option(requeue(Requeue), Options, false),
		boolean_to_integer(Requeue, RequeueInt),
		RecoverArguments = [
			requeue-RequeueInt
		],
		send_method(Output, ChannelNumber, basic, recover, RecoverArguments),
		read_frame(Input, RecoverOkFrame, Context),
		(	RecoverOkFrame = frame(method, ChannelNumber, basic, recover_ok, _) ->
			true
		;	throw(error(amqp_error(basic_error('Basic recover failed')), Context))
		).

	% ==========================================================================
	% Implementation - Receiving Messages
	% ==========================================================================

	receive(Channel, Message, Options) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		option(timeout(_Timeout), Options, -1),
		% Read the deliver/return method frame
		read_frame(Input, MethodFrame, Context),
		(	MethodFrame = frame(method, ChannelNumber, basic, deliver, DeliverArguments) ->
			% Read content header
			read_frame(Input, HeaderFrame, Context),
			(	HeaderFrame = frame(header, ChannelNumber, basic, BodySize, Properties) ->
				% Read content body
				read_content_body(Input, BodySize, BodyBytes, Context),
				atom_codes(Body, BodyBytes),
				member(delivery_tag-DeliveryTag, DeliverArguments),
				member(exchange-Exchange, DeliverArguments),
				member(routing_key-RoutingKey, DeliverArguments),
				Message = message(deliver, DeliveryTag, Exchange, RoutingKey, Properties, Body)
			;	throw(error(amqp_error(protocol_error('Expected content header')), Context))
			)
		;	MethodFrame = frame(method, ChannelNumber, basic, return, ReturnArguments) ->
			% Returned message
			read_frame(Input, HeaderFrame, Context),
			(	HeaderFrame = frame(header, ChannelNumber, basic, BodySize, Properties) ->
				read_content_body(Input, BodySize, BodyBytes, Context),
				atom_codes(Body, BodyBytes),
				member(exchange-Exchange, ReturnArguments),
				member(routing_key-RoutingKey, ReturnArguments),
				member(reply_code-ReplyCode, ReturnArguments),
				member(reply_text-ReplyText, ReturnArguments),
				Message = message(return, ReplyCode, ReplyText, Exchange, RoutingKey, Properties, Body)
			;	throw(error(amqp_error(protocol_error('Expected content header')), Context))
			)
		;	MethodFrame = frame(heartbeat, 0, _, _, _) ->
			% Heartbeat received, recurse
			receive(Channel, Message, Options)
		;	% Other method frame - return as-is
			Message = MethodFrame
		).

	% ==========================================================================
	% Implementation - Transaction Operations
	% ==========================================================================

	tx_select(Channel) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		send_method(Output, ChannelNumber, tx, select, []),
		read_frame(Input, SelectOkFrame, Context),
		(	SelectOkFrame = frame(method, ChannelNumber, tx, select_ok, _) ->
			true
		;	throw(error(amqp_error(tx_error('Tx select failed')), Context))
		).

	tx_commit(Channel) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		send_method(Output, ChannelNumber, tx, commit, []),
		read_frame(Input, CommitOkFrame, Context),
		(	CommitOkFrame = frame(method, ChannelNumber, tx, commit_ok, _) ->
			true
		;	throw(error(amqp_error(tx_error('Tx commit failed')), Context))
		).

	tx_rollback(Channel) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		send_method(Output, ChannelNumber, tx, rollback, []),
		read_frame(Input, RollbackOkFrame, Context),
		(	RollbackOkFrame = frame(method, ChannelNumber, tx, rollback_ok, _) ->
			true
		;	throw(error(amqp_error(tx_error('Tx rollback failed')), Context))
		).

	% ==========================================================================
	% Implementation - Confirm Operations
	% ==========================================================================

	confirm_select(Channel) :-
		context(Context),
		arg(1, Channel, Connection),
		arg(2, Channel, ChannelNumber),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		send_method(Output, ChannelNumber, confirm, select, [nowait-0]),
		read_frame(Input, SelectOkFrame, Context),
		(	SelectOkFrame = frame(method, ChannelNumber, confirm, select_ok, _) ->
			true
		;	throw(error(amqp_error(confirm_error('Confirm select failed')), Context))
		).

	% ==========================================================================
	% Implementation - Heartbeat
	% ==========================================================================

	send_heartbeat(Connection) :-
		arg(2, Connection, Output),
		frame_type(heartbeat, Type),
		frame_end(FrameEnd),
		Bytes = [Type, 0, 0, 0, 0, 0, 0, FrameEnd],
		write_bytes(Bytes, Output),
		flush_output(Output).

	% ==========================================================================
	% Implementation - Message Inspection
	% ==========================================================================

	message_body(message(_, _, _, _, _, Body), Body) :- !.
	message_body(message(_, _, _, _, _, _, Body), Body).

	message_properties(message(_, _, _, _, Properties, _), Properties) :- !.
	message_properties(message(_, _, _, _, _, Properties, _), Properties).

	message_property(Message, Name, Value) :-
		message_properties(Message, Properties),
		member(Name-Value, Properties).

	message_delivery_tag(message(deliver, DeliveryTag, _, _, _, _), DeliveryTag).

	message_exchange(message(deliver, _, Exchange, _, _, _), Exchange) :- !.
	message_exchange(message(return, _, _, Exchange, _, _, _), Exchange).

	message_routing_key(message(deliver, _, _, RoutingKey, _, _), RoutingKey) :- !.
	message_routing_key(message(return, _, _, _, RoutingKey, _, _), RoutingKey).

	% ==========================================================================
	% Frame Encoding/Decoding - Public API
	% ==========================================================================

	encode_frame(frame(Type, Channel, Payload), Bytes) :-
		frame_type(Type, TypeByte),
		frame_end(FrameEnd),
		encode_payload(Type, Payload, PayloadBytes),
		length(PayloadBytes, PayloadSize),
		encode_short(Channel, ChannelBytes),
		encode_long(PayloadSize, SizeBytes),
		append([[TypeByte| ChannelBytes], SizeBytes, PayloadBytes, [FrameEnd]], Bytes).

	decode_frame(Bytes, Frame) :-
		Bytes = [TypeByte| Rest1],
		frame_type(Type, TypeByte),
		decode_short(Rest1, Channel, Rest2),
		decode_long(Rest2, PayloadSize, Rest3),
		length(PayloadBytes, PayloadSize),
		append(PayloadBytes, [FrameEnd| _], Rest3),
		frame_end(FrameEnd),
		decode_payload(Type, Channel, PayloadBytes, Frame).

	% ==========================================================================
	% Binary Encoding Helpers
	% ==========================================================================

	% Encode 8-bit unsigned integer (octet)
	encode_octet(Value, [Byte]) :-
		Byte is Value /\ 0xFF.

	% Decode 8-bit unsigned integer
	decode_octet([Byte| Rest], Value, Rest) :-
		Value is Byte /\ 0xFF.

	% Encode 16-bit unsigned integer (short) - big endian
	encode_short(Value, [Hi, Lo]) :-
		Hi is (Value >> 8) /\ 0xFF,
		Lo is Value /\ 0xFF.

	% Decode 16-bit unsigned integer
	decode_short([Hi, Lo| Rest], Value, Rest) :-
		Value is (Hi << 8) \/ Lo.

	% Encode 32-bit unsigned integer (long) - big endian
	encode_long(Value, [B3, B2, B1, B0]) :-
		B3 is (Value >> 24) /\ 0xFF,
		B2 is (Value >> 16) /\ 0xFF,
		B1 is (Value >> 8) /\ 0xFF,
		B0 is Value /\ 0xFF.

	% Decode 32-bit unsigned integer
	decode_long([B3, B2, B1, B0| Rest], Value, Rest) :-
		Value is (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

	% Encode 64-bit unsigned integer (long long) - big endian
	encode_longlong(Value, [B7, B6, B5, B4, B3, B2, B1, B0]) :-
		B7 is (Value >> 56) /\ 0xFF,
		B6 is (Value >> 48) /\ 0xFF,
		B5 is (Value >> 40) /\ 0xFF,
		B4 is (Value >> 32) /\ 0xFF,
		B3 is (Value >> 24) /\ 0xFF,
		B2 is (Value >> 16) /\ 0xFF,
		B1 is (Value >> 8) /\ 0xFF,
		B0 is Value /\ 0xFF.

	% Decode 64-bit unsigned integer
	decode_longlong([B7, B6, B5, B4, B3, B2, B1, B0| Rest], Value, Rest) :-
		Value is (B7 << 56) \/ (B6 << 48) \/ (B5 << 40) \/ (B4 << 32) \/
		         (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

	% Encode short string (length <= 255)
	encode_shortstr(Atom, Bytes) :-
		atom_codes(Atom, Codes),
		length(Codes, Length),
		(	Length > 255 ->
			throw(error(amqp_error(encoding_error('Short string too long')), _))
		;	Bytes = [Length| Codes]
		).

	% Decode short string
	decode_shortstr([Length| Rest], Atom, Remaining) :-
		length(Codes, Length),
		append(Codes, Remaining, Rest),
		atom_codes(Atom, Codes).

	% Encode long string
	encode_longstr(Atom, Bytes) :-
		atom_codes(Atom, Codes),
		length(Codes, Length),
		encode_long(Length, LenBytes),
		append(LenBytes, Codes, Bytes).

	% Decode long string
	decode_longstr(Bytes, Atom, Rest) :-
		decode_long(Bytes, Length, After),
		length(Codes, Length),
		append(Codes, Rest, After),
		atom_codes(Atom, Codes).

	% Encode field table
	encode_table([], [0, 0, 0, 0]).
	encode_table(Pairs, Bytes) :-
		Pairs \== [],
		encode_table_pairs(Pairs, PairBytes),
		length(PairBytes, Length),
		encode_long(Length, LengthBytes),
		append(LengthBytes, PairBytes, Bytes).

	encode_table_pairs([], []).
	encode_table_pairs([Key-Value| Rest], Bytes) :-
		encode_shortstr(Key, KeyBytes),
		encode_field_value(Value, ValueBytes),
		encode_table_pairs(Rest, RestBytes),
		append([KeyBytes, ValueBytes, RestBytes], Bytes).

	% Encode field value with type tag
	encode_field_value(bool(true), [116, 1]) :- !.    % 't' for boolean
	encode_field_value(bool(false), [116, 0]) :- !.
	encode_field_value(byte(V), [98| Bytes]) :- !, encode_octet(V, Bytes).  % 'b'
	encode_field_value(short(V), [115| Bytes]) :- !, encode_short(V, Bytes).  % 's'
	encode_field_value(int(V), [73| Bytes]) :- !, encode_long(V, Bytes).  % 'I'
	encode_field_value(long(V), [108| Bytes]) :- !, encode_longlong(V, Bytes).  % 'l'
	encode_field_value(float(V), [102| Bytes]) :- !, encode_float(V, Bytes).  % 'f'
	encode_field_value(double(V), [100| Bytes]) :- !, encode_double(V, Bytes).  % 'd'
	encode_field_value(longstr(V), [83| Bytes]) :- !, encode_longstr(V, Bytes).  % 'S'
	encode_field_value(table(V), [70| Bytes]) :- !, encode_table(V, Bytes).  % 'F'
	encode_field_value(array(V), [65| Bytes]) :- !, encode_array(V, Bytes).  % 'A'
	encode_field_value(void, [86]) :- !.  % 'V'
	encode_field_value(timestamp(V), [84| Bytes]) :- !, encode_longlong(V, Bytes).  % 'T'
	% Default: treat atom as short string
	encode_field_value(V, [83| Bytes]) :-
		atom(V),
		encode_longstr(V, Bytes).

	% Decode field table
	decode_table(Bytes, Pairs, Rest) :-
		decode_long(Bytes, Length, After),
		(	Length == 0 ->
			Pairs = [],
			Rest = After
		;	length(TableBytes, Length),
			append(TableBytes, Rest, After),
			decode_table_pairs(TableBytes, Pairs)
		).

	decode_table_pairs([], []).
	decode_table_pairs([Byte| Bytes], [Key-Value| Pairs]) :-
		decode_shortstr([Byte| Bytes], Key, [First| After1]),
		decode_field_value(First, After1, Value, After2),
		decode_table_pairs(After2, Pairs).

	% Decode field value
	decode_field_value(116, [Byte| Rest], bool(Value), Rest) :-
		(Byte == 1 -> Value = true ; Value = false).
	decode_field_value(98, Bytes, byte(Value), Rest) :-
		decode_octet(Bytes, Value, Rest).
	decode_field_value(115, Bytes, short(Value), Rest) :-
		decode_short(Bytes, Value, Rest).
	decode_field_value(73, Bytes, int(Value), Rest) :-
		decode_long(Bytes, Value, Rest).
	decode_field_value(108, Bytes, long(Value), Rest) :-
		decode_longlong(Bytes, Value, Rest).
	decode_field_value(102, Bytes, float(Value), Rest) :-
		decode_float(Bytes, Value, Rest).
	decode_field_value(100, Bytes, double(Value), Rest) :-
		decode_double(Bytes, Value, Rest).
	decode_field_value(83, Bytes, longstr(Value), Rest) :-
		decode_longstr(Bytes, Value, Rest).
	decode_field_value(70, Bytes, table(Value), Rest) :-
		decode_table(Bytes, Value, Rest).
	decode_field_value(65, Bytes, array(Value), Rest) :-
		decode_array(Bytes, Value, Rest).
	decode_field_value(86, Rest, void, Rest).
	decode_field_value(84, Bytes, timestamp(Value), Rest) :-
		decode_longlong(Bytes, Value, Rest).

	% For testing
	decode_field_value([Byte| Bytes], Value, Rest) :-
		decode_field_value(Byte, Bytes, Value, Rest).

	% Encode array
	encode_array(Values, Bytes) :-
		encode_array_values(Values, ValueBytes),
		length(ValueBytes, Length),
		encode_long(Length, LengthBytes),
		append(LengthBytes, ValueBytes, Bytes).

	encode_array_values([], []).
	encode_array_values([Value| Values], Bytes) :-
		encode_field_value(Value, ValueBytes),
		encode_array_values(Values, RestBytes),
		append(ValueBytes, RestBytes, Bytes).

	% Decode array
	decode_array(Bytes, Values, Rest) :-
		decode_long(Bytes, Length, After),
		(	Length == 0 ->
			Values = [],
			Rest = After
		;	length(ArrayBytes, Length),
			append(ArrayBytes, Rest, After),
			decode_array_values(ArrayBytes, Values)
		).

	decode_array_values([], []).
	decode_array_values([Byte| Bytes], [Value| Values]) :-
		decode_field_value(Byte, Bytes, Value, After),
		decode_array_values(After, Values).

	% ==========================================================================
	% IEEE 754 Float/Double Encoding and Decoding
	% ==========================================================================

	% Float encoding (IEEE 754 single precision - 32 bits)
	% Format: 1 sign bit + 8 exponent bits (bias 127) + 23 mantissa bits
	encode_float(0.0, [0x00, 0x00, 0x00, 0x00]) :- !.
	encode_float(Value, Bytes) :-
		Value = -0.0,
		!,
		Bytes = [0x80, 0x00, 0x00, 0x00].
	encode_float(Value, Bytes) :-
		(	Value < 0 ->
			Sign = 1,
			AbsValue is abs(Value)
		;	Sign = 0,
			AbsValue = Value
		),
		% Calculate exponent and mantissa
		float_to_exponent_mantissa_32(AbsValue, Exponent, Mantissa),
		% Encode to bytes (big-endian)
		BiasedExp is Exponent + 127,
		Byte3 is (Sign << 7) \/ (BiasedExp >> 1),
		Byte2 is ((BiasedExp /\ 0x01) << 7) \/ ((Mantissa >> 16) /\ 0x7f),
		Byte1 is (Mantissa >> 8) /\ 0xff,
		Byte0 is Mantissa /\ 0xff,
		Bytes = [Byte3, Byte2, Byte1, Byte0].

	float_to_exponent_mantissa_32(Value, Exponent, Mantissa) :-
		% Find exponent: floor(log2(value))
		LogValue is log(Value) / log(2),
		Exponent is floor(LogValue),
		% Normalized mantissa: (value / 2^exponent - 1) * 2^23
		NormalizedMantissa is (Value / (2 ** Exponent) - 1) * 8388608,  % 2^23 = 8388608
		Mantissa is round(NormalizedMantissa) /\ 0x7fffff.

	% Float decoding (IEEE 754 single precision)
	decode_float([0x00, 0x00, 0x00, 0x00| Rest], 0.0, Rest) :- !.
	decode_float([0x80, 0x00, 0x00, 0x00| Rest], NegZero, Rest) :-
		!,
		NegZero is -0.0.
	decode_float([0x7f, 0x80, 0x00, 0x00| Rest], @infinity, Rest) :- !.
	decode_float([0xff, 0x80, 0x00, 0x00| Rest], @negative_infinity, Rest) :- !.
	decode_float([0x7f, 0xc0, 0x00, 0x00| Rest], @not_a_number, Rest) :- !.
	decode_float([Byte3, Byte2, Byte1, Byte0| Rest], Value, Rest) :-
		Sign is Byte3 >> 7,
		Exponent is ((Byte3 /\ 0x7f) << 1) + (Byte2 >> 7),
		Significand is ((Byte2 /\ 0x7f) << 16) + (Byte1 << 8) + Byte0,
		BiasedExp is Exponent - 127,
		(	Exponent =:= 0 ->
			% Denormalized number
			Value is ((-1) ** Sign) * (Significand * (2 ** (-23))) * (2 ** (-126))
		;	% Normalized number
			Value is ((-1) ** Sign) * (1 + Significand * (2 ** (-23))) * (2 ** BiasedExp)
		).

	% Double encoding (IEEE 754 double precision - 64 bits)
	% Format: 1 sign bit + 11 exponent bits (bias 1023) + 52 mantissa bits
	encode_double(0.0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) :- !.
	encode_double(Value, Bytes) :-
		Value = -0.0,
		!,
		Bytes = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00].
	encode_double(Value, Bytes) :-
		(	Value < 0 ->
			Sign = 1,
			AbsValue is abs(Value)
		;	Sign = 0,
			AbsValue = Value
		),
		% Calculate exponent and mantissa
		float_to_exponent_mantissa_64(AbsValue, Exponent, Mantissa),
		% Encode to bytes (big-endian)
		BiasedExp is Exponent + 1023,
		Byte7 is (Sign << 7) \/ (BiasedExp >> 4),
		Byte6 is ((BiasedExp /\ 0x0f) << 4) \/ ((Mantissa >> 48) /\ 0x0f),
		Byte5 is (Mantissa >> 40) /\ 0xff,
		Byte4 is (Mantissa >> 32) /\ 0xff,
		Byte3 is (Mantissa >> 24) /\ 0xff,
		Byte2 is (Mantissa >> 16) /\ 0xff,
		Byte1 is (Mantissa >> 8) /\ 0xff,
		Byte0 is Mantissa /\ 0xff,
		Bytes = [Byte7, Byte6, Byte5, Byte4, Byte3, Byte2, Byte1, Byte0].

	float_to_exponent_mantissa_64(Value, Exponent, Mantissa) :-
		% Find exponent: floor(log2(value))
		LogValue is log(Value) / log(2),
		Exponent is floor(LogValue),
		% Normalized mantissa: (value / 2^exponent - 1) * 2^52
		NormalizedMantissa is (Value / (2 ** Exponent) - 1) * 4503599627370496,  % 2^52
		Mantissa is round(NormalizedMantissa) /\ 0xfffffffffffff.

	% Double decoding (IEEE 754 double precision)
	decode_double([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00| Rest], 0.0, Rest) :- !.
	decode_double([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00| Rest], NegZero, Rest) :-
		!,
		NegZero is -0.0.
	decode_double([0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00| Rest], @infinity, Rest) :- !.
	decode_double([0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00| Rest], @negative_infinity, Rest) :- !.
	decode_double([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00| Rest], @not_a_number, Rest) :- !.
	decode_double([Byte7, Byte6, Byte5, Byte4, Byte3, Byte2, Byte1, Byte0| Rest], Value, Rest) :-
		Sign is Byte7 >> 7,
		Exponent is ((Byte7 /\ 0x7f) << 4) + (Byte6 >> 4),
		Significand is ((Byte6 /\ 0x0f) << 48) + (Byte5 << 40) + (Byte4 << 32) + (Byte3 << 24) + (Byte2 << 16) + (Byte1 << 8) + Byte0,
		BiasedExp is Exponent - 1023,
		(	Exponent =:= 0 ->
			% Denormalized number
			Value is ((-1) ** Sign) * (Significand * (2 ** (-52))) * (2 ** (-1022))
		;	% Normalized number
			Value is ((-1) ** Sign) * (1 + Significand * (2 ** (-52))) * (2 ** BiasedExp)
		).

	% ==========================================================================
	% Method Frame Encoding
	% ==========================================================================

	encode_payload(method, method_payload(Class, Method, Arguments), Bytes) :-
		class_id(Class, ClassId),
		method_id(Class, Method, MethodId),
		encode_short(ClassId, ClassBytes),
		encode_short(MethodId, MethodBytes),
		encode_method_arguments(Class, Method, Arguments, ArgumentsBytes),
		append([ClassBytes, MethodBytes, ArgumentsBytes], Bytes).

	encode_payload(header, header_payload(Class, BodySize, Properties), Bytes) :-
		class_id(Class, ClassId),
		encode_short(ClassId, ClassBytes),
		encode_short(0, WeightBytes),  % Weight always 0
		encode_longlong(BodySize, SizeBytes),
		encode_properties(Properties, PropBytes),
		append([ClassBytes, WeightBytes, SizeBytes, PropBytes], Bytes).

	encode_payload(body, BodyBytes, BodyBytes).

	encode_payload(heartbeat, _, []).

	% Decode payload based on type
	decode_payload(method, Channel, Bytes, frame(method, Channel, Class, Method, Arguments)) :-
		decode_short(Bytes, ClassId, Rest1),
		decode_short(Rest1, MethodId, Rest2),
		class_id(Class, ClassId),
		method_id(Class, Method, MethodId),
		decode_method_arguments(Class, Method, Rest2, Arguments).

	decode_payload(header, Channel, Bytes, frame(header, Channel, Class, BodySize, Properties)) :-
		decode_short(Bytes, ClassId, Rest1),
		decode_short(Rest1, _Weight, Rest2),
		decode_longlong(Rest2, BodySize, Rest3),
		class_id(Class, ClassId),
		decode_properties(Rest3, Properties).

	decode_payload(body, Channel, Bytes, frame(body, Channel, Bytes)).

	decode_payload(heartbeat, _, _, frame(heartbeat, 0, [], [], [])).

	% ==========================================================================
	% Method Argument Encoding/Decoding
	% ==========================================================================

	% Connection.Start-Ok
	encode_method_arguments(connection, start_ok, MethodArguments, Bytes) :-
		member(client_properties-Props, MethodArguments),
		member(mechanism-Mechanism, MethodArguments),
		member(response-Response, MethodArguments),
		member(locale-Locale, MethodArguments),
		encode_table(Props, PropsBytes),
		encode_shortstr(Mechanism, MechBytes),
		encode_longstr(Response, RespBytes),
		encode_shortstr(Locale, LocaleBytes),
		append([PropsBytes, MechBytes, RespBytes, LocaleBytes], Bytes).

	% Connection.Tune-Ok
	encode_method_arguments(connection, tune_ok, MethodArguments, Bytes) :-
		member(channel_max-ChannelMax, MethodArguments),
		member(frame_max-FrameMax, MethodArguments),
		member(heartbeat-Heartbeat, MethodArguments),
		encode_short(ChannelMax, ChanBytes),
		encode_long(FrameMax, FrameBytes),
		encode_short(Heartbeat, HbBytes),
		append([ChanBytes, FrameBytes, HbBytes], Bytes).

	% Connection.Open
	encode_method_arguments(connection, open, MethodArguments, Bytes) :-
		member(virtual_host-VHost, MethodArguments),
		encode_shortstr(VHost, VHostBytes),
		append([VHostBytes, [0, 0]], Bytes).  % reserved fields

	% Connection.Close
	encode_method_arguments(connection, close, MethodArguments, Bytes) :-
		member(reply_code-ReplyCode, MethodArguments),
		member(reply_text-ReplyText, MethodArguments),
		member(class_id-ClassId, MethodArguments),
		member(method_id-MethodId, MethodArguments),
		encode_short(ReplyCode, CodeBytes),
		encode_shortstr(ReplyText, TextBytes),
		encode_short(ClassId, ClassBytes),
		encode_short(MethodId, MethBytes),
		append([CodeBytes, TextBytes, ClassBytes, MethBytes], Bytes).

	% Connection.Close-Ok
	encode_method_arguments(connection, close_ok, _, []).

	% Channel.Open
	encode_method_arguments(channel, open, _, [0]).  % reserved

	% Channel.Close
	encode_method_arguments(channel, close, MethodArguments, Bytes) :-
		member(reply_code-ReplyCode, MethodArguments),
		member(reply_text-ReplyText, MethodArguments),
		member(class_id-ClassId, MethodArguments),
		member(method_id-MethodId, MethodArguments),
		encode_short(ReplyCode, CodeBytes),
		encode_shortstr(ReplyText, TextBytes),
		encode_short(ClassId, ClassBytes),
		encode_short(MethodId, MethBytes),
		append([CodeBytes, TextBytes, ClassBytes, MethBytes], Bytes).

	% Channel.Close-Ok
	encode_method_arguments(channel, close_ok, _, []).

	% Exchange.Declare
	encode_method_arguments(exchange, declare, MethodArguments, Bytes) :-
		member(exchange-Exchange, MethodArguments),
		member(type-Type, MethodArguments),
		member(flags-Flags, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),  % reserved
		encode_shortstr(Exchange, ExchBytes),
		encode_shortstr(Type, TypeBytes),
		encode_octet(Flags, FlagBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, ExchBytes, TypeBytes, FlagBytes, ArgumentsBytes], Bytes).

	% Exchange.Delete
	encode_method_arguments(exchange, delete, MethodArguments, Bytes) :-
		member(exchange-Exchange, MethodArguments),
		member(flags-Flags, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Exchange, ExchBytes),
		encode_octet(Flags, FlagBytes),
		append([TicketBytes, ExchBytes, FlagBytes], Bytes).

	% Exchange.Bind
	encode_method_arguments(exchange, bind, MethodArguments, Bytes) :-
		member(destination-Dest, MethodArguments),
		member(source-Source, MethodArguments),
		member(routing_key-RoutingKey, MethodArguments),
		member(nowait-NoWait, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Dest, DestBytes),
		encode_shortstr(Source, SourceBytes),
		encode_shortstr(RoutingKey, RkBytes),
		encode_octet(NoWait, NwBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, DestBytes, SourceBytes, RkBytes, NwBytes, ArgumentsBytes], Bytes).

	% Exchange.Unbind
	encode_method_arguments(exchange, unbind, MethodArguments, Bytes) :-
		member(destination-Dest, MethodArguments),
		member(source-Source, MethodArguments),
		member(routing_key-RoutingKey, MethodArguments),
		member(nowait-NoWait, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Dest, DestBytes),
		encode_shortstr(Source, SourceBytes),
		encode_shortstr(RoutingKey, RkBytes),
		encode_octet(NoWait, NwBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, DestBytes, SourceBytes, RkBytes, NwBytes, ArgumentsBytes], Bytes).

	% Queue.Declare
	encode_method_arguments(queue, declare, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(flags-Flags, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_octet(Flags, FlagBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, QueueBytes, FlagBytes, ArgumentsBytes], Bytes).

	% Queue.Bind
	encode_method_arguments(queue, bind, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(exchange-Exchange, MethodArguments),
		member(routing_key-RoutingKey, MethodArguments),
		member(nowait-NoWait, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_shortstr(Exchange, ExchBytes),
		encode_shortstr(RoutingKey, RkBytes),
		encode_octet(NoWait, NwBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, QueueBytes, ExchBytes, RkBytes, NwBytes, ArgumentsBytes], Bytes).

	% Queue.Unbind
	encode_method_arguments(queue, unbind, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(exchange-Exchange, MethodArguments),
		member(routing_key-RoutingKey, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_shortstr(Exchange, ExchBytes),
		encode_shortstr(RoutingKey, RkBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, QueueBytes, ExchBytes, RkBytes, ArgumentsBytes], Bytes).

	% Queue.Purge
	encode_method_arguments(queue, purge, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(nowait-NoWait, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_octet(NoWait, NwBytes),
		append([TicketBytes, QueueBytes, NwBytes], Bytes).

	% Queue.Delete
	encode_method_arguments(queue, delete, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(flags-Flags, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_octet(Flags, FlagBytes),
		append([TicketBytes, QueueBytes, FlagBytes], Bytes).

	% Basic.Qos
	encode_method_arguments(basic, qos, MethodArguments, Bytes) :-
		member(prefetch_size-PrefetchSize, MethodArguments),
		member(prefetch_count-PrefetchCount, MethodArguments),
		member(global-Global, MethodArguments),
		encode_long(PrefetchSize, SizeBytes),
		encode_short(PrefetchCount, CountBytes),
		encode_octet(Global, GlobalBytes),
		append([SizeBytes, CountBytes, GlobalBytes], Bytes).

	% Basic.Consume
	encode_method_arguments(basic, consume, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(consumer_tag-ConsumerTag, MethodArguments),
		member(flags-Flags, MethodArguments),
		member(arguments-Arguments, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_shortstr(ConsumerTag, TagBytes),
		encode_octet(Flags, FlagBytes),
		encode_table(Arguments, ArgumentsBytes),
		append([TicketBytes, QueueBytes, TagBytes, FlagBytes, ArgumentsBytes], Bytes).

	% Basic.Cancel
	encode_method_arguments(basic, cancel, MethodArguments, Bytes) :-
		member(consumer_tag-ConsumerTag, MethodArguments),
		member(nowait-NoWait, MethodArguments),
		encode_shortstr(ConsumerTag, TagBytes),
		encode_octet(NoWait, NwBytes),
		append([TagBytes, NwBytes], Bytes).

	% Basic.Publish
	encode_method_arguments(basic, publish, MethodArguments, Bytes) :-
		member(exchange-Exchange, MethodArguments),
		member(routing_key-RoutingKey, MethodArguments),
		member(flags-Flags, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Exchange, ExchBytes),
		encode_shortstr(RoutingKey, RkBytes),
		encode_octet(Flags, FlagBytes),
		append([TicketBytes, ExchBytes, RkBytes, FlagBytes], Bytes).

	% Basic.Get
	encode_method_arguments(basic, get, MethodArguments, Bytes) :-
		member(queue-Queue, MethodArguments),
		member(no_ack-NoAck, MethodArguments),
		encode_short(0, TicketBytes),
		encode_shortstr(Queue, QueueBytes),
		encode_octet(NoAck, NoAckBytes),
		append([TicketBytes, QueueBytes, NoAckBytes], Bytes).

	% Basic.Ack
	encode_method_arguments(basic, ack, MethodArguments, Bytes) :-
		member(delivery_tag-DeliveryTag, MethodArguments),
		member(multiple-Multiple, MethodArguments),
		encode_longlong(DeliveryTag, TagBytes),
		encode_octet(Multiple, MultBytes),
		append([TagBytes, MultBytes], Bytes).

	% Basic.Reject
	encode_method_arguments(basic, reject, MethodArguments, Bytes) :-
		member(delivery_tag-DeliveryTag, MethodArguments),
		member(requeue-Requeue, MethodArguments),
		encode_longlong(DeliveryTag, TagBytes),
		encode_octet(Requeue, ReqBytes),
		append([TagBytes, ReqBytes], Bytes).

	% Basic.Nack (RabbitMQ extension)
	encode_method_arguments(basic, nack, MethodArguments, Bytes) :-
		member(delivery_tag-DeliveryTag, MethodArguments),
		member(flags-Flags, MethodArguments),
		encode_longlong(DeliveryTag, TagBytes),
		encode_octet(Flags, FlagBytes),
		append([TagBytes, FlagBytes], Bytes).

	% Basic.Recover
	encode_method_arguments(basic, recover, MethodArguments, Bytes) :-
		member(requeue-Requeue, MethodArguments),
		encode_octet(Requeue, ReqBytes),
		Bytes = ReqBytes.

	% Tx.Select
	encode_method_arguments(tx, select, _, []).

	% Tx.Commit
	encode_method_arguments(tx, commit, _, []).

	% Tx.Rollback
	encode_method_arguments(tx, rollback, _, []).

	% Confirm.Select
	encode_method_arguments(confirm, select, MethodArguments, Bytes) :-
		member(nowait-NoWait, MethodArguments),
		encode_octet(NoWait, NwBytes),
		Bytes = NwBytes.

	% ==========================================================================
	% Method Argument Decoding
	% ==========================================================================

	% Connection.Start
	decode_method_arguments(connection, start, Bytes, Arguments) :-
		decode_octet(Bytes, VersionMajor, Rest1),
		decode_octet(Rest1, VersionMinor, Rest2),
		decode_table(Rest2, ServerProperties, Rest3),
		decode_longstr(Rest3, Mechanisms, Rest4),
		decode_longstr(Rest4, Locales, _),
		Arguments = [
			version_major-VersionMajor,
			version_minor-VersionMinor,
			server_properties-ServerProperties,
			mechanisms-Mechanisms,
			locales-Locales
		].

	% Connection.Tune
	decode_method_arguments(connection, tune, Bytes, Arguments) :-
		decode_short(Bytes, ChannelMax, Rest1),
		decode_long(Rest1, FrameMax, Rest2),
		decode_short(Rest2, Heartbeat, _),
		Arguments = [
			channel_max-ChannelMax,
			frame_max-FrameMax,
			heartbeat-Heartbeat
		].

	% Connection.Open-Ok
	decode_method_arguments(connection, open_ok, Bytes, Arguments) :-
		decode_shortstr(Bytes, Reserved, _),
		Arguments = [reserved-Reserved].

	% Connection.Close
	decode_method_arguments(connection, close, Bytes, Arguments) :-
		decode_short(Bytes, ReplyCode, Rest1),
		decode_shortstr(Rest1, ReplyText, Rest2),
		decode_short(Rest2, ClassId, Rest3),
		decode_short(Rest3, MethodId, _),
		Arguments = [
			reply_code-ReplyCode,
			reply_text-ReplyText,
			class_id-ClassId,
			method_id-MethodId
		].

	% Connection.Close-Ok
	decode_method_arguments(connection, close_ok, _, []).

	% Channel.Open-Ok
	decode_method_arguments(channel, open_ok, Bytes, Arguments) :-
		decode_longstr(Bytes, Reserved, _),
		Arguments = [reserved-Reserved].

	% Channel.Close
	decode_method_arguments(channel, close, Bytes, Arguments) :-
		decode_short(Bytes, ReplyCode, Rest1),
		decode_shortstr(Rest1, ReplyText, Rest2),
		decode_short(Rest2, ClassId, Rest3),
		decode_short(Rest3, MethodId, _),
		Arguments = [
			reply_code-ReplyCode,
			reply_text-ReplyText,
			class_id-ClassId,
			method_id-MethodId
		].

	% Channel.Close-Ok
	decode_method_arguments(channel, close_ok, _, []).

	% Exchange.Declare-Ok
	decode_method_arguments(exchange, declare_ok, _, []).

	% Exchange.Delete-Ok
	decode_method_arguments(exchange, delete_ok, _, []).

	% Exchange.Bind-Ok
	decode_method_arguments(exchange, bind_ok, _, []).

	% Exchange.Unbind-Ok
	decode_method_arguments(exchange, unbind_ok, _, []).

	% Queue.Declare-Ok
	decode_method_arguments(queue, declare_ok, Bytes, Arguments) :-
		decode_shortstr(Bytes, Queue, Rest1),
		decode_long(Rest1, MessageCount, Rest2),
		decode_long(Rest2, ConsumerCount, _),
		Arguments = [
			queue-Queue,
			message_count-MessageCount,
			consumer_count-ConsumerCount
		].

	% Queue.Bind-Ok
	decode_method_arguments(queue, bind_ok, _, []).

	% Queue.Unbind-Ok
	decode_method_arguments(queue, unbind_ok, _, []).

	% Queue.Purge-Ok
	decode_method_arguments(queue, purge_ok, Bytes, Arguments) :-
		decode_long(Bytes, MessageCount, _),
		Arguments = [message_count-MessageCount].

	% Queue.Delete-Ok
	decode_method_arguments(queue, delete_ok, Bytes, Arguments) :-
		decode_long(Bytes, MessageCount, _),
		Arguments = [message_count-MessageCount].

	% Basic.Qos-Ok
	decode_method_arguments(basic, qos_ok, _, []).

	% Basic.Consume-Ok
	decode_method_arguments(basic, consume_ok, Bytes, Arguments) :-
		decode_shortstr(Bytes, ConsumerTag, _),
		Arguments = [consumer_tag-ConsumerTag].

	% Basic.Cancel-Ok
	decode_method_arguments(basic, cancel_ok, Bytes, Arguments) :-
		decode_shortstr(Bytes, ConsumerTag, _),
		Arguments = [consumer_tag-ConsumerTag].

	% Basic.Deliver
	decode_method_arguments(basic, deliver, Bytes, Arguments) :-
		decode_shortstr(Bytes, ConsumerTag, Rest1),
		decode_longlong(Rest1, DeliveryTag, Rest2),
		decode_octet(Rest2, Redelivered, Rest3),
		decode_shortstr(Rest3, Exchange, Rest4),
		decode_shortstr(Rest4, RoutingKey, _),
		Arguments = [
			consumer_tag-ConsumerTag,
			delivery_tag-DeliveryTag,
			redelivered-Redelivered,
			exchange-Exchange,
			routing_key-RoutingKey
		].

	% Basic.Get-Ok
	decode_method_arguments(basic, get_ok, Bytes, Arguments) :-
		decode_longlong(Bytes, DeliveryTag, Rest1),
		decode_octet(Rest1, Redelivered, Rest2),
		decode_shortstr(Rest2, Exchange, Rest3),
		decode_shortstr(Rest3, RoutingKey, Rest4),
		decode_long(Rest4, MessageCount, _),
		Arguments = [
			delivery_tag-DeliveryTag,
			redelivered-Redelivered,
			exchange-Exchange,
			routing_key-RoutingKey,
			message_count-MessageCount
		].

	% Basic.Get-Empty
	decode_method_arguments(basic, get_empty, Bytes, Arguments) :-
		decode_shortstr(Bytes, ClusterId, _),
		Arguments = [cluster_id-ClusterId].

	% Basic.Return
	decode_method_arguments(basic, return, Bytes, Arguments) :-
		decode_short(Bytes, ReplyCode, Rest1),
		decode_shortstr(Rest1, ReplyText, Rest2),
		decode_shortstr(Rest2, Exchange, Rest3),
		decode_shortstr(Rest3, RoutingKey, _),
		Arguments = [
			reply_code-ReplyCode,
			reply_text-ReplyText,
			exchange-Exchange,
			routing_key-RoutingKey
		].

	% Basic.Recover-Ok
	decode_method_arguments(basic, recover_ok, _, []).

	% Tx.Select-Ok
	decode_method_arguments(tx, select_ok, _, []).

	% Tx.Commit-Ok
	decode_method_arguments(tx, commit_ok, _, []).

	% Tx.Rollback-Ok
	decode_method_arguments(tx, rollback_ok, _, []).

	% Confirm.Select-Ok
	decode_method_arguments(confirm, select_ok, _, []).

	% ==========================================================================
	% Content Properties Encoding/Decoding
	% ==========================================================================

	% Basic class properties bitmap:
	% Bit 15: content-type        Bit 14: content-encoding
	% Bit 13: headers             Bit 12: delivery-mode
	% Bit 11: priority            Bit 10: correlation-id
	% Bit 9:  reply-to            Bit 8:  expiration
	% Bit 7:  message-id          Bit 6:  timestamp
	% Bit 5:  type                Bit 4:  user-id
	% Bit 3:  app-id              Bit 2:  reserved
	% Bit 1:  reserved            Bit 0:  (continuation flag)

	build_content_properties(Options, Properties, Flags) :-
		% Build properties list and calculate flags
		build_property(content_type, Options, Props0, [], 15, Flag0, 0),
		build_property(content_encoding, Options, Props1, Props0, 14, Flag1, Flag0),
		build_property(headers, Options, Props2, Props1, 13, Flag2, Flag1),
		build_property(delivery_mode, Options, Props3, Props2, 12, Flag3, Flag2),
		build_property(priority, Options, Props4, Props3, 11, Flag4, Flag3),
		build_property(correlation_id, Options, Props5, Props4, 10, Flag5, Flag4),
		build_property(reply_to, Options, Props6, Props5, 9, Flag6, Flag5),
		build_property(expiration, Options, Props7, Props6, 8, Flag7, Flag6),
		build_property(message_id, Options, Props8, Props7, 7, Flag8, Flag7),
		build_property(timestamp, Options, Props9, Props8, 6, Flag9, Flag8),
		build_property(type, Options, Props10, Props9, 5, Flag10, Flag9),
		build_property(user_id, Options, Props11, Props10, 4, Flag11, Flag10),
		build_property(app_id, Options, Properties, Props11, 3, Flags, Flag11).

	build_property(Name, Options, [Name-Value| PropsIn], PropsIn, BitPos, FlagOut, FlagIn) :-
		Opt =.. [Name, Value],
		member(Opt, Options),
		!,
		FlagOut is FlagIn \/ (1 << BitPos).
	build_property(_, _, Props, Props, _, Flag, Flag).

	encode_properties(Properties, Bytes) :-
		properties_to_flags(Properties, Flags),
		encode_short(Flags, FlagBytes),
		encode_property_values(Properties, Flags, PropBytes),
		append(FlagBytes, PropBytes, Bytes).

	properties_to_flags(Properties, Flags) :-
		props_flag(content_type, Properties, 15, F0, 0),
		props_flag(content_encoding, Properties, 14, F1, F0),
		props_flag(headers, Properties, 13, F2, F1),
		props_flag(delivery_mode, Properties, 12, F3, F2),
		props_flag(priority, Properties, 11, F4, F3),
		props_flag(correlation_id, Properties, 10, F5, F4),
		props_flag(reply_to, Properties, 9, F6, F5),
		props_flag(expiration, Properties, 8, F7, F6),
		props_flag(message_id, Properties, 7, F8, F7),
		props_flag(timestamp, Properties, 6, F9, F8),
		props_flag(type, Properties, 5, F10, F9),
		props_flag(user_id, Properties, 4, F11, F10),
		props_flag(app_id, Properties, 3, Flags, F11).

	props_flag(Name, Properties, Bit, FlagOut, FlagIn) :-
		(	member(Name-_, Properties) ->
			FlagOut is FlagIn \/ (1 << Bit)
		;	FlagOut = FlagIn
		).

	encode_property_values(Properties, _, Bytes) :-
		% Encode properties in the correct AMQP order (highest bit first)
		encode_prop_if_present(content_type, Properties, B0, []),
		encode_prop_if_present(content_encoding, Properties, B1, B0),
		encode_prop_if_present(headers, Properties, B2, B1),
		encode_prop_if_present(delivery_mode, Properties, B3, B2),
		encode_prop_if_present(priority, Properties, B4, B3),
		encode_prop_if_present(correlation_id, Properties, B5, B4),
		encode_prop_if_present(reply_to, Properties, B6, B5),
		encode_prop_if_present(expiration, Properties, B7, B6),
		encode_prop_if_present(message_id, Properties, B8, B7),
		encode_prop_if_present(timestamp, Properties, B9, B8),
		encode_prop_if_present(type, Properties, B10, B9),
		encode_prop_if_present(user_id, Properties, B11, B10),
		encode_prop_if_present(app_id, Properties, Bytes, B11).

	encode_prop_if_present(Name, Properties, BytesOut, BytesIn) :-
		(	member(Name-Value, Properties) ->
			encode_single_property(Name, Value, PropBytes),
			append(BytesIn, PropBytes, BytesOut)
		;	BytesOut = BytesIn
		).

	encode_single_property(content_type, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(content_encoding, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(headers, V, Bytes) :- encode_table(V, Bytes).
	encode_single_property(delivery_mode, V, Bytes) :- encode_octet(V, Bytes).
	encode_single_property(priority, V, Bytes) :- encode_octet(V, Bytes).
	encode_single_property(correlation_id, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(reply_to, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(expiration, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(message_id, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(timestamp, V, Bytes) :- encode_longlong(V, Bytes).
	encode_single_property(type, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(user_id, V, Bytes) :- encode_shortstr(V, Bytes).
	encode_single_property(app_id, V, Bytes) :- encode_shortstr(V, Bytes).

	decode_properties(Bytes, Properties) :-
		decode_short(Bytes, Flags, Rest),
		decode_property_values(Flags, Rest, Properties).

	decode_property_values(Flags, Bytes, Properties) :-
		decode_property_if_set(Flags, 15, content_type, shortstr, Bytes, Props0, [], Rest0),
		decode_property_if_set(Flags, 14, content_encoding, shortstr, Rest0, Props1, Props0, Rest1),
		decode_property_if_set(Flags, 13, headers, table, Rest1, Props2, Props1, Rest2),
		decode_property_if_set(Flags, 12, delivery_mode, octet, Rest2, Props3, Props2, Rest3),
		decode_property_if_set(Flags, 11, priority, octet, Rest3, Props4, Props3, Rest4),
		decode_property_if_set(Flags, 10, correlation_id, shortstr, Rest4, Props5, Props4, Rest5),
		decode_property_if_set(Flags, 9, reply_to, shortstr, Rest5, Props6, Props5, Rest6),
		decode_property_if_set(Flags, 8, expiration, shortstr, Rest6, Props7, Props6, Rest7),
		decode_property_if_set(Flags, 7, message_id, shortstr, Rest7, Props8, Props7, Rest8),
		decode_property_if_set(Flags, 6, timestamp, longlong, Rest8, Props9, Props8, Rest9),
		decode_property_if_set(Flags, 5, type, shortstr, Rest9, Props10, Props9, Rest10),
		decode_property_if_set(Flags, 4, user_id, shortstr, Rest10, Props11, Props10, Rest11),
		decode_property_if_set(Flags, 3, app_id, shortstr, Rest11, Properties, Props11, _).

	decode_property_if_set(Flags, Bit, Name, Type, Bytes, [Name-Value| PropsIn], PropsIn, Rest) :-
		Flags /\ (1 << Bit) =\= 0,
		!,
		decode_typed_value(Type, Bytes, Value, Rest).
	decode_property_if_set(_, _, _, _, Bytes, Props, Props, Bytes).

	decode_typed_value(shortstr, Bytes, Value, Rest) :- decode_shortstr(Bytes, Value, Rest).
	decode_typed_value(longstr, Bytes, Value, Rest) :- decode_longstr(Bytes, Value, Rest).
	decode_typed_value(octet, Bytes, Value, Rest) :- decode_octet(Bytes, Value, Rest).
	decode_typed_value(short, Bytes, Value, Rest) :- decode_short(Bytes, Value, Rest).
	decode_typed_value(long, Bytes, Value, Rest) :- decode_long(Bytes, Value, Rest).
	decode_typed_value(longlong, Bytes, Value, Rest) :- decode_longlong(Bytes, Value, Rest).
	decode_typed_value(table, Bytes, Value, Rest) :- decode_table(Bytes, Value, Rest).

	% ==========================================================================
	% Frame I/O
	% ==========================================================================

	send_method(Stream, Channel, Class, Method, Arguments) :-
		class_id(Class, ClassId),
		method_id(Class, Method, MethodId),
		encode_short(ClassId, ClassBytes),
		encode_short(MethodId, MethodBytes),
		encode_method_arguments(Class, Method, Arguments, ArgumentsBytes),
		append([ClassBytes, MethodBytes, ArgumentsBytes], PayloadBytes),
		send_frame_bytes(Stream, 1, Channel, PayloadBytes).

	send_content_header(Stream, Channel, BodySize, _PropertyFlags, Properties, _Context) :-
		class_id(basic, ClassId),
		encode_short(ClassId, ClassBytes),
		encode_short(0, WeightBytes),  % Weight always 0
		encode_longlong(BodySize, SizeBytes),
		encode_properties(Properties, PropBytes),
		append([ClassBytes, WeightBytes, SizeBytes, PropBytes], PayloadBytes),
		send_frame_bytes(Stream, 2, Channel, PayloadBytes).

	send_content_body(Stream, Channel, BodyBytes, MaxSize, _Context) :-
		length(BodyBytes, Length),
		(	Length =< MaxSize ->
			send_frame_bytes(Stream, 3, Channel, BodyBytes)
		;	% Split body into multiple frames
			length(Chunk, MaxSize),
			append(Chunk, Rest, BodyBytes),
			send_frame_bytes(Stream, 3, Channel, Chunk),
			send_content_body(Stream, Channel, Rest, MaxSize, _Context)
		).

	send_frame_bytes(Stream, Type, Channel, PayloadBytes) :-
		length(PayloadBytes, PayloadSize),
		encode_short(Channel, ChannelBytes),
		encode_long(PayloadSize, SizeBytes),
		frame_end(FrameEnd),
		append([[Type], ChannelBytes, SizeBytes, PayloadBytes, [FrameEnd]], FrameBytes),
		write_bytes(FrameBytes, Stream),
		flush_output(Stream).

	read_frame(Stream, Frame, Context) :-
		catch(
			read_frame_(Stream, Frame),
			Error,
			throw(error(amqp_error(Error), Context))
		).

	read_frame_(Stream, Frame) :-
		% Read frame header: type(1) + channel(2) + size(4) = 7 bytes
		read_bytes(Stream, 7, HeaderBytes),
		HeaderBytes = [TypeByte| Rest1],
		decode_short(Rest1, Channel, Rest2),
		decode_long(Rest2, PayloadSize, _),
		% Read payload
		read_bytes(Stream, PayloadSize, PayloadBytes),
		% Read frame end marker
		read_bytes(Stream, 1, [FrameEndByte]),
		frame_end(ExpectedEnd),
		(	FrameEndByte == ExpectedEnd ->
			true
		;	throw(protocol_error('Invalid frame end marker'))
		),
		% Decode frame
		frame_type(Type, TypeByte),
		decode_payload(Type, Channel, PayloadBytes, Frame).

	read_content_body(_, 0, [], _) :-
		!.
	read_content_body(Stream, Remaining, BodyBytes, Context) :-
		read_frame(Stream, Frame, Context),
		(	Frame = frame(body, _, ChunkBytes) ->
			length(ChunkBytes, ChunkLength),
			NewRemaining is Remaining - ChunkLength,
			append(ChunkBytes, RestBytes, BodyBytes),
			read_content_body(Stream, NewRemaining, RestBytes, Context)
		;	throw(error(amqp_error(protocol_error('Expected body frame')), Context))
		).

	% ==========================================================================
	% Stream I/O Helpers
	% ==========================================================================

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	read_bytes(_, 0, []) :-
		!.
	read_bytes(Stream, N, [Byte| Bytes]) :-
		N > 0,
		get_byte(Stream, Byte),
		(	Byte == -1 ->
			throw(connection_closed)
		;	N1 is N - 1,
			read_bytes(Stream, N1, Bytes)
		).

	% ==========================================================================
	% Auxiliary Predicates
	% ==========================================================================

	option(Option, Options, _Default) :-
		member(Option, Options),
		!.
	option(Option, _, Default) :-
		Option =.. [_, Default].

	boolean_to_integer(true,  1).
	boolean_to_integer(false, 0).
	boolean_to_integer(1,     1).
	boolean_to_integer(0,     0).

	negotiate_value(0, Server, Server) :- !.
	negotiate_value(Client, 0, Client) :- !.
	negotiate_value(Client, Server, Value) :-
		Value is min(Client, Server).

	negotiate_heartbeat(0, _, 0) :- !.
	negotiate_heartbeat(_, 0, 0) :- !.
	negotiate_heartbeat(Client, Server, Value) :-
		Value is min(Client, Server).

	build_sasl_response(Username, Password, _Mechanisms, Response) :-
		% PLAIN mechanism: \0username\0password
		atom_codes(Username, UserCodes),
		atom_codes(Password, PassCodes),
		append([0| UserCodes], [0| PassCodes], ResponseCodes),
		atom_codes(Response, ResponseCodes).

	body_to_bytes(Body, Bytes) :-
		(	atom(Body) ->
			atom_codes(Body, Bytes)
		;	is_list(Body) ->
			Bytes = Body
		;	% Convert term to atom first
			write_to_codes(Body, Bytes)
		).

:- end_object.
