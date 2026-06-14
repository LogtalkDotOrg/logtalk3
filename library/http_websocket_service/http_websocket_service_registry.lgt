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


:- object(http_websocket_service_registry).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-11,
		comment is 'Session registry predicates for active WebSocket sessions and queued broadcast delivery.'
	]).

	:- public(open/1).
	:- mode(open(-compound), one).
	:- info(open/1, [
		comment is 'Opens a new empty WebSocket session registry.',
		argnames is ['Registry']
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a session registry and removes all registered sessions and queued messages.',
		argnames is ['Registry'],
		exceptions is [
			'``Registry`` is not an open WebSocket service registry handle' - error
		]
	]).

	:- public(register/2).
	:- mode(register(+compound, -compound), one_or_error).
	:- info(register/2, [
		comment is 'Registers a new active session in the given registry and returns its session identifier.',
		argnames is ['Registry', 'Session'],
		exceptions is [
			'``Registry`` is not an open WebSocket service registry handle' - error
		]
	]).

	:- public(unregister/2).
	:- mode(unregister(+compound, +compound), one_or_error).
	:- info(unregister/2, [
		comment is 'Removes a session from the given registry together with any queued outbound messages.',
		argnames is ['Registry', 'Session'],
		exceptions is [
			'``Registry`` or ``Session`` are not valid registered WebSocket service handles' - error
		]
	]).

	:- public(session/2).
	:- mode(session(+compound, ?compound), zero_or_more).
	:- info(session/2, [
		comment is 'Enumerates active session identifiers registered in the given registry.',
		argnames is ['Registry', 'Session']
	]).

	:- public(sessions/2).
	:- mode(sessions(+compound, -list(compound)), one_or_error).
	:- info(sessions/2, [
		comment is 'Returns the list of active session identifiers registered in the given registry.',
		argnames is ['Registry', 'Sessions'],
		exceptions is [
			'``Registry`` is not an open WebSocket service registry handle' - error
		]
	]).

	:- public(session_count/2).
	:- mode(session_count(+compound, -integer), one_or_error).
	:- info(session_count/2, [
		comment is 'Returns the number of active sessions currently registered in the given registry.',
		argnames is ['Registry', 'Count'],
		exceptions is [
			'``Registry`` is not an open WebSocket service registry handle' - error
		]
	]).

	:- public(send/3).
	:- mode(send(+compound, +compound, +compound), one_or_error).
	:- info(send/3, [
		comment is 'Queues a normalized outbound message for the specified registered session.',
		argnames is ['Registry', 'Session', 'Message'],
		exceptions is [
			'``Registry`` or ``Session`` are invalid, or ``Message`` is not a valid normalized WebSocket message term' - error
		]
	]).

	:- public(broadcast/2).
	:- mode(broadcast(+compound, +compound), one_or_error).
	:- info(broadcast/2, [
		comment is 'Queues a normalized outbound message for all currently registered sessions.',
		argnames is ['Registry', 'Message'],
		exceptions is [
			'``Registry`` is invalid or ``Message`` is not a valid normalized WebSocket message term' - error
		]
	]).

	:- public(broadcast_except/3).
	:- mode(broadcast_except(+compound, +compound, +compound), one_or_error).
	:- info(broadcast_except/3, [
		comment is 'Queues a normalized outbound message for all currently registered sessions except the specified session.',
		argnames is ['Registry', 'Session', 'Message'],
		exceptions is [
			'``Registry`` or ``Session`` are invalid, or ``Message`` is not a valid normalized WebSocket message term' - error
		]
	]).

	:- public(take_pending/3).
	:- mode(take_pending(+compound, +compound, -list(compound)), one_or_error).
	:- info(take_pending/3, [
		comment is 'Removes and returns the queued outbound messages for the specified registered session.',
		argnames is ['Registry', 'Session', 'Messages'],
		exceptions is [
			'``Registry`` or ``Session`` are not valid registered WebSocket service handles' - error
		]
	]).

	:- private(registry_seed_/1).
	:- dynamic(registry_seed_/1).
	:- mode(registry_seed_(?positive_integer), zero_or_one).
	:- info(registry_seed_/1, [
		comment is 'Last allocated registry identifier.',
		argnames is ['RegistryId']
	]).

	:- private(registry_state_/2).
	:- dynamic(registry_state_/2).
	:- mode(registry_state_(?positive_integer, ?non_negative_integer), zero_or_more).
	:- info(registry_state_/2, [
		comment is 'Per-registry next session identifier state.',
		argnames is ['RegistryId', 'NextSessionId']
	]).

	:- private(registry_session_/3).
	:- dynamic(registry_session_/3).
	:- mode(registry_session_(?positive_integer, ?positive_integer, ?list(compound)), zero_or_more).
	:- info(registry_session_/3, [
		comment is 'Registered sessions and their queued outbound messages indexed by registry identifier. Messages are stored in reverse order to make enqueue operations constant time.',
		argnames is ['RegistryId', 'SessionId', 'QueuedMessagesReversed']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			allocate_registry_id/1,
			open_registry/1,
			close_registry/1,
			register_session/2,
			unregister_session/2,
			queue_session_message/3,
			take_pending_messages/3,
			current_sessions/2
		]).
	:- endif.

	:- uses(list, [
		length/2, member/2, reverse/2
	]).

	open(Registry) :-
		open_registry(Registry).

	close(Registry) :-
		close_registry(Registry).

	register(Registry, Session) :-
		register_session(Registry, Session).

	unregister(Registry, Session) :-
		unregister_session(Registry, Session).

	session(Registry, Session) :-
		current_sessions(Registry, Sessions),
		member(Session, Sessions).

	sessions(Registry, Sessions) :-
		current_sessions(Registry, Sessions).

	session_count(Registry, Count) :-
		current_sessions(Registry, Sessions),
		length(Sessions, Count).

	send(Registry, Session, Message) :-
		validate_registry_message(Message),
		queue_session_message(Registry, Session, Message).

	broadcast(Registry, Message) :-
		validate_registry_message(Message),
		current_sessions(Registry, Sessions),
		broadcast_message(Sessions, Registry, Message).

	broadcast_except(Registry, Session, Message) :-
		validate_registry_message(Message),
		current_sessions(Registry, Sessions),
		broadcast_except_message(Sessions, Registry, Session, Message).

	take_pending(Registry, Session, Messages) :-
		take_pending_messages(Registry, Session, Messages).

	validate_registry_message(Message) :-
		(	http_websocket_messages::is_message(Message) ->
			true
		;	domain_error(http_websocket_service_registry_message, Message)
		).

	broadcast_message([], _Registry, _Message).
	broadcast_message([Session| Sessions], Registry, Message) :-
		queue_session_message(Registry, Session, Message),
		broadcast_message(Sessions, Registry, Message).

	broadcast_except_message([], _Registry, _ExceptSession, _Message).
	broadcast_except_message([Session| Sessions], Registry, ExceptSession, Message) :-
		(	Session == ExceptSession ->
			true
		;	queue_session_message(Registry, Session, Message)
		),
		broadcast_except_message(Sessions, Registry, ExceptSession, Message).

	allocate_registry_id(RegistryId) :-
		(	retract(registry_seed_(CurrentRegistryId)) ->
			RegistryId is CurrentRegistryId + 1
		;	RegistryId = 1
		),
		assertz(registry_seed_(RegistryId)).

	open_registry(session_registry(RegistryId)) :-
		allocate_registry_id(RegistryId),
		assertz(registry_state_(RegistryId, 0)).

	close_registry(Registry) :-
		nonvar(Registry),
		Registry = session_registry(RegistryId),
		!,
		(	retract(registry_state_(RegistryId, _NextSessionId)) ->
			retractall(registry_session_(RegistryId, _SessionId, _Messages)),
			true
		;	existence_error(http_websocket_service_registry, session_registry(RegistryId))
		).
	close_registry(Registry) :-
		(	var(Registry) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry, Registry)
		).

	register_session(Registry, websocket_session(RegistryId, SessionId)) :-
		nonvar(Registry),
		Registry = session_registry(RegistryId),
		!,
		(	retract(registry_state_(RegistryId, CurrentSessionId)) ->
			SessionId is CurrentSessionId + 1,
			assertz(registry_state_(RegistryId, SessionId)),
			assertz(registry_session_(RegistryId, SessionId, []))
		;	existence_error(http_websocket_service_registry, session_registry(RegistryId))
		).
	register_session(Registry, _Session) :-
		(	var(Registry) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry, Registry)
		).

	unregister_session(Registry, Session) :-
		nonvar(Registry),
		nonvar(Session),
		Registry = session_registry(RegistryId),
		Session = websocket_session(RegistryId, SessionId),
		!,
		(	retract(registry_session_(RegistryId, SessionId, _Messages)) ->
			true
		;	existence_error(http_websocket_service_registry_session, websocket_session(RegistryId, SessionId))
		).
	unregister_session(Registry, Session) :-
		(	var(Registry) ->
			instantiation_error
		;	var(Session) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry_session, Session)
		).

	queue_session_message(Registry, Session, Message) :-
		nonvar(Registry),
		nonvar(Session),
		Registry = session_registry(RegistryId),
		Session = websocket_session(RegistryId, SessionId),
		!,
		(	retract(registry_session_(RegistryId, SessionId, Messages0)) ->
			assertz(registry_session_(RegistryId, SessionId, [Message| Messages0]))
		;	existence_error(http_websocket_service_registry_session, websocket_session(RegistryId, SessionId))
		).
	queue_session_message(Registry, Session, _Message) :-
		(	var(Registry) ->
			instantiation_error
		;	var(Session) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry_session, Session)
		).

	take_pending_messages(Registry, Session, Messages) :-
		nonvar(Registry),
		nonvar(Session),
		Registry = session_registry(RegistryId),
		Session = websocket_session(RegistryId, SessionId),
		!,
		(	retract(registry_session_(RegistryId, SessionId, Messages0)) ->
			reverse(Messages0, Messages),
			assertz(registry_session_(RegistryId, SessionId, []))
		;	existence_error(http_websocket_service_registry_session, websocket_session(RegistryId, SessionId))
		).
	take_pending_messages(Registry, Session, _Messages) :-
		(	var(Registry) ->
			instantiation_error
		;	var(Session) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry_session, Session)
		).

	current_sessions(Registry, Sessions) :-
		nonvar(Registry),
		Registry = session_registry(RegistryId),
		!,
		(	registry_state_(RegistryId, _NextSessionId) ->
			findall(websocket_session(RegistryId, SessionId), registry_session_(RegistryId, SessionId, _Messages), Sessions)
		;	existence_error(http_websocket_service_registry, session_registry(RegistryId))
		).
	current_sessions(Registry, _Sessions) :-
		(	var(Registry) ->
			instantiation_error
		;	domain_error(http_websocket_service_registry, Registry)
		).

:- end_object.
