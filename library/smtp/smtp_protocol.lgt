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


:- protocol(smtp_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-10,
		comment is 'Protocol for portable SMTP clients.'
	]).

	:- public(send/5).
	:- mode(send(+atom, +integer, +compound, --compound, +list), one_or_error).
	:- info(send/5, [
		comment is 'Opens an SMTP session, sends a message, and closes the session.',
		argnames is ['Host', 'Port', 'Message', 'Result', 'Options'],
		exceptions is [
			'``Host`` or ``Port`` is a variable' - instantiation_error,
			'``Host`` is neither a variable nor an atom' - type_error(atom, 'Host'),
			'``Port`` is neither a variable nor an integer' - type_error(integer, 'Port'),
			'``Port`` is an integer but not positive' - domain_error(positive_integer, 'Port'),
			'``Message`` is not an SMTP message term' - domain_error(smtp_message, 'Message'),
			'The envelope sender or a recipient is a variable' - instantiation_error,
			'The envelope sender or a recipient is neither a variable nor an atom' - type_error(atom, 'Mailbox'),
			'The envelope sender or a recipient is not a valid mailbox' - domain_error(smtp_mailbox, 'Mailbox'),
			'``Recipients`` is not a non-empty list of mailboxes or a mailbox atom' - domain_error(smtp_recipients, 'Recipients'),
			'``Headers`` is not a list' - type_error(list, 'Headers'),
			'A header name or value is a variable' - instantiation_error,
			'A header name or value is neither a variable nor an atom' - type_error(atom, 'HeaderComponent'),
			'``Header`` is not a valid header pair' - domain_error(smtp_header, 'Header'),
			'``Body`` is not a supported text representation' - domain_error(smtp_body, 'Body'),
			'A caller-supplied MIME header conflicts with UTF-8 Base64 serialization or duplicates another MIME header' - domain_error(smtp_mime_header, 'Header'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The SMTP connection cannot be opened' - smtp_error(connection_failed),
			'OpenSSL terminates the secure connection while an SMTP response is expected' - smtp_error(secure_connection_failed('Status', 'Diagnostic')),
			'The SMTP server closes the connection while a response is expected at ``Stage``' - smtp_error(connection_closed('Stage')),
			'The SMTP server rejects the session greeting' - smtp_error(greeting_failed('Code', 'Lines')),
			'The SMTP server returns an invalid or unexpected response' - smtp_error(protocol_error('Response')),
			'Authentication is requested over an insecure connection without explicit permission' - permission_error(authenticate, insecure_smtp_connection, plain),
			'The SMTP server does not advertise a supported authentication mechanism' - smtp_error(authentication_not_supported),
			'The SMTP server rejects authentication' - smtp_error(auth_failed('Code', 'Lines')),
			'The SMTP server rejects the envelope sender' - smtp_error(sender_rejected('Code', 'Lines')),
			'The SMTP server rejects the DATA command or the submitted message' - smtp_error(send_failed('Code', 'Lines'))
		]
	]).

	:- public(connect/4).
	:- mode(connect(+atom, +integer, --compound, +list), one_or_error).
	:- info(connect/4, [
		comment is 'Opens an SMTP session for sending one or more messages.',
		argnames is ['Host', 'Port', 'Connection', 'Options'],
		exceptions is [
			'``Host`` or ``Port`` is a variable' - instantiation_error,
			'``Host`` is neither a variable nor an atom' - type_error(atom, 'Host'),
			'``Port`` is neither a variable nor an integer' - type_error(integer, 'Port'),
			'``Port`` is an integer but not positive' - domain_error(positive_integer, 'Port'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Option`` is valid but is not a connection option' - domain_error(smtp_connection_option, 'Option'),
			'The SMTP connection cannot be opened' - smtp_error(connection_failed),
			'OpenSSL terminates the secure connection while an SMTP response is expected' - smtp_error(secure_connection_failed('Status', 'Diagnostic')),
			'The SMTP server closes the connection while a response is expected at ``Stage``' - smtp_error(connection_closed('Stage')),
			'The SMTP server rejects the session greeting' - smtp_error(greeting_failed('Code', 'Lines')),
			'The SMTP server returns an invalid or unexpected response' - smtp_error(protocol_error('Response')),
			'Authentication is requested over an insecure connection without explicit permission' - permission_error(authenticate, insecure_smtp_connection, plain),
			'The SMTP server does not advertise a supported authentication mechanism' - smtp_error(authentication_not_supported),
			'The SMTP server rejects authentication' - smtp_error(auth_failed('Code', 'Lines'))
		]
	]).

	:- public(disconnect/1).
	:- mode(disconnect(+compound), one_or_error).
	:- info(disconnect/1, [
		comment is 'Gracefully closes an SMTP session.',
		argnames is ['Connection'],
		exceptions is [
			'``Connection`` is not an open SMTP connection handle' - domain_error(smtp_connection, 'Connection')
		]
	]).

	:- public(connection_alive/1).
	:- mode(connection_alive(+compound), zero_or_one).
	:- info(connection_alive/1, [
		comment is 'Checks if the local streams for an SMTP connection are open.',
		argnames is ['Connection']
	]).

	:- public(send/4).
	:- mode(send(+compound, +compound, --compound, +list), one_or_error).
	:- info(send/4, [
		comment is 'Sends a message over an open SMTP session.',
		argnames is ['Connection', 'Message', 'Result', 'Options'],
		exceptions is [
			'``Connection`` is not an open SMTP connection handle' - domain_error(smtp_connection, 'Connection'),
			'``Message`` is not an SMTP message term' - domain_error(smtp_message, 'Message'),
			'The envelope sender or a recipient is a variable' - instantiation_error,
			'The envelope sender or a recipient is neither a variable nor an atom' - type_error(atom, 'Mailbox'),
			'The envelope sender or a recipient is not a valid mailbox' - domain_error(smtp_mailbox, 'Mailbox'),
			'``Recipients`` is not a non-empty list of mailboxes or a mailbox atom' - domain_error(smtp_recipients, 'Recipients'),
			'``Headers`` is not a list' - type_error(list, 'Headers'),
			'A header name or value is a variable' - instantiation_error,
			'A header name or value is neither a variable nor an atom' - type_error(atom, 'HeaderComponent'),
			'``Header`` is not a valid header pair' - domain_error(smtp_header, 'Header'),
			'``Body`` is not a supported text representation' - domain_error(smtp_body, 'Body'),
			'A caller-supplied MIME header conflicts with UTF-8 Base64 serialization or duplicates another MIME header' - domain_error(smtp_mime_header, 'Header'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Option`` is valid but is not a transaction option' - domain_error(smtp_transaction_option, 'Option'),
			'The SMTP server closes the connection while a response is expected at ``Stage``' - smtp_error(connection_closed('Stage')),
			'The SMTP server returns an invalid or unexpected response' - smtp_error(protocol_error('Response')),
			'The SMTP server rejects the envelope sender' - smtp_error(sender_rejected('Code', 'Lines')),
			'The SMTP server rejects the DATA command or the submitted message' - smtp_error(send_failed('Code', 'Lines'))
		]
	]).

:- end_protocol.
