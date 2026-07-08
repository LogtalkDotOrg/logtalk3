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


:- object(s3_session(_HTTPSocket_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Explicit S3 client sessions carrying default request options.'
	]).

	:- public(get_object/6).
	:- mode(get_object(+compound, +atom, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(get_object/6, [
		comment is 'Fetches an object into a local file path using session defaults merged with per-call options and returns its response metadata as a separate argument.',
		argnames is ['Session', 'Bucket', 'Key', 'File', 'Properties', 'Options'],
		remarks is [
			'Option precedence' - 'Per-call options are merged before the stored session defaults, so explicit call options take precedence.',
			'Option ``credentials/1``' - 'Access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``.',
			'Option ``headers/1``' - 'Optional extra normalized HTTP request headers.',
			'Option ``version/1``' - 'HTTP protocol version as ``http(Major, Minor)``.',
			'Option ``properties/1``' - 'Optional extra normalized HTTP request properties.',
			'Option ``connection_options/1``' - 'Optional transport-specific HTTP client options.',
			'Option ``payload_hash_mode/1``' - 'Payload signing mode, either ``signed`` or ``unsigned``.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` exists but is not a writable file' - domain_error(file([], [write]), 'File'),
			'The directory ``Directory`` exists but is not writable' - domain_error(directory([write]), 'Directory'),
			'The directory ``Directory`` does not exist' - existence_error(directory, 'Directory'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(open/1).
	:- mode(open(-compound), one_or_error).
	:- info(open/1, [
		comment is 'Opens a new S3 client session with no default options.',
		argnames is ['Session'],
		exceptions is []
	]).

	:- public(open/2).
	:- mode(open(-compound, +list(compound)), one_or_error).
	:- info(open/2, [
		comment is 'Opens a new S3 client session with the given default options.',
		argnames is ['Session', 'Options'],
		remarks is [
			'Option role' - '``Options`` stores default request options reused by later session calls.',
			'Option ``credentials/1``' - 'Default access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional default session token for temporary credentials.',
			'Option ``region/1``' - 'Default AWS region to use when ``endpoint/1`` is absent.',
			'Option ``endpoint/1``' - 'Optional default custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Default endpoint addressing style, either ``virtual_hosted`` or ``path``.',
			'Option ``headers/1``' - 'Optional default normalized HTTP request headers.',
			'Option ``version/1``' - 'Default HTTP protocol version as ``http(Major, Minor)``.',
			'Option ``properties/1``' - 'Optional default normalized HTTP request properties.',
			'Option ``connection_options/1``' - 'Optional default transport-specific HTTP client options.',
			'Option ``payload_hash_mode/1``' - 'Default payload signing mode, either ``signed`` or ``unsigned``.',
			'Option ``request_time/1``' - 'Optional default local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'Default UTC offset used to derive the signing time from the local clock.',
			'Option ``amz_date/1``' - 'Optional default AWS SigV4 timestamp overriding any derived request time.'
		],
		exceptions is [
			'``Options`` contains an invalid S3 session option' - domain_error(option, 'Option')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a S3 client session.',
		argnames is ['Session'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session')
		]
	]).

	:- public(list_buckets/3).
	:- mode(list_buckets(+compound, -compound, +list(compound)), one_or_error).
	:- info(list_buckets/3, [
		comment is 'Lists accessible buckets using session defaults merged with per-call options.',
		argnames is ['Session', 'Buckets', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Options`` contains an invalid S3 session option' - domain_error(option, 'Option'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(head_bucket/4).
	:- mode(head_bucket(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(head_bucket/4, [
		comment is 'Returns bucket metadata using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Metadata', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(list_objects_v2/5).
	:- mode(list_objects_v2(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(list_objects_v2/5, [
		comment is 'Lists objects using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Request', 'Listing', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence. The ``Request`` argument carries the ListObjectsV2-specific request terms.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Request`` is a variable' - instantiation_error,
			'An element ``Option`` of the request option list is a variable' - instantiation_error,
			'An element ``Option`` of the request option list is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'``Request`` is not a valid ListObjectsV2 request term or option list' - domain_error(s3_list_objects_request, 'Request'),
			'An element ``Option`` of the request option list is a compound term but not a valid ListObjectsV2 request option' - domain_error(s3_list_objects_request_option, 'Option'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(head_object/5).
	:- mode(head_object(+compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(head_object/5, [
		comment is 'Returns object metadata using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Key', 'Metadata', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(presigned_get_object/5).
	:- mode(presigned_get_object(+compound, +atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_get_object/5, [
		comment is 'Generates a presigned download URL using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` plus ``expires(Seconds)`` for the URL lifetime and ``payload_hash(Hash)`` when ``payload_hash_mode(signed)`` is selected. Presigned URLs default to ``payload_hash_mode(unsigned)``. Options are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_client_option, 'Option')
		]
	]).

	:- public(presigned_put_object/5).
	:- mode(presigned_put_object(+compound, +atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_put_object/5, [
		comment is 'Generates a presigned upload URL using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` plus ``expires(Seconds)`` for the URL lifetime and ``payload_hash(Hash)`` when ``payload_hash_mode(signed)`` is selected. Presigned URLs default to ``payload_hash_mode(unsigned)``. Options are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_client_option, 'Option')
		]
	]).

	:- public(presigned_post_object/5).
	:- mode(presigned_post_object(+compound, +atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_post_object/5, [
		comment is 'Generates a presigned POST URL using session defaults merged with per-call options.',
		argnames is ['Session', 'Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` plus ``expires(Seconds)`` for the URL lifetime and ``payload_hash(Hash)`` when ``payload_hash_mode(signed)`` is selected. Presigned URLs default to ``payload_hash_mode(unsigned)``. Options are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_client_option, 'Option')
		]
	]).

	:- public(put_object/7).
	:- mode(put_object(+compound, +atom, +atom, +atom, -atom, -compound, +list(compound)), one_or_error).
	:- info(put_object/7, [
		comment is 'Uploads a local file path using session defaults merged with per-call options and returns the object ETag and response metadata as separate arguments.',
		argnames is ['Session', 'Bucket', 'Key', 'File', 'ETag', 'Properties', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` exists but is not a readable file' - domain_error(file([], [read]), 'File'),
			'The file ``File`` does not exist' - existence_error(file, 'File'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(delete_object/5).
	:- mode(delete_object(+compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(delete_object/5, [
		comment is 'Deletes an object using session defaults merged with per-call options, returning ``delete_result(Properties)``.',
		argnames is ['Session', 'Bucket', 'Key', 'Result', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(copy_object/6).
	:- mode(copy_object(+compound, +compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(copy_object/6, [
		comment is 'Copies an object using session defaults merged with per-call options, returning ``copy_result(ETag, Properties)``.',
		argnames is ['Session', 'Source', 'Bucket', 'Key', 'Result', 'Options'],
		remarks is [
			'Options' - 'Per-call options accept the same terms as ``open/2`` and are merged before the stored session defaults, so explicit call options take precedence.'
		],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor a valid S3 session handle' - domain_error(s3_session, 'Session'),
			'``Session`` is a valid S3 session handle but not open' - existence_error(s3_session, 'Session'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Source`` is a variable' - instantiation_error,
			'The version identifier in ``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a compound term' - type_error(compound, 'Source'),
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'The version identifier in ``Source`` is neither a variable nor an atom' - type_error(atom, 'VersionId'),
			'``Source`` is a compound term but not a valid copy source term' - domain_error(s3_copy_source, 'Source'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'The version identifier in ``Source`` is an atom but not a valid version identifier' - domain_error(s3_version_id, 'VersionId'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The delegated S3 client request failed' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- private(session_seed_/1).
	:- dynamic(session_seed_/1).
	:- mode(session_seed_(?positive_integer), zero_or_one).
	:- info(session_seed_/1, [
		comment is 'Dynamic counter used to generate fresh session identifiers.',
		argnames is ['SessionId']
	]).

	:- private(session_state_/2).
	:- dynamic(session_state_/2).
	:- mode(session_state_(?positive_integer, ?list(compound)), zero_or_more).
	:- info(session_state_/2, [
		comment is 'Dynamic mapping between session identifiers and their stored default option lists.',
		argnames is ['SessionId', 'Options']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			open_session/2,
			close_session/1,
			current_session_options/2
		]).
	:- endif.

	:- uses(list, [
		append/3, member/2, valid/1 as is_list/1
	]).

	open(Session) :-
		open(Session, []).

	open(Session, Options) :-
		^^check_options(Options),
		open_session(Session, Options).

	close(Session) :-
		context(Context),
		close_session(Session, Context).

	list_buckets(Session, Buckets, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::list_buckets(Buckets, MergedOptions).

	head_bucket(Session, Bucket, Metadata, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::head_bucket(Bucket, Metadata, MergedOptions).

	list_objects_v2(Session, Bucket, Request, Listing, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::list_objects_v2(Bucket, Request, Listing, MergedOptions).

	head_object(Session, Bucket, Key, Metadata, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::head_object(Bucket, Key, Metadata, MergedOptions).

	get_object(Session, Bucket, Key, File, Properties, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::get_object(Bucket, Key, File, Properties, MergedOptions).

	presigned_get_object(Session, Bucket, Key, URL, Options) :-
		context(Context),
		merged_presigned_session_options(Session, Options, MergedOptions, Context),
		s3_client(_HTTPSocket_)::presigned_get_object(Bucket, Key, URL, MergedOptions).

	presigned_put_object(Session, Bucket, Key, URL, Options) :-
		context(Context),
		merged_presigned_session_options(Session, Options, MergedOptions, Context),
		s3_client(_HTTPSocket_)::presigned_put_object(Bucket, Key, URL, MergedOptions).

	presigned_post_object(Session, Bucket, Key, URL, Options) :-
		context(Context),
		merged_presigned_session_options(Session, Options, MergedOptions, Context),
		s3_client(_HTTPSocket_)::presigned_post_object(Bucket, Key, URL, MergedOptions).

	put_object(Session, Bucket, Key, File, ETag, Properties, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::put_object(Bucket, Key, File, ETag, Properties, MergedOptions).

	delete_object(Session, Bucket, Key, Result, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::delete_object(Bucket, Key, Result, MergedOptions).

	copy_object(Session, Source, Bucket, Key, Result, Options) :-
		merged_session_options(Session, Options, MergedOptions),
		s3_client(_HTTPSocket_)::copy_object(Source, Bucket, Key, Result, MergedOptions).

	merged_session_options(Session, Options, MergedOptions) :-
		^^check_options(Options),
		current_session_options(Session, Defaults),
		append(Options, Defaults, MergedOptions).

	merged_presigned_session_options(Session, Options, MergedOptions, Context) :-
		check_presigned_session_options(Options, Options, Context),
		current_session_options(Session, Defaults),
		append(Options, Defaults, MergedOptions).

	check_presigned_session_options(Options, _, Context) :-
		var(Options),
		throw(error(instantiation_error, Context)).
	check_presigned_session_options([Option| Options], AllOptions, Context) :-
		!,
		check_presigned_session_option(Option, Context),
		check_presigned_session_options(Options, AllOptions, Context).
	check_presigned_session_options([], _, _) :-
		!.
	check_presigned_session_options(_, AllOptions, Context) :-
		throw(error(type_error(list, AllOptions), Context)).

	check_presigned_session_option(Option, Context) :-
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	\+ compound(Option) ->
			throw(error(type_error(compound, Option), Context))
		;	valid_option(Option) ->
			true
		;	presigned_session_option(Option) ->
			true
		;	throw(error(domain_error(option, Option), Context))
		).

	presigned_session_option(expires(_)).
	presigned_session_option(payload_hash(_)).

	open_session(Session, Options) :-
		generate_session_id(SessionId),
		assertz(session_state_(SessionId, Options)),
		Session = s3_session(SessionId).

	close_session(Session, Context) :-
		session_id(Session, SessionId),
		(	session_state_(SessionId, _) ->
			retractall(session_state_(SessionId, _))
		;	throw(error(existence_error(s3_session, Session), Context))
		).

	current_session_options(Session, Options) :-
		session_id(Session, SessionId),
		(	session_state_(SessionId, Options) ->
			true
		;	existence_error(s3_session, Session)
		).

	session_id(Session, SessionId) :-
		(	var(Session) ->
			instantiation_error
		;	Session = s3_session(SessionId),
			integer(SessionId),
			SessionId > 0 ->
			true
		;	domain_error(s3_session, Session)
		).

	generate_session_id(SessionId) :-
		(	retract(session_seed_(CurrentId)) ->
			SessionId is CurrentId + 1
		;	SessionId = 1
		),
		assertz(session_seed_(SessionId)).

	valid_option(region(Region)) :-
		valid_non_empty_atom(Region).
	valid_option(credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))) :-
		valid_non_empty_atom(AccessKeyId),
		valid_non_empty_atom(SecretAccessKey).
	valid_option(session_token(Token)) :-
		valid_non_empty_atom(Token).
	valid_option(endpoint(URL)) :-
		valid_endpoint(URL).
	valid_option(addressing_style(AddressingStyle)) :-
		once((AddressingStyle == virtual_hosted; AddressingStyle == path)).
	valid_option(headers(Headers)) :-
		is_list(Headers).
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.
	valid_option(properties(Properties)) :-
		is_list(Properties).
	valid_option(connection_options(ConnectionOptions)) :-
		is_list(ConnectionOptions).
	valid_option(payload_hash_mode(PayloadHashMode)) :-
		once((PayloadHashMode == signed; PayloadHashMode == unsigned)).
	valid_option(request_time(date_time(Year, Month, Day, Hours, Minutes, Seconds))) :-
		integer(Year),
		integer(Month),
		integer(Day),
		integer(Hours),
		integer(Minutes),
		integer(Seconds).
	valid_option(utc_offset_seconds(OffsetSeconds)) :-
		integer(OffsetSeconds).
	valid_option(amz_date(AmzDate)) :-
		atom(AmzDate).

	default_option(region('us-east-1')).
	default_option(addressing_style(virtual_hosted)).
	default_option(headers([])).
	default_option(version(http(1, 1))).
	default_option(properties([])).
	default_option(connection_options([])).
	default_option(payload_hash_mode(signed)).
	default_option(utc_offset_seconds(0)).

	valid_endpoint(URL) :-
		atom(URL),
		url(atom)::parse(URL, Components),
		member(scheme(Scheme), Components),
		once((Scheme == http; Scheme == https)).

	valid_non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

:- end_object.
