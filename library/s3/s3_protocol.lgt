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


:- protocol(s3_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-29,
		comment is 'Extracted protocol entity'
	]).

	:- public(list_buckets/2).
	:- mode(list_buckets(-compound, +list(compound)), one_or_error).
	:- info(list_buckets/2, [
		comment is 'Lists accessible buckets.',
		argnames is ['Buckets', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent. Defaults to ``us-east-1``.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``. Defaults to ``virtual_hosted``.',
			'Option ``headers/1``' - 'Optional extra normalized HTTP request headers.',
			'Option ``version/1``' - 'HTTP protocol version as ``http(Major, Minor)``. Defaults to ``http(1,1)``.',
			'Option ``properties/1``' - 'Optional extra normalized HTTP request properties.',
			'Option ``connection_options/1``' - 'Optional transport-specific HTTP client options.',
			'Option ``payload_hash_mode/1``' - 'Payload signing mode, either ``signed`` or ``unsigned``. Defaults to ``signed``.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.'
		],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'Credentials are missing or invalid' - domain_error(s3_credentials, missing),
			'The S3 service returned a non-success status or malformed XML body' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(head_bucket/3).
	:- mode(head_bucket(+atom, -compound, +list(compound)), one_or_error).
	:- info(head_bucket/3, [
		comment is 'Returns response metadata for a bucket.',
		argnames is ['Bucket', 'Metadata', 'Options'],
		remarks is [
			'Options' - 'Supported options are the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(list_objects_v2/4).
	:- mode(list_objects_v2(+atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(list_objects_v2/4, [
		comment is 'Lists bucket objects using the ListObjectsV2 API.',
		argnames is ['Bucket', 'Request', 'Listing', 'Options'],
		remarks is [
			'Options' - '``Options`` accepts the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``. The ``Request`` argument carries the ListObjectsV2-specific request options.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Request`` is a variable' - instantiation_error,
			'An element ``Option`` of the request option list is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'An element ``Option`` of the request option list is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Request`` is not a valid ListObjectsV2 request term or option list' - domain_error(s3_list_objects_request, 'Request'),
			'An element ``Option`` of the request option list is a compound term but not a valid ListObjectsV2 request option' - domain_error(s3_list_objects_request_option, 'Option'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status or malformed XML body' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(head_object/4).
	:- mode(head_object(+atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(head_object/4, [
		comment is 'Returns response metadata for an object.',
		argnames is ['Bucket', 'Key', 'Metadata', 'Options'],
		remarks is [
			'Options' - 'Supported options are the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(get_object/5).
	:- mode(get_object(+atom, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(get_object/5, [
		comment is 'Fetches an object into a local file path and returns its response metadata as a separate argument.',
		argnames is ['Bucket', 'Key', 'File', 'Properties', 'Options'],
		remarks is [
			'Options' - 'Supported options are: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))`` for access credentials; ``session_token(Token)`` for temporary credentials; ``region(Region)`` for the AWS region, defaulting to ``us-east-1`` when ``endpoint/1`` is absent; ``endpoint(URL)`` for a custom S3-compatible endpoint; ``addressing_style(virtual_hosted|path)`` for endpoint addressing, defaulting to ``virtual_hosted``; ``headers(Headers)`` for extra normalized request headers; ``version(http(Major, Minor))`` for the HTTP protocol version, defaulting to ``http(1,1)``; ``properties(Properties)`` for extra normalized HTTP request properties; ``connection_options(ConnectionOptions)`` for transport-specific HTTP client options; ``payload_hash_mode(signed|unsigned)`` for payload signing, defaulting to ``signed``; ``request_time(date_time(...))`` for an explicit local request time; ``utc_offset_seconds(OffsetSeconds)`` for deriving UTC from the local clock, defaulting to ``0``; and ``amz_date(AmzDate)`` for an explicit AWS SigV4 timestamp overriding time derivation.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``File`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``File`` exists but is not a writable file' - domain_error(file([], [write]), 'File'),
			'The directory ``Directory`` exists but is not writable' - domain_error(directory([write]), 'Directory'),
			'The directory ``Directory`` does not exist' - existence_error(directory, 'Directory'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(put_object/6).
	:- mode(put_object(+atom, +atom, +atom, -atom, -compound, +list(compound)), one_or_error).
	:- info(put_object/6, [
		comment is 'Uploads a local file path and returns the object ETag and response metadata as separate arguments.',
		argnames is ['Bucket', 'Key', 'File', 'ETag', 'Properties', 'Options'],
		remarks is [
			'Options' - 'Supported options are the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``File`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``File`` exists but is not a readable file' - domain_error(file([], [read]), 'File'),
			'The file ``File`` does not exist' - existence_error(file, 'File'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(delete_object/4).
	:- mode(delete_object(+atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(delete_object/4, [
		comment is 'Deletes an object, returning ``delete_result(Properties)``.',
		argnames is ['Bucket', 'Key', 'Result', 'Options'],
		remarks is [
			'Options' - 'Supported options are the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(copy_object/5).
	:- mode(copy_object(+compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(copy_object/5, [
		comment is 'Copies an object from a source bucket/key into the destination bucket/key, returning ``copy_result(ETag, Properties)``.',
		argnames is ['Source', 'Bucket', 'Key', 'Result', 'Options'],
		remarks is [
			'Options' - 'Supported options are the standard S3 request options: ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``, optional ``session_token(Token)``, ``region(Region)``, optional custom ``endpoint(URL)``, ``addressing_style(virtual_hosted|path)``, optional ``headers(Headers)``, ``version(http(Major, Minor))``, optional ``properties(Properties)``, optional ``connection_options(ConnectionOptions)``, ``payload_hash_mode(signed|unsigned)``, optional ``request_time(date_time(...))``, ``utc_offset_seconds(OffsetSeconds)``, and optional explicit ``amz_date(AmzDate)``.'
		],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'The version identifier in ``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a compound term' - type_error(compound, 'Source'),
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'The version identifier in ``Source`` is neither a variable nor an atom' - type_error(atom, 'VersionId'),
			'``Source`` is a compound term but not a valid copy source term' - domain_error(s3_copy_source, 'Source'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'The version identifier in ``Source`` is an atom but not a valid version identifier' - domain_error(s3_version_id, 'VersionId'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'The S3 service returned a non-success status or malformed XML body' - domain_error(s3_http_status, 'Status')
		]
	]).

	:- public(presigned_get_object/4).
	:- mode(presigned_get_object(+atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_get_object/4, [
		comment is 'Generates a presigned URL for downloading an object with an external HTTP client.',
		argnames is ['Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent. Defaults to ``us-east-1``.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``. Defaults to ``virtual_hosted``.',
			'Option ``headers/1``' - 'Optional extra normalized request headers. When present they are signed into the URL and must be reproduced by the eventual caller.',
			'Option ``payload_hash_mode/1``' - 'Presigned URLs default to ``payload_hash_mode(unsigned)``. Use ``signed`` together with ``payload_hash/1`` to sign a precomputed payload hash.',
			'Option ``payload_hash/1``' - 'Precomputed hexadecimal SHA-256 payload hash required when ``payload_hash_mode(signed)`` is selected.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.',
			'Option ``expires/1``' - 'Presigned URL lifetime in seconds. Defaults to ``900`` and must not exceed ``604800``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'Credentials are missing or invalid' - domain_error(s3_credentials, missing),
			'The expiration is invalid or outside the AWS SigV4 range' - domain_error(s3_presign_expires, 'Expires'),
			'The payload hash is missing, invalid, or incompatible with unsigned mode' - domain_error(s3_presign_payload_hash, 'Hash')
		]
	]).

	:- public(presigned_put_object/4).
	:- mode(presigned_put_object(+atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_put_object/4, [
		comment is 'Generates a presigned URL for uploading an object with an external HTTP client.',
		argnames is ['Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent. Defaults to ``us-east-1``.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``. Defaults to ``virtual_hosted``.',
			'Option ``headers/1``' - 'Optional extra normalized request headers. When present they are signed into the URL and must be reproduced by the eventual caller.',
			'Option ``payload_hash_mode/1``' - 'Presigned URLs default to ``payload_hash_mode(unsigned)``. Use ``signed`` together with ``payload_hash/1`` to sign a precomputed payload hash.',
			'Option ``payload_hash/1``' - 'Precomputed hexadecimal SHA-256 payload hash required when ``payload_hash_mode(signed)`` is selected.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.',
			'Option ``expires/1``' - 'Presigned URL lifetime in seconds. Defaults to ``900`` and must not exceed ``604800``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'Credentials are missing or invalid' - domain_error(s3_credentials, missing),
			'The expiration is invalid or outside the AWS SigV4 range' - domain_error(s3_presign_expires, 'Expires'),
			'The payload hash is missing, invalid, or incompatible with unsigned mode' - domain_error(s3_presign_payload_hash, 'Hash')
		]
	]).

	:- public(presigned_post_object/4).
	:- mode(presigned_post_object(+atom, +atom, -atom, +list(compound)), one_or_error).
	:- info(presigned_post_object/4, [
		comment is 'Generates a presigned URL for a POST request to an object with an external HTTP client.',
		argnames is ['Bucket', 'Key', 'URL', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent. Defaults to ``us-east-1``.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible endpoint URL.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``. Defaults to ``virtual_hosted``.',
			'Option ``headers/1``' - 'Optional extra normalized request headers. When present they are signed into the URL and must be reproduced by the eventual caller.',
			'Option ``payload_hash_mode/1``' - 'Presigned URLs default to ``payload_hash_mode(unsigned)``. Use ``signed`` together with ``payload_hash/1`` to sign a precomputed payload hash.',
			'Option ``payload_hash/1``' - 'Precomputed hexadecimal SHA-256 payload hash required when ``payload_hash_mode(signed)`` is selected.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.',
			'Option ``expires/1``' - 'Presigned URL lifetime in seconds. Defaults to ``900`` and must not exceed ``604800``.'
		],
		exceptions is [
			'``Bucket`` is a variable' - instantiation_error,
			'``Key`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable nor an atom' - type_error(atom, 'Bucket'),
			'``Key`` is neither a variable nor an atom' - type_error(atom, 'Key'),
			'``Bucket`` is an atom but not a valid S3 bucket' - domain_error(s3_bucket, 'Bucket'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid S3 client option' - domain_error(s3_client_option, 'Option'),
			'Credentials are missing or invalid' - domain_error(s3_credentials, missing),
			'The expiration is invalid or outside the AWS SigV4 range' - domain_error(s3_presign_expires, 'Expires'),
			'The payload hash is missing, invalid, or incompatible with unsigned mode' - domain_error(s3_presign_payload_hash, 'Hash')
		]
	]).

:- end_protocol.
