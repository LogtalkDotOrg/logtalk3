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


:- object(s3_endpoints,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Endpoint and canonical-URI helpers for S3-compatible services.'
	]).

	:- public(request/6).
	:- mode(request(+atom, @atom, @atom, -atom, -compound, +list(compound)), one_or_error).
	:- info(request/6, [
		comment is 'Builds the request base URL plus signing endpoint context for an S3 operation.',
		argnames is ['Operation', 'Bucket', 'Key', 'URL', 'Endpoint', 'Options'],
		remarks is [
			'Option ``region/1``' - 'AWS region to use when ``endpoint/1`` is absent. Defaults to ``us-east-1``.',
			'Option ``endpoint/1``' - 'Optional custom S3-compatible base endpoint URL, which may include a path prefix.',
			'Option ``addressing_style/1``' - 'Endpoint addressing style, either ``virtual_hosted`` or ``path``. Defaults to ``virtual_hosted``.'
		],
		exceptions is [
			'``Operation`` is not a supported S3 operation' - domain_error(s3_operation, 'Operation'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable not a valid bucket' - domain_error(s3_request, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable not a valid key' - domain_error(s3_request, 'Key'),
			'``Options`` contains an invalid endpoint or addressing style' - domain_error(s3_endpoint, 'Endpoint')
		]
	]).

	:- public(canonical_uri/4).
	:- mode(canonical_uri(+atom, @atom, @atom, -atom), one_or_error).
	:- info(canonical_uri/4, [
		comment is 'Returns the canonical request URI for an S3 operation using the selected endpoint style.',
		argnames is ['Operation', 'Bucket', 'Key', 'CanonicalURI'],
		exceptions is [
			'``Operation`` is not a supported S3 operation' - domain_error(s3_operation, 'Operation'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` is neither a variable not a valid bucket' - domain_error(s3_request, 'Bucket'),
			'``Key`` is a variable' - instantiation_error,
			'``Key`` is neither a variable not a valid key' - domain_error(s3_request, 'Key')
		]
	]).

	:- uses(list, [
		member/2, reverse/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	request(Operation, Bucket, Key, URL, endpoint(HostHeader, CanonicalURI), Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		endpoint_context(MergedOptions, Operation, Bucket, Scheme, HostHeader, BasePath, AddressingStyle),
		canonical_uri(Operation, endpoint_context(BasePath, AddressingStyle), Bucket, Key, CanonicalURI),
		atomic_list_concat([Scheme, '://', HostHeader, CanonicalURI], URL).

	canonical_uri(Operation, Bucket, Key, CanonicalURI) :-
		canonical_uri(Operation, endpoint_context('', virtual_hosted), Bucket, Key, CanonicalURI).

	:- public(canonical_uri/5).
	:- mode(canonical_uri(+atom, +compound, @atom, @atom, -atom), one_or_error).
	:- info(canonical_uri/5, [
		comment is 'Returns the canonical request URI for an S3 operation using an explicit endpoint context.',
		argnames is ['Operation', 'Context', 'Bucket', 'Key', 'CanonicalURI'],
		exceptions is [
			'``Operation`` is not a supported S3 operation' - domain_error(s3_operation, 'Operation'),
			'``Context`` is not a valid endpoint context' - domain_error(s3_endpoint, 'Context'),
			'``Bucket`` is a variable' - instantiation_error,
			'``Bucket`` or ``Key`` is invalid for the operation' - domain_error(s3_request, 'Argument')
		]
	]).

	canonical_uri(Operation, endpoint_context(BasePath, AddressingStyle), Bucket, Key, CanonicalURI) :-
		operation_bucket_required(Operation, BucketRequired),
		operation_key_required(Operation, KeyRequired),
		validate_operation(Operation),
		validate_bucket_requirement(BucketRequired, Bucket),
		validate_key_requirement(KeyRequired, Key),
		normalize_base_path(BasePath, NormalizedBasePath),
		path_segments(AddressingStyle, Bucket, Key, Segments),
		encoded_segments(Segments, EncodedSegments),
		canonical_path(NormalizedBasePath, EncodedSegments, CanonicalURI).

	endpoint_context(Options, Operation, Bucket, Scheme, HostHeader, BasePath, AddressingStyle) :-
		^^option(addressing_style(AddressingStyle), Options),
		(	^^option(endpoint(Endpoint), Options) ->
			custom_endpoint_context(Endpoint, Operation, Bucket, AddressingStyle, Scheme, HostHeader, BasePath)
		;	^^option(region(Region), Options),
			aws_endpoint_context(Region, Operation, Bucket, AddressingStyle, Scheme, HostHeader),
			BasePath = ''
		).

	custom_endpoint_context(Endpoint, Operation, Bucket, AddressingStyle, Scheme, HostHeader, BasePath) :-
		url(atom)::parse(Endpoint, Components),
		member(scheme(Scheme), Components),
		(Scheme == http ; Scheme == https),
		member(authority(Authority), Components),
		!,
		(	member(path(Path), Components) ->
			BasePath = Path
		;	BasePath = ''
		),
		resolve_custom_host_header(Operation, AddressingStyle, Bucket, Authority, HostHeader).
	custom_endpoint_context(Endpoint, _Operation, _Bucket, _AddressingStyle, _Scheme, _HostHeader, _BasePath) :-
		domain_error(s3_endpoint, Endpoint).

	resolve_custom_host_header(list_buckets, _AddressingStyle, _Bucket, Authority, Authority) :-
		!.
	resolve_custom_host_header(_Operation, path, _Bucket, Authority, Authority).
	resolve_custom_host_header(_Operation, virtual_hosted, Bucket, Authority, HostHeader) :-
		validate_bucket_requirement(true, Bucket),
		atomic_list_concat([Bucket, Authority], '.', HostHeader).

	aws_endpoint_context(Region, Operation, Bucket, AddressingStyle, https, HostHeader) :-
		aws_base_host(Region, Operation, BaseHost),
		resolve_aws_host_header(Operation, AddressingStyle, Bucket, BaseHost, HostHeader).

	aws_base_host(Region, list_buckets, Host) :-
		!,
		atomic_list_concat(['s3', Region, 'amazonaws.com'], '.', Host).
	aws_base_host(Region, _Operation, Host) :-
		atomic_list_concat(['s3', Region, 'amazonaws.com'], '.', Host).

	resolve_aws_host_header(list_buckets, _AddressingStyle, _Bucket, BaseHost, BaseHost) :-
		!.
	resolve_aws_host_header(_Operation, path, _Bucket, BaseHost, BaseHost).
	resolve_aws_host_header(_Operation, virtual_hosted, Bucket, BaseHost, HostHeader) :-
		validate_bucket_requirement(true, Bucket),
		atomic_list_concat([Bucket, BaseHost], '.', HostHeader).

	path_segments(virtual_hosted, _Bucket, Key, Segments) :-
		key_segments(Key, Segments).
	path_segments(path, Bucket, Key, Segments) :-
		bucket_key_segments(Bucket, Key, Segments).

	key_segments(Key, []) :-
		(	var(Key) ; Key == none ),
		!.
	key_segments(Key, Segments) :-
		atom(Key),
		!,
		atom_codes(Key, Codes),
		split_codes(Codes, 0'/, Segments).
	key_segments(Key, _Segments) :-
		type_error(atom, Key).

	bucket_key_segments(Bucket, Key, Segments) :-
		(	var(Bucket) ->
			Segments = []
		;	Bucket == none ->
			Segments = []
		;	Segments = [Bucket| KeySegments],
			key_segments(Key, KeySegments)
		).

	split_codes([], _Separator, ['']).
	split_codes(Codes, Separator, Segments) :-
		split_codes(Codes, Separator, [], [], ReversedSegments),
		reverse(ReversedSegments, Segments).

	split_codes([], _Separator, Current0, Segments0, [Segment| Segments0]) :-
		reverse(Current0, CurrentCodes),
		atom_codes(Segment, CurrentCodes).
	split_codes([Separator| Codes], Separator, Current0, Segments0, Segments) :-
		reverse(Current0, CurrentCodes),
		atom_codes(Segment, CurrentCodes),
		split_codes(Codes, Separator, [], [Segment| Segments0], Segments).
	split_codes([Code| Codes], Separator, Current0, Segments0, Segments) :-
		split_codes(Codes, Separator, [Code| Current0], Segments0, Segments).

	encoded_segments([], []).
	encoded_segments([Segment| Segments], [EncodedSegment| EncodedSegments]) :-
		encode_segment(Segment, EncodedSegment),
		encoded_segments(Segments, EncodedSegments).

	encode_segment(Segment, Encoded) :-
		atom_codes(Segment, Codes),
		encode_segment_codes(Codes, EncodedCodes),
		atom_codes(Encoded, EncodedCodes).

	encode_segment_codes([], []).
	encode_segment_codes([Code| Codes], EncodedCodes) :-
		(	unreserved_code(Code) ->
			EncodedCodes = [Code| Rest]
		;	percent_encoded_byte(Code, High, Low),
			EncodedCodes = [0'%, High, Low| Rest]
		),
		encode_segment_codes(Codes, Rest).

	canonical_path('', [], '/').
	canonical_path('', Segments, Path) :-
		Segments \== [],
		!,
		atomic_list_concat(Segments, '/', Joined),
		atomic_list_concat(['/', Joined], Path).
	canonical_path(BasePath, [], BasePath).
	canonical_path(BasePath, Segments, Path) :-
		atomic_list_concat(Segments, '/', Joined),
		(	BasePath == '/' ->
			atomic_list_concat(['/', Joined], Path)
		;	atomic_list_concat([BasePath, '/', Joined], Path)
		).

	normalize_base_path('', '') :-
		!.
	normalize_base_path('/', '/') :-
		!.
	normalize_base_path(BasePath, NormalizedBasePath) :-
		atom(BasePath),
		ensure_leading_slash(BasePath, WithSlash),
		strip_trailing_slash(WithSlash, NormalizedBasePath).

	ensure_leading_slash(Path, Path) :-
		sub_atom(Path, 0, 1, _, '/'),
		!.
	ensure_leading_slash(Path, WithSlash) :-
		atom_concat('/', Path, WithSlash).

	strip_trailing_slash(Path, Path) :-
		Path == '/',
		!.
	strip_trailing_slash(Path, Stripped) :-
		sub_atom(Path, _, 1, 0, '/'),
		!,
		sub_atom(Path, 0, _, 1, Stripped).
	strip_trailing_slash(Path, Path).

	operation_bucket_required(list_buckets, false).
	operation_bucket_required(head_bucket, true).
	operation_bucket_required(list_objects_v2, true).
	operation_bucket_required(head_object, true).
	operation_bucket_required(get_object, true).
	operation_bucket_required(put_object, true).
	operation_bucket_required(post_object, true).
	operation_bucket_required(delete_object, true).
	operation_bucket_required(copy_object, true).

	operation_key_required(list_buckets, false).
	operation_key_required(head_bucket, false).
	operation_key_required(list_objects_v2, false).
	operation_key_required(head_object, true).
	operation_key_required(get_object, true).
	operation_key_required(put_object, true).
	operation_key_required(post_object, true).
	operation_key_required(delete_object, true).
	operation_key_required(copy_object, true).

	validate_operation(Operation) :-
		(	operation_bucket_required(Operation, _),
			operation_key_required(Operation, _) ->
			true
		;	domain_error(s3_operation, Operation)
		).

	validate_bucket_requirement(false, _Bucket) :-
		!.
	validate_bucket_requirement(true, Bucket) :-
		(	atom(Bucket),
			Bucket \== '' ->
			true
		;	var(Bucket) ->
			instantiation_error
		;	domain_error(s3_bucket, Bucket)
		).

	validate_key_requirement(false, _Key) :-
		!.
	validate_key_requirement(true, Key) :-
		(	atom(Key) ->
			true
		;	var(Key) ->
			instantiation_error
		;	domain_error(s3_key, Key)
		).

	valid_option(region(Region)) :-
		valid_non_empty_atom(Region).
	valid_option(endpoint(URL)) :-
		valid_endpoint(URL).
	valid_option(addressing_style(AddressingStyle)) :-
		once((AddressingStyle == virtual_hosted; AddressingStyle == path)).

	default_option(region('us-east-1')).
	default_option(addressing_style(virtual_hosted)).

	valid_endpoint(URL) :-
		atom(URL),
		url(atom)::parse(URL, Components),
		member(scheme(Scheme), Components),
		once((Scheme == http ; Scheme == https)).

	valid_non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

	unreserved_code(Code) :-
		Code >= 0'a,
		Code =< 0'z,
		!.
	unreserved_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z,
		!.
	unreserved_code(Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	unreserved_code(0'-).
	unreserved_code(0'.).
	unreserved_code(0'_).
	unreserved_code(0'~).

	percent_encoded_byte(Code, High, Low) :-
		HighValue is Code // 16,
		LowValue is Code mod 16,
		hex_digit(HighValue, High),
		hex_digit(LowValue, Low).

	hex_digit(Value, Code) :-
		Value < 10,
		!,
		Code is 0'0 + Value.
	hex_digit(Value, Code) :-
		Code is 0'A + Value - 10.

:- end_object.
