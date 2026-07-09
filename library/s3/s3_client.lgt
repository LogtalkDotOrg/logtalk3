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


:- object(s3_client(_HTTPTransport_),
	implements(s3_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Stateless S3-compatible client facade over the normalized HTTP client.',
		parnames is ['HTTPTransport']
	]).

	:- uses(http_core, [
		body/2, generate_body/3, status/2
	]).

	:- uses(list, [
		append/3, member/2, valid/1 as proper_list/1
	]).

	:- uses(mime_types, [
		guess_file_type/3
	]).

	:- uses(os, [
		decompose_file_name/4, file_size/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	get_object(Bucket, Key, File, Properties, Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		check_output_file(File, Context),
		get_object_body(Bucket, Key, Body, Properties, Options),
		http_core::generate_body(file(File), Body, []).

	list_buckets(Buckets, Options) :-
		s3_request(get, list_buckets, none, none, [], empty, [], Response, Options),
		response_success(Response),
		http_core::body(Response, Body),
		s3_xml::list_buckets_response(Body, Buckets).

	head_bucket(Bucket, bucket_metadata(Properties), Options) :-
		context(Context),
		check_bucket(Bucket, Context),
		s3_request(head, head_bucket, Bucket, none, [], empty, [], Response, Options),
		response_success(Response),
		s3_headers::response_properties(Response, Properties).

	list_objects_v2(Bucket, Request0, Listing, Options) :-
		context(Context),
		check_bucket(Bucket, Context),
		normalize_list_objects_request(Request0, RequestOptions, Context),
		list_objects_query_pairs(RequestOptions, QueryPairs),
		s3_request(get, list_objects_v2, Bucket, none, QueryPairs, empty, [], Response, Options),
		response_success(Response),
		http_core::body(Response, Body),
		s3_xml::list_objects_v2_response(Body, Listing).

	head_object(Bucket, Key, object_metadata(Properties), Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		s3_request(head, head_object, Bucket, Key, [], empty, [], Response, Options),
		response_success(Response),
		s3_headers::response_properties(Response, Properties).

	get_object_body(Bucket, Key, Body, Properties, Options) :-
		s3_request(get, get_object, Bucket, Key, [], empty, [], Response, Options),
		response_success(Response),
		s3_headers::response_properties(Response, Properties),
		http_core::body(Response, Body).

	presigned_get_object(Bucket, Key, URL, Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		presigned_url(get, get_object, Bucket, Key, URL, Options).

	presigned_put_object(Bucket, Key, URL, Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		presigned_url(put, put_object, Bucket, Key, URL, Options).

	presigned_post_object(Bucket, Key, URL, Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		presigned_url(post, post_object, Bucket, Key, URL, Options).

	put_object(Bucket, Key, File, ETag, Properties, Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		check_input_file(File, Context),
		file_upload_body(File, Body),
		put_object_body(Bucket, Key, Body, ETag, Properties, Options).

	put_object_body(Bucket, Key, Body, ETag, Properties, Options) :-
		s3_request(put, put_object, Bucket, Key, [], Body, [], Response, Options),
		response_success(Response),
		s3_headers::response_properties(Response, Properties),
		etag_from_properties(Properties, ETag).

	delete_object(Bucket, Key, delete_result(Properties), Options) :-
		context(Context),
		check_object_request(Bucket, Key, Context),
		s3_request(delete, delete_object, Bucket, Key, [], empty, [], Response, Options),
		response_success(Response),
		s3_headers::response_properties(Response, Properties).

	copy_object(Source, Bucket, Key, Result, Options) :-
		context(Context),
		check_copy_source(Source, Context),
		check_object_request(Bucket, Key, Context),
		copy_source_header(Source, Header, QueryPairs),
		s3_request(put, copy_object, Bucket, Key, QueryPairs, empty, [Header], Response, Options),
		response_success(Response),
		(	http_core::body(Response, Body),
			Body \== empty ->
			s3_xml::copy_object_response(Body, Result)
		;	s3_headers::response_properties(Response, Properties),
			etag_from_properties(Properties, ETag),
			Result = copy_result(ETag, Properties)
		).

	check_object_request(Bucket, Key, Context) :-
		check_bucket(Bucket, Context),
		check_key(Key, Context).

	check_bucket(Bucket, Context) :-
		(	var(Bucket) ->
			throw(error(instantiation_error, Context))
		;	\+ atom(Bucket) ->
			throw(error(type_error(atom, Bucket), Context))
		;	Bucket == '' ->
			throw(error(domain_error(s3_bucket, Bucket), Context))
		;	true
		).

	check_key(Key, Context) :-
		(	var(Key) ->
			throw(error(instantiation_error, Context))
		;	atom(Key) ->
			true
		;	throw(error(type_error(atom, Key), Context))
		).

	check_copy_source(Source, Context) :-
		(	var(Source) ->
			throw(error(instantiation_error, Context))
		;	Source = source(SourceBucket, SourceKey) ->
			check_object_request(SourceBucket, SourceKey, Context)
		;	Source = source(SourceBucket, SourceKey, VersionId) ->
			check_object_request(SourceBucket, SourceKey, Context),
			check_version_id(VersionId, Context)
		;	compound(Source) ->
			throw(error(domain_error(s3_copy_source, Source), Context))
		;	throw(error(type_error(compound, Source), Context))
		).

	check_version_id(VersionId, Context) :-
		(	var(VersionId) ->
			throw(error(instantiation_error, Context))
		;	\+ atom(VersionId) ->
			throw(error(type_error(atom, VersionId), Context))
		;	VersionId == '' ->
			throw(error(domain_error(s3_version_id, VersionId), Context))
		;	true
		).

	check_input_file(File, Context) :-
		type::check(file([], [read]), File, Context).

	check_output_file(File, Context) :-
		type::check(atom, File, Context),
		(	type::valid(file, File) ->
			type::check(file([], [write]), File, Context)
		;	os::decompose_file_name(File, Directory, _Name, _Extension),
			type::check(directory([write]), Directory, Context)
		).

	s3_request(Method, Operation, Bucket, Key, QueryPairs, Body, Headers0, Response, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		endpoint_request_options(MergedOptions, EndpointOptions),
		signing_request_options(MergedOptions, SigningOptions),
		request_headers(MergedOptions, Headers0, RequestHeaders),
		s3_endpoints::request(Operation, Bucket, Key, BaseURL, endpoint(HostHeader, CanonicalURI), EndpointOptions),
		s3_signing::sign_request(Method, HostHeader, CanonicalURI, QueryPairs, Body, RequestHeaders, Headers, SigningOptions),
		append_query(BaseURL, QueryPairs, URL),
		http_request_options(Headers, Body, MergedOptions, HTTPOptions),
		http_client::request(Method, URL, Response, [transport(_HTTPTransport_)| HTTPOptions]).

	presigned_url(Method, Operation, Bucket, Key, URL, Options) :-
		extract_presign_options(Options, RequestOptions, PresignOptions),
		^^check_options(RequestOptions),
		check_presign_request_options(PresignOptions),
		^^merge_options(RequestOptions, BaseMergedRequestOptions),
		presigned_request_defaults(RequestOptions, BaseMergedRequestOptions, MergedRequestOptions),
		append(PresignOptions, MergedRequestOptions, MergedOptions),
		endpoint_request_options(MergedRequestOptions, EndpointOptions),
		presigning_request_options(MergedOptions, SigningOptions),
		request_headers(MergedRequestOptions, [], Headers),
		s3_endpoints::request(Operation, Bucket, Key, BaseURL, endpoint(HostHeader, CanonicalURI), EndpointOptions),
		s3_signing::presigned_query_pairs(Method, HostHeader, CanonicalURI, [], Headers, QueryPairs, SigningOptions),
		append_query(BaseURL, QueryPairs, URL).

	endpoint_request_options([], []).
	endpoint_request_options([Option| Options], [Option| EndpointOptions]) :-
		relevant_endpoint_option(Option),
		!,
		endpoint_request_options(Options, EndpointOptions).
	endpoint_request_options([_Option| Options], EndpointOptions) :-
		endpoint_request_options(Options, EndpointOptions).

	relevant_endpoint_option(region(_)).
	relevant_endpoint_option(endpoint(_)).
	relevant_endpoint_option(addressing_style(_)).

	signing_request_options([], []).
	signing_request_options([Option| Options], [Option| SigningOptions]) :-
		relevant_signing_option(Option),
		!,
		signing_request_options(Options, SigningOptions).
	signing_request_options([_Option| Options], SigningOptions) :-
		signing_request_options(Options, SigningOptions).

	relevant_signing_option(credentials(_, _)).
	relevant_signing_option(session_token(_)).
	relevant_signing_option(region(_)).
	relevant_signing_option(payload_hash_mode(_)).
	relevant_signing_option(request_time(_)).
	relevant_signing_option(utc_offset_seconds(_)).
	relevant_signing_option(amz_date(_)).

	presigning_request_options([], []).
	presigning_request_options([Option| Options], [Option| SigningOptions]) :-
		relevant_presigning_option(Option),
		!,
		presigning_request_options(Options, SigningOptions).
	presigning_request_options([_Option| Options], SigningOptions) :-
		presigning_request_options(Options, SigningOptions).

	relevant_presigning_option(Option) :-
		relevant_signing_option(Option).
	relevant_presigning_option(expires(_)).
	relevant_presigning_option(payload_hash(_)).

	request_headers(Options, OperationHeaders, Headers) :-
		^^option(headers(OptionHeaders), Options),
		append(OperationHeaders, OptionHeaders, Headers).

	presigned_request_defaults(RequestOptions, MergedOptions, MergedOptions) :-
		member(payload_hash_mode(_), RequestOptions),
		!.
	presigned_request_defaults(_RequestOptions, MergedOptions0, [payload_hash_mode(unsigned)| MergedOptions0]).

	http_request_options(Headers, Body, Options, [headers(Headers), body(Body), version(Version), properties(Properties), connection_options(ConnectionOptions)]) :-
		^^option(version(Version), Options),
		^^option(properties(Properties), Options),
		^^option(connection_options(ConnectionOptions), Options).

	append_query(URL, [], URL) :-
		!.
	append_query(URL0, QueryPairs, URL) :-
		s3_signing::canonical_query(QueryPairs, Query),
		atomic_list_concat([URL0, '?', Query], URL).

	response_success(Response) :-
		http_core::status(Response, status(Code, _Reason)),
		(	Code >= 200,
			Code =< 299 ->
			true
		;	response_error(Response, Error),
			domain_error(s3_http_status, error(Code, Error))
		).

	response_error(Response, Error) :-
		http_core::body(Response, Body),
		s3_xml::error_response(Body, Error),
		!.
	response_error(_Response, unknown).

	etag_from_properties(Properties, ETag) :-
		member(header(etag, ETag), Properties),
		!.
	etag_from_properties(_Properties, none).

	file_upload_body(File, content(MediaType, file(File, 0, Length))) :-
		os::file_size(File, Length),
		file_upload_media_type(File, MediaType).

	file_upload_media_type(File, MediaType) :-
		guess_file_type(File, GuessedMediaType, _Encoding),
		(	GuessedMediaType == '' ->
			MediaType = 'application/octet-stream'
		;	MediaType = GuessedMediaType
		).

	copy_source_header(source(SourceBucket, SourceKey), x_amz_copy_source-HeaderValue, []) :-
		!,
		copy_source_value(SourceBucket, SourceKey, HeaderValue).
	copy_source_header(source(SourceBucket, SourceKey, VersionId), x_amz_copy_source-HeaderValue, []) :-
		!,
		copy_source_value(SourceBucket, SourceKey, BaseHeaderValue),
		s3_signing::canonical_query(['versionId'-VersionId], Query),
		atomic_list_concat([BaseHeaderValue, '?', Query], HeaderValue).
	copy_source_header(Source, _Header, _QueryPairs) :-
		domain_error(s3_copy_source, Source).

	copy_source_value(SourceBucket, SourceKey, HeaderValue) :-
		s3_endpoints::canonical_uri(get_object, endpoint_context('', path), SourceBucket, SourceKey, SourceURI),
		HeaderValue = SourceURI.

	normalize_list_objects_request(Request, _Options, Context) :-
		var(Request),
		!,
		throw(error(instantiation_error, Context)).
	normalize_list_objects_request(list_objects_v2_request(Options), Options, Context) :-
		!,
		check_list_objects_request_options(Options, Context).
	normalize_list_objects_request(Options, Options, Context) :-
		proper_list(Options),
		!,
		check_list_objects_request_options(Options, Context).
	normalize_list_objects_request(Request, _Options, Context) :-
		throw(error(domain_error(s3_list_objects_request, Request), Context)).

	list_objects_query_pairs(Options, ['list-type'-2| QueryPairs]) :-
		findall(
			Name-Value,
			list_objects_query_pair(Options, Name, Value),
			QueryPairs
		).

	list_objects_query_pair(Options, delimiter, Value) :-
		^^option(delimiter(Value), Options).
	list_objects_query_pair(Options, 'continuation-token', Value) :-
		^^option(continuation_token(Value), Options).
	list_objects_query_pair(Options, 'encoding-type', Value) :-
		^^option(encoding_type(Value), Options).
	list_objects_query_pair(Options, 'fetch-owner', true) :-
		^^option(fetch_owner(true), Options).
	list_objects_query_pair(Options, 'max-keys', Value) :-
		^^option(max_keys(Value), Options).
	list_objects_query_pair(Options, prefix, Value) :-
		^^option(prefix(Value), Options).
	list_objects_query_pair(Options, 'start-after', Value) :-
		^^option(start_after(Value), Options).

	check_presign_request_options([]).
	check_presign_request_options([Option| Options]) :-
		check_presign_request_option(Option),
		check_presign_request_options(Options).

	check_presign_request_option(expires(Expires)) :-
		integer(Expires),
		Expires > 0,
		Expires =< 604800,
		!.
	check_presign_request_option(expires(Expires)) :-
		domain_error(s3_presign_expires, Expires).
	check_presign_request_option(payload_hash(Hash)) :-
		valid_payload_hash(Hash),
		!.
	check_presign_request_option(Option) :-
		domain_error(s3_client_option, Option).

	extract_presign_options([], [], []).
	extract_presign_options([Option| Options], RequestOptions, [Option| PresignOptions]) :-
		nonvar(Option),
		presign_only_option(Option),
		!,
		extract_presign_options(Options, RequestOptions, PresignOptions).
	extract_presign_options([Option| Options], [Option| RequestOptions], PresignOptions) :-
		extract_presign_options(Options, RequestOptions, PresignOptions).

	presign_only_option(expires(_)).
	presign_only_option(payload_hash(_)).

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
		proper_list(Headers).
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.
	valid_option(properties(Properties)) :-
		proper_list(Properties).
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions).
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

	check_list_objects_request_options([], _).
	check_list_objects_request_options([Option| Options], Context) :-
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	\+ compound(Option) ->
			throw(error(type_error(compound, Option), Context))
		;	check_list_objects_request_option(Option) ->
			true
		;	throw(error(domain_error(s3_list_objects_request_option, Option), Context))
		),
		check_list_objects_request_options(Options, Context).

	check_list_objects_request_option(delimiter(Value)) :-
		valid_non_empty_atom(Value).
	check_list_objects_request_option(continuation_token(Value)) :-
		valid_non_empty_atom(Value).
	check_list_objects_request_option(encoding_type(EncodingType)) :-
		EncodingType == url.
	check_list_objects_request_option(fetch_owner(FetchOwner)) :-
		once((FetchOwner == true; FetchOwner == false)).
	check_list_objects_request_option(max_keys(Value)) :-
		integer(Value),
		Value >= 0.
	check_list_objects_request_option(prefix(Value)) :-
		atom(Value).
	check_list_objects_request_option(start_after(Value)) :-
		atom(Value).

	valid_endpoint(URL) :-
		atom(URL),
		url(atom)::parse(URL, Components),
		member(scheme(Scheme), Components),
		once((Scheme == http ; Scheme == https)).

	valid_payload_hash(Hash) :-
		atom(Hash),
		atom_length(Hash, 64),
		atom_codes(Hash, Codes),
		hex_codes(Codes),
		!.
	valid_payload_hash(Hash) :-
		domain_error(s3_payload_hash, Hash).

	hex_codes([]).
	hex_codes([Code| Codes]) :-
		hex_code(Code),
		hex_codes(Codes).

	hex_code(Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	hex_code(Code) :-
		Code >= 0'a,
		Code =< 0'f,
		!.
	hex_code(Code) :-
		Code >= 0'A,
		Code =< 0'F.

	valid_non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

:- end_object.


:- object(s3_client,
	extends(s3_client(http_process_transport))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Stateless S3-compatible client facade over the normalized HTTP client using by default the ``http_process_transport`` library.'
	]).

:- end_object.
