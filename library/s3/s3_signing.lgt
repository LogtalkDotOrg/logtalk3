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


:- object(s3_signing,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'AWS Signature Version 4 helpers specialized for S3.'
	]).

	:- public(payload_hash/2).
	:- mode(payload_hash(+compound, -atom), one_or_error).
	:- info(payload_hash/2, [
		comment is 'Returns the hexadecimal SHA-256 payload hash for a normalized HTTP body term.',
		argnames is ['Body', 'Hash'],
		exceptions is [
			'``Body`` is not a valid normalized HTTP body term' - domain_error(http_body, 'Body'),
			'No registered HTTP body codec exists for the body media type when codec-based encoding is required' - existence_error(http_body_codec, 'MediaType')
		]
	]).

	:- public(canonical_query/2).
	:- mode(canonical_query(+list(pair), -atom), one_or_error).
	:- info(canonical_query/2, [
		comment is 'Returns the canonical AWS SigV4 query string for the given name-value pairs.',
		argnames is ['Pairs', 'Query'],
		exceptions is [
			'``Pairs`` is not a valid list of canonical query pairs' - domain_error(s3_query_pairs, 'Pairs')
		]
	]).

	:- public(canonical_request/6).
	:- mode(canonical_request(+atom, +atom, +list(pair), +list(compound), +atom, -atom), one_or_error).
	:- info(canonical_request/6, [
		comment is 'Builds the canonical request string for the given method, URI, query pairs, signed headers, and payload hash.',
		argnames is ['Method', 'CanonicalURI', 'QueryPairs', 'Headers', 'PayloadHash', 'CanonicalRequest'],
		exceptions is [
			'``Method`` is not a supported HTTP method for this helper' - domain_error(s3_method, 'Method'),
			'``QueryPairs`` is not a valid list of canonical query pairs' - domain_error(s3_query_pairs, 'QueryPairs')
		]
	]).

	:- public(sign_request/8).
	:- mode(sign_request(+atom, +atom, +atom, +list(pair), +compound, +list(compound), -list(compound), +list(compound)), one_or_error).
	:- info(sign_request/8, [
		comment is 'Signs an S3 request and returns the final normalized header list.',
		argnames is ['Method', 'HostHeader', 'CanonicalURI', 'QueryPairs', 'Body', 'Headers', 'SignedHeaders', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials.',
			'Option ``region/1``' - 'AWS region for the credential scope. Defaults to ``us-east-1``.',
			'Option ``payload_hash_mode/1``' - 'Payload signing mode, either ``signed`` or ``unsigned``. Defaults to ``signed``.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.'
		],
		exceptions is [
			'``Options`` is missing valid credentials' - domain_error(s3_credentials, missing),
			'``Body`` is not a valid normalized HTTP body term' - domain_error(http_body, 'Body'),
			'``Headers`` contains a managed or invalid signed header' - domain_error(s3_managed_header, 'Header'),
			'The signing timestamp is invalid' - domain_error(s3_amz_date, 'AmzDate')
		]
	]).

	:- public(presigned_query_pairs/7).
	:- mode(presigned_query_pairs(+atom, +atom, +atom, +list(pair), +list(compound), -list(pair), +list(compound)), one_or_error).
	:- info(presigned_query_pairs/7, [
		comment is 'Signs an S3 request for query-string authentication and returns the final canonical query pairs, including the signature.',
		argnames is ['Method', 'HostHeader', 'CanonicalURI', 'QueryPairs', 'Headers', 'SignedQueryPairs', 'Options'],
		remarks is [
			'Option ``credentials/1``' - 'Required access credentials as ``credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))``.',
			'Option ``session_token/1``' - 'Optional session token for temporary credentials. It is emitted as the ``X-Amz-Security-Token`` query parameter.',
			'Option ``region/1``' - 'AWS region for the credential scope. Defaults to ``us-east-1``.',
			'Option ``payload_hash_mode/1``' - 'Presigned URLs default to ``unsigned`` payload hashing and use ``UNSIGNED-PAYLOAD``. Use ``signed`` together with ``payload_hash/1`` to sign a precomputed payload hash.',
			'Option ``payload_hash/1``' - 'Precomputed hexadecimal SHA-256 payload hash required when ``payload_hash_mode(signed)`` is selected.',
			'Option ``request_time/1``' - 'Optional explicit local request time as ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``.',
			'Option ``utc_offset_seconds/1``' - 'UTC offset used to derive the signing time from the local clock. Defaults to ``0``.',
			'Option ``amz_date/1``' - 'Optional explicit AWS SigV4 timestamp overriding any derived request time.',
			'Option ``expires/1``' - 'Presigned URL lifetime in seconds. Defaults to ``900`` and must not exceed ``604800``.'
		],
		exceptions is [
			'``Options`` is missing valid credentials' - domain_error(s3_credentials, missing),
			'``Headers`` contains a managed or invalid signed header' - domain_error(s3_managed_header, 'Header'),
			'The signing timestamp is invalid' - domain_error(s3_amz_date, 'AmzDate'),
			'The expiration is invalid or outside the AWS SigV4 range' - domain_error(s3_presign_expires, 'Expires'),
			'The payload hash is missing, invalid, or incompatible with unsigned mode' - domain_error(s3_presign_payload_hash, 'Hash')
		]
	]).

	:- uses(date, [
		local_to_utc/3
	]).

	:- uses(hmac, [
		digest/4, hex_digest/4
	]).

	:- uses(http_core, [
		generate_body/3
	]).

	:- uses(list, [
		append/2, append/3, length/2, member/2
	]).

	:- uses(os, [
		date_time/7
	]).

	:- uses(term_io, [
		write_term_to_atom/3
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	payload_hash(Body, Hash) :-
		http_core::generate_body(bytes(Bytes), Body, []),
		sha256::hash(Bytes, Hash).

	canonical_query(Pairs, Query) :-
		valid_query_pairs(Pairs),
		findall(
			EncodedName-EncodedValue,
			(
				member(Name-Value, Pairs),
				query_component_atom(Name, NameAtom),
				query_component_atom(Value, ValueAtom),
				percent_encode_atom(NameAtom, false, EncodedName),
				percent_encode_atom(ValueAtom, false, EncodedValue)
			),
			EncodedPairs
		),
		sort_pairs(EncodedPairs, SortedPairs),
		canonical_query_string(SortedPairs, Query).

	canonical_request(Method, CanonicalURI, QueryPairs, Headers, PayloadHash, CanonicalRequest) :-
		valid_method(Method),
		canonical_query(QueryPairs, CanonicalQuery),
		canonical_headers(Headers, CanonicalHeaders, SignedHeaders),
		method_atom(Method, MethodAtom),
		atomic_list_concat([
			MethodAtom,
			CanonicalURI,
			CanonicalQuery,
			CanonicalHeaders,
			SignedHeaders,
			PayloadHash
		], '\n', CanonicalRequest).

	sign_request(Method, HostHeader, CanonicalURI, QueryPairs, Body, Headers0, Headers, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		valid_method(Method),
		reject_managed_headers(Headers0),
		credentials(MergedOptions, AccessKeyId, SecretAccessKey),
		region(MergedOptions, Region),
		resolve_payload_hash(Body, MergedOptions, PayloadHash),
		resolve_amz_date(MergedOptions, AmzDate, DateStamp),
		session_token_header(MergedOptions, TokenHeaders),
		append([
			[host-HostHeader, x_amz_date-AmzDate, x_amz_content_sha256-PayloadHash],
			TokenHeaders,
			Headers0
		], UnsignedHeaders),
		canonical_request(Method, CanonicalURI, QueryPairs, UnsignedHeaders, PayloadHash, CanonicalRequest),
		canonical_headers(UnsignedHeaders, _CanonicalHeaders, SignedHeadersAtom),
		credential_scope(DateStamp, Region, Scope),
		string_to_sign(AmzDate, Scope, CanonicalRequest, StringToSign),
		signature(SecretAccessKey, DateStamp, Region, StringToSign, Signature),
		authorization_header(AccessKeyId, Scope, SignedHeadersAtom, Signature, Authorization),
		Headers = [authorization-Authorization| UnsignedHeaders].

	presigned_query_pairs(Method, HostHeader, CanonicalURI, QueryPairs0, Headers0, QueryPairs, Options) :-
		check_presign_options(Options),
		valid_method(Method),
		reject_managed_headers(Headers0),
		credentials(Options, AccessKeyId, SecretAccessKey),
		region(Options, Region),
		resolve_presigned_payload_hash(Options, PayloadHash),
		resolve_amz_date(Options, AmzDate, DateStamp),
		resolve_presign_expires(Options, Expires),
		SignedRequestHeaders = [host-HostHeader| Headers0],
		canonical_headers(SignedRequestHeaders, _CanonicalHeaders, SignedHeadersAtom),
		credential_scope(DateStamp, Region, Scope),
		presign_parameters(AccessKeyId, Scope, AmzDate, Expires, SignedHeadersAtom, Options, SigningParameters),
		append(QueryPairs0, SigningParameters, UnsignedQueryPairs),
		canonical_request(Method, CanonicalURI, UnsignedQueryPairs, SignedRequestHeaders, PayloadHash, CanonicalRequest),
		string_to_sign(AmzDate, Scope, CanonicalRequest, StringToSign),
		signature(SecretAccessKey, DateStamp, Region, StringToSign, Signature),
		append(UnsignedQueryPairs, ['X-Amz-Signature'-Signature], QueryPairs).

	resolve_payload_hash(_Body, Options, 'UNSIGNED-PAYLOAD') :-
		^^option(payload_hash_mode(unsigned), Options),
		!.
	resolve_payload_hash(Body, _Options, PayloadHash) :-
		payload_hash(Body, PayloadHash).

	resolve_presigned_payload_hash(Options, PayloadHash) :-
		^^option(payload_hash_mode(signed), Options),
		!,
		(	^^option(payload_hash(PayloadHash), Options) ->
			true
		;	domain_error(s3_presign_payload_hash, missing)
		).
	resolve_presigned_payload_hash(Options, 'UNSIGNED-PAYLOAD') :-
		\+ ^^option(payload_hash(_), Options),
		!.
	resolve_presigned_payload_hash(Options, _PayloadHash) :-
		^^option(payload_hash(PayloadHash), Options),
		domain_error(s3_presign_payload_hash, PayloadHash).

	resolve_amz_date(Options, AmzDate, DateStamp) :-
		(	^^option(amz_date(AmzDate0), Options) ->
			validate_amz_date(AmzDate0),
			AmzDate = AmzDate0
		;	^^option(request_time(DateTime), Options) ->
			format_amz_date(DateTime, AmzDate)
		;	^^option(utc_offset_seconds(OffsetSeconds), Options),
			os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _Milliseconds),
			date::local_to_utc(date_time(Year, Month, Day, Hours, Minutes, Seconds), OffsetSeconds, UTCDateTime),
			format_amz_date(UTCDateTime, AmzDate)
		),
		amz_date_stamp(AmzDate, DateStamp).

	session_token_header(Options, [x_amz_security_token-Token]) :-
		^^option(session_token(Token), Options),
		!.
	session_token_header(_Options, []).

	resolve_presign_expires(Options, Expires) :-
		(	^^option(expires(Expires0), Options) ->
			validate_presign_expires(Expires0),
			Expires = Expires0
		;	Expires = 900
		).

	presign_parameters(AccessKeyId, Scope, AmzDate, Expires, SignedHeaders, Options, Parameters) :-
		atomic_list_concat([AccessKeyId, '/', Scope], Credential),
		session_token_query_pair(Options, TokenParameters),
		Parameters = [
			'X-Amz-Algorithm'-'AWS4-HMAC-SHA256',
			'X-Amz-Credential'-Credential,
			'X-Amz-Date'-AmzDate,
			'X-Amz-Expires'-Expires,
			'X-Amz-SignedHeaders'-SignedHeaders
			| TokenParameters
		].

	session_token_query_pair(Options, ['X-Amz-Security-Token'-Token]) :-
		^^option(session_token(Token), Options),
		!.
	session_token_query_pair(_Options, []).

	canonical_headers(Headers, CanonicalHeaders, SignedHeadersAtom) :-
		header_name_values(Headers, NameValuePairs),
		sort_pairs(NameValuePairs, SortedPairs),
		canonical_header_lines(SortedPairs, CanonicalHeaderLines),
		atomic_list_concat(CanonicalHeaderLines, '\n', CanonicalHeaderBlock0),
		atom_concat(CanonicalHeaderBlock0, '\n', CanonicalHeaders),
		signed_header_names(SortedPairs, SignedHeadersAtom).

	canonical_query_string([], '').
	canonical_query_string([Name-Value| Pairs], Query) :-
		findall(
			Pair,
			(
				member(PairName-PairValue, [Name-Value| Pairs]),
				atomic_list_concat([PairName, '=', PairValue], Pair)
			),
			QueryPairs
		),
		atomic_list_concat(QueryPairs, '&', Query).

	header_name_values([], []).
	header_name_values([Name-Value| Headers], [NormalizedName-NormalizedValue| Pairs]) :-
		validate_header_name(Name),
		header_name_atom(Name, NormalizedName),
		header_value_atom(Value, NormalizedValue),
		header_name_values(Headers, Pairs).

	canonical_header_lines([], []).
	canonical_header_lines([Name-Value| Pairs], [Line| Lines]) :-
		atomic_list_concat([Name, ':', Value], Line),
		canonical_header_lines(Pairs, Lines).

	signed_header_names([], '').
	signed_header_names([Name-_| Pairs], SignedHeaders) :-
		findall(HeaderName, member(HeaderName-_, [Name-_| Pairs]), Names),
		atomic_list_concat(Names, ';', SignedHeaders).

	string_to_sign(AmzDate, Scope, CanonicalRequest, StringToSign) :-
		atom_codes(CanonicalRequest, CanonicalRequestCodes),
		sha256::hash(CanonicalRequestCodes, CanonicalRequestHash),
		atomic_list_concat([
			'AWS4-HMAC-SHA256',
			AmzDate,
			Scope,
			CanonicalRequestHash
		], '\n', StringToSign).

	signature(SecretAccessKey, DateStamp, Region, StringToSign, Signature) :-
		atom_concat('AWS4', SecretAccessKey, SecretPrefix),
		atom_codes(SecretPrefix, SecretPrefixCodes),
		atom_codes(DateStamp, DateStampCodes),
		atom_codes(Region, RegionCodes),
		atom_codes(s3, ServiceCodes),
		atom_codes('aws4_request', RequestCodes),
		atom_codes(StringToSign, StringToSignCodes),
		hmac::digest(sha256, SecretPrefixCodes, DateStampCodes, DateKey),
		hmac::digest(sha256, DateKey, RegionCodes, RegionKey),
		hmac::digest(sha256, RegionKey, ServiceCodes, ServiceKey),
		hmac::digest(sha256, ServiceKey, RequestCodes, SigningKey),
		hmac::hex_digest(sha256, SigningKey, StringToSignCodes, Signature).

	authorization_header(AccessKeyId, Scope, SignedHeaders, Signature, Authorization) :-
		atomic_list_concat([
			'AWS4-HMAC-SHA256 Credential=', AccessKeyId, '/', Scope,
			', SignedHeaders=', SignedHeaders,
			', Signature=', Signature
		], Authorization).

	credential_scope(DateStamp, Region, Scope) :-
		atomic_list_concat([DateStamp, Region, s3, aws4_request], '/', Scope).

	amz_date_stamp(AmzDate, DateStamp) :-
		sub_atom(AmzDate, 0, 8, _, DateStamp).

	format_amz_date(date_time(Year, Month, Day, Hours, Minutes, Seconds), AmzDate) :-
		four_digit_atom(Year, YearAtom),
		two_digit_atom(Month, MonthAtom),
		two_digit_atom(Day, DayAtom),
		two_digit_atom(Hours, HoursAtom),
		two_digit_atom(Minutes, MinutesAtom),
		two_digit_atom(Seconds, SecondsAtom),
		atomic_list_concat([
			YearAtom, MonthAtom, DayAtom,
			'T',
			HoursAtom, MinutesAtom, SecondsAtom,
			'Z'
		], AmzDate).
	format_amz_date(DateTime, _AmzDate) :-
		domain_error(s3_request_time, DateTime).

	validate_amz_date(AmzDate) :-
		atom(AmzDate),
		atom_length(AmzDate, 16),
		sub_atom(AmzDate, 8, 1, _, 'T'),
		sub_atom(AmzDate, 15, 1, _, 'Z'),
		!.
	validate_amz_date(AmzDate) :-
		domain_error(s3_amz_date, AmzDate).

	validate_presign_expires(Expires) :-
		integer(Expires),
		Expires > 0,
		Expires =< 604800,
		!.
	validate_presign_expires(Expires) :-
		domain_error(s3_presign_expires, Expires).

	credentials(Options, AccessKeyId, SecretAccessKey) :-
		^^option(credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey)), Options),
		!.
	credentials(_Options, _AccessKeyId, _SecretAccessKey) :-
		domain_error(s3_credentials, missing).

	region(Options, Region) :-
		^^option(region(Region), Options),
		!.
	region(_Options, 'us-east-1').

	check_presign_options([]).
	check_presign_options([Option| Options]) :-
		check_presign_option(Option),
		check_presign_options(Options).

	check_presign_option(Option) :-
		valid_option(Option),
		!.
	check_presign_option(expires(Expires)) :-
		validate_presign_expires(Expires),
		!.
	check_presign_option(payload_hash(Hash)) :-
		validate_payload_hash(Hash),
		!.
	check_presign_option(Option) :-
		domain_error(s3_presign_option, Option).

	valid_option(credentials(access_key_id(AccessKeyId), secret_access_key(SecretAccessKey))) :-
		valid_non_empty_atom(AccessKeyId),
		valid_non_empty_atom(SecretAccessKey).
	valid_option(session_token(Token)) :-
		valid_non_empty_atom(Token).
	valid_option(region(Region)) :-
		valid_non_empty_atom(Region).
	valid_option(payload_hash_mode(PayloadHashMode)) :-
		once((PayloadHashMode == signed; PayloadHashMode == unsigned)).
	valid_option(payload_hash(Hash)) :-
		validate_payload_hash(Hash).
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
	default_option(payload_hash_mode(signed)).
	default_option(utc_offset_seconds(0)).

	valid_non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

	validate_payload_hash(Hash) :-
		atom(Hash),
		atom_length(Hash, 64),
		atom_codes(Hash, Codes),
		hex_codes(Codes),
		!.
	validate_payload_hash(Hash) :-
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

	valid_query_pairs([]).
	valid_query_pairs([Name-Value| Pairs]) :-
		query_component_atom(Name, _),
		query_component_atom(Value, _),
		!,
		valid_query_pairs(Pairs).
	valid_query_pairs(Pairs) :-
		domain_error(s3_query_pairs, Pairs).

	query_component_atom(Value, Atom) :-
		atom(Value),
		!,
		Atom = Value.
	query_component_atom(Value, Atom) :-
		number(Value),
		!,
		number_codes(Value, Codes),
		atom_codes(Atom, Codes).
	query_component_atom(Value, Atom) :-
		(	Value == true ->
			Atom = 'true'
		;	Value == false ->
			Atom = 'false'
		;	domain_error(s3_query_component, Value)
		).

	percent_encode_atom(Atom, PreserveSlash, Encoded) :-
		atom_codes(Atom, Codes),
		percent_encode_codes(Codes, PreserveSlash, EncodedCodes),
		atom_codes(Encoded, EncodedCodes).

	percent_encode_codes([], _PreserveSlash, []).
	percent_encode_codes([Code| Codes], PreserveSlash, EncodedCodes) :-
		(	unreserved_code(Code) ->
			EncodedCodes = [Code| Rest]
		;	PreserveSlash == true,
			Code =:= 0'/ ->
			EncodedCodes = [Code| Rest]
		;	percent_encoded_byte(Code, High, Low),
			EncodedCodes = [0'%, High, Low| Rest]
		),
		percent_encode_codes(Codes, PreserveSlash, Rest).

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

	method_atom(Method, MethodAtom) :-
		atom_codes(Method, Codes),
		uppercase_ascii_codes(Codes, UppercaseCodes),
		atom_codes(MethodAtom, UppercaseCodes).

	uppercase_ascii_codes([], []).
	uppercase_ascii_codes([Code| Codes], [UppercaseCode| UppercaseCodes]) :-
		(	Code >= 0'a,
			Code =< 0'z ->
			UppercaseCode is Code - 32
		;	UppercaseCode = Code
		),
		uppercase_ascii_codes(Codes, UppercaseCodes).

	reject_managed_headers([]).
	reject_managed_headers([Name-_| Headers]) :-
		(	managed_header(Name) ->
			domain_error(s3_managed_header, Name)
		;	true
		),
		reject_managed_headers(Headers).

	managed_header(authorization).
	managed_header(host).
	managed_header(x_amz_content_sha256).
	managed_header(x_amz_date).
	managed_header(x_amz_security_token).

	validate_header_name(Name) :-
		(	atom(Name) ->
			true
		;	domain_error(s3_header_name, Name)
		).

	header_name_atom(Name, Atom) :-
		atom_codes(Name, Codes),
		lowercase_name_codes(Codes, LowercaseCodes),
		atom_codes(Atom, LowercaseCodes).

	lowercase_name_codes([], []).
	lowercase_name_codes([0'_| Codes], [0'-| LowercaseCodes]) :-
		!,
		lowercase_name_codes(Codes, LowercaseCodes).
	lowercase_name_codes([Code| Codes], [LowercaseCode| LowercaseCodes]) :-
		(	Code >= 0'A,
			Code =< 0'Z ->
			LowercaseCode is Code + 32
		;	LowercaseCode = Code
		),
		lowercase_name_codes(Codes, LowercaseCodes).

	header_value_atom(Value, Atom) :-
		atom(Value),
		!,
		Atom = Value.
	header_value_atom(Value, Atom) :-
		number(Value),
		!,
		number_codes(Value, Codes),
		atom_codes(Atom, Codes).
	header_value_atom([Value], Atom) :-
		!,
		header_value_atom(Value, Atom).
	header_value_atom(Values, Atom) :-
		list_of_atoms(Values),
		!,
		atomic_list_concat(Values, ',', Atom).
	header_value_atom(Value, Atom) :-
		write_term_to_atom(Value, Atom, []).

	list_of_atoms([]).
	list_of_atoms([Atom| Atoms]) :-
		atom(Atom),
		list_of_atoms(Atoms).

	sort_pairs(Pairs, SortedPairs) :-
		keysort(Pairs, SortedPairs).

	two_digit_atom(Value, Atom) :-
		Value >= 0,
		Value =< 99,
		!,
		(	Value < 10 ->
			number_codes(Value, [Code]),
			atom_codes(Atom, [0'0, Code])
		;	number_codes(Value, Codes),
			atom_codes(Atom, Codes)
		).

	four_digit_atom(Value, Atom) :-
		Value >= 0,
		Value =< 9999,
		!,
		number_codes(Value, Codes0),
		pad_left_codes(Codes0, 4, 0'0, Codes),
		atom_codes(Atom, Codes).

	pad_left_codes(Codes, Length, _PadCode, Codes) :-
		length(Codes, CurrentLength),
		CurrentLength >= Length,
		!.
	pad_left_codes(Codes0, Length, PadCode, Codes) :-
		length(Codes0, CurrentLength),
		Missing is Length - CurrentLength,
		pad_codes(Missing, PadCode, Padding),
		append(Padding, Codes0, Codes).

	pad_codes(0, _PadCode, []) :-
		!.
	pad_codes(Count, PadCode, [PadCode| Codes]) :-
		Count > 0,
		NextCount is Count - 1,
		pad_codes(NextCount, PadCode, Codes).

	valid_method(get).
	valid_method(head).
	valid_method(put).
	valid_method(post).
	valid_method(delete).

:- end_object.
