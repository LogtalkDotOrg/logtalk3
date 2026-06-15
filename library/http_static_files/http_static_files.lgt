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


:- object(http_static_files,
	imports([options, http_docroot_paths])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router-agnostic static file response helper built on the normalized ``http_core`` library.'
	]).

	:- public(serve/4).
	:- mode(serve(+atom, +compound, +atom, -compound), one_or_error).
	:- info(serve/4, [
		comment is 'Serves a relative request path from the given document root using the default options and returns a normalized response.',
		argnames is ['Path', 'Request', 'DocumentRoot', 'Response'],
		exceptions is [
			'``Path``, ``Request``, or ``DocumentRoot`` are invalid for document-root serving' - error
		]
	]).

	:- public(serve/5).
	:- mode(serve(+atom, +compound, +atom, +list(compound), -compound), one_or_error).
	:- info(serve/5, [
		comment is 'Serves a relative request path from the given document root using the given options and returns a normalized response.',
		argnames is ['Path', 'Request', 'DocumentRoot', 'Options', 'Response'],
		exceptions is [
			'``Path``, ``Request``, ``DocumentRoot``, or ``Options`` are invalid for static-file serving' - error
		]
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	:- uses(date, [
		date_time_to_unix/2, format_date_time/4, unix_to_date_time/2, valid_date_time/1
	]).

	serve(Path, Request, DocumentRoot, Response) :-
		serve(Path, Request, DocumentRoot, [], Response).

	serve(Path, Request, DocumentRoot, UserOptions, Response) :-
		^^validate_relative_path(Path),
		^^validate_request(Request),
		^^validate_document_root(DocumentRoot),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	^^supported_method(Request) ->
			(	resolve_resource(Path, Request, DocumentRoot, Options, Resource) ->
				resource_response(Request, Resource, Response)
			;	not_found_response(Request, Response)
			)
		;	method_not_allowed_response(Request, Response)
		).

	valid_option(index_files(IndexFiles)) :-
		valid_atom_list(IndexFiles).
	valid_option(mime_types_strict(Boolean)) :-
		valid_boolean_option(Boolean).
	valid_option(cache_control(Directives)) :-
		valid_cache_control_directives(Directives).
	valid_option(expires(none)).
	valid_option(expires(Value)) :-
		valid_expires_value(Value).

	default_option(index_files(['index.html', 'index.htm'])).
	default_option(mime_types_strict(false)).
	default_option(cache_control([])).
	default_option(expires(none)).

	resolve_resource(Path, Request, DocumentRoot, Options, Resource) :-
		resolved_document_root(DocumentRoot, Root),
		^^resolved_target_path(Path, Root, Candidate),
		existing_target_file(Candidate, Root, Options, TargetFile),
		selected_representation_file(Request, TargetFile, File, VaryAcceptEncoding),
		(	File == not_acceptable ->
			Resource = not_acceptable(VaryAcceptEncoding)
		;	resource(File, Options, VaryAcceptEncoding, Resource)
		).

	resolved_document_root(DocumentRoot, Root) :-
		os::absolute_file_name(DocumentRoot, AbsoluteDocumentRoot),
		(	os::directory_exists(AbsoluteDocumentRoot) ->
			os::path_concat(AbsoluteDocumentRoot, '', Root)
		;	domain_error(http_static_files_document_root, DocumentRoot)
		).

	existing_target_file(Candidate, _Root, _Options, Candidate) :-
		os::file_exists(Candidate),
		!.
	existing_target_file(Candidate, Root, Options, File) :-
		os::directory_exists(Candidate),
		!,
		directory_index_file(Candidate, Root, Options, File).

	directory_index_file(Directory, Root, Options, File) :-
		^^option(index_files(IndexFiles), Options),
		member(IndexFile, IndexFiles),
		os::path_concat(Directory, IndexFile, Candidate0),
		os::absolute_file_name(Candidate0, Candidate),
		^^path_within_root(Root, Candidate),
		os::file_exists(Candidate),
		!,
		File = Candidate.

	selected_representation_file(Request, TargetFile, File, VaryAcceptEncoding) :-
		negotiable_resource_file(TargetFile),
		precompressed_variant_files(TargetFile, VariantFiles),
		!,
		VaryAcceptEncoding = true,
		preferred_representation_file(Request, TargetFile, VariantFiles, File).
	selected_representation_file(Request, TargetFile, not_acceptable, false) :-
		negotiable_resource_file(TargetFile),
		request_header_value(Request, accept_encoding, Value),
		accept_encoding_specs(Value, Specs),
		identity_quality(Specs, IdentityQuality),
		IdentityQuality =< 0.0,
		!.
	selected_representation_file(_Request, File, File, false).

	negotiable_resource_file(File) :-
		mime_types::guess_file_type(File, _Type, Encoding, false),
		Encoding == ''.

	precompressed_variant_files(BaseFile, VariantFiles) :-
		(	precompressed_variant_file(BaseFile, br, BrotliFile) ->
			BrotliVariants = [br-BrotliFile]
		;	BrotliVariants = []
		),
		(	precompressed_variant_file(BaseFile, gzip, GzipFile) ->
			append(BrotliVariants, [gzip-GzipFile], VariantFiles)
		;	VariantFiles = BrotliVariants
		),
		VariantFiles \== [].

	precompressed_variant_file(BaseFile, Encoding, VariantFile) :-
		precompressed_variant_suffix(Encoding, Suffix),
		atom_concat(BaseFile, Suffix, VariantFile),
		os::file_exists(VariantFile).

	precompressed_variant_suffix(br, '.br').
	precompressed_variant_suffix(gzip, '.gz').

	preferred_representation_file(Request, BaseFile, VariantFiles, File) :-
		request_header_value(Request, accept_encoding, Value),
		!,
		accept_encoding_specs(Value, Specs),
		best_representation_file(Specs, BaseFile, VariantFiles, File).
	preferred_representation_file(_Request, BaseFile, _VariantFiles, BaseFile).

	best_representation_file(Specs, BaseFile, VariantFiles, File) :-
		identity_quality(Specs, IdentityQuality),
		representation_rank(identity, IdentityRank),
		BestChoice0 = choice(BaseFile, IdentityQuality, IdentityRank),
		variant_representation_choice(br, Specs, VariantFiles, BrotliChoice),
		better_representation_choice(BrotliChoice, BestChoice0, BestChoice1),
		variant_representation_choice(gzip, Specs, VariantFiles, GzipChoice),
		better_representation_choice(GzipChoice, BestChoice1, choice(File0, Quality, _Rank)),
		(	Quality > 0.0 ->
			File = File0
		;	File = not_acceptable
		).

	variant_representation_choice(Encoding, Specs, VariantFiles, choice(File, Quality, Rank)) :-
		memberchk(Encoding-File, VariantFiles),
		content_coding_quality(Specs, Encoding, Quality),
		Quality > 0.0,
		representation_rank(Encoding, Rank),
		!.
	variant_representation_choice(_Encoding, _Specs, _VariantFiles, none).

	better_representation_choice(none, BestChoice, BestChoice) :-
		!.
	better_representation_choice(choice(File, Quality, Rank), choice(_BestFile, BestQuality, BestRank), choice(File, Quality, Rank)) :-
		(	Quality > BestQuality
		;	Quality =:= BestQuality,
			Rank > BestRank
		),
		!.
	better_representation_choice(_Choice, BestChoice, BestChoice).

	representation_rank(br, 3).
	representation_rank(gzip, 2).
	representation_rank(identity, 1).

	content_coding_quality(Specs, Encoding, Quality) :-
		content_coding_specific_quality(Specs, Encoding, Quality),
		!.
	content_coding_quality(Specs, _Encoding, Quality) :-
		content_coding_wildcard_quality(Specs, Quality),
		!.
	content_coding_quality(_Specs, _Encoding, 0.0).

	content_coding_specific_quality(Specs, gzip, Quality) :-
		best_content_coding_quality(Specs, [gzip, 'x-gzip'], Quality),
		!.
	content_coding_specific_quality(Specs, Encoding, Quality) :-
		best_content_coding_quality(Specs, [Encoding], Quality).

	content_coding_wildcard_quality(Specs, Quality) :-
		best_content_coding_quality(Specs, ['*'], Quality).

	best_content_coding_quality(Specs, Encodings, Quality) :-
		best_content_coding_quality(Specs, Encodings, -1.0, Quality),
		Quality >= 0.0.

	best_content_coding_quality([], _Encodings, Quality, Quality).
	best_content_coding_quality([accept_encoding(Coding, CodingQuality)| Specs], Encodings, BestQuality0, BestQuality) :-
		(	memberchk(Coding, Encodings),
			CodingQuality > BestQuality0 ->
			BestQuality1 = CodingQuality
		;	BestQuality1 = BestQuality0
		),
		best_content_coding_quality(Specs, Encodings, BestQuality1, BestQuality).

	identity_quality(Specs, Quality) :-
		content_coding_specific_quality(Specs, identity, Quality),
		!.
	identity_quality(Specs, 0.0) :-
		content_coding_wildcard_quality(Specs, WildcardQuality),
		WildcardQuality =< 0.0,
		!.
	identity_quality(_Specs, 1.0).

	accept_encoding_specs(Value, Specs) :-
		trim_ows_atom(Value, TrimmedValue),
		(	TrimmedValue == '' ->
			Specs = []
		;	atom::split(TrimmedValue, ',', Items),
			accept_encoding_items(Items, Specs)
		).

	accept_encoding_items([], []).
	accept_encoding_items([Item| Items], Specs) :-
		(	accept_encoding_item(Item, Spec) ->
			Specs = [Spec| Specs0]
		;	Specs = Specs0
		),
		accept_encoding_items(Items, Specs0).

	accept_encoding_item(Item0, accept_encoding(Coding, Quality)) :-
		trim_ows_atom(Item0, Item),
		Item \== '',
		atom::split(Item, ';', Parts),
		Parts = [Coding0| Parameters],
		trim_ows_atom(Coding0, TrimmedCoding),
		TrimmedCoding \== '',
		lowercase_ascii_atom(TrimmedCoding, Coding),
		accept_encoding_quality(Parameters, Quality),
		Quality >= 0.0,
		Quality =< 1.0.

	accept_encoding_quality(Parameters, Quality) :-
		accept_encoding_quality(Parameters, 1.0, Quality).

	accept_encoding_quality([], Quality, Quality).
	accept_encoding_quality([Parameter0| _Parameters], _DefaultQuality, Quality) :-
		q_parameter_quality(Parameter0, Quality),
		!.
	accept_encoding_quality([_Parameter| Parameters], DefaultQuality, Quality) :-
		accept_encoding_quality(Parameters, DefaultQuality, Quality).

	q_parameter_quality(Parameter0, Quality) :-
		trim_ows_atom(Parameter0, Parameter),
		atom::split(Parameter, '=', [Name0, Value0]),
		trim_ows_atom(Name0, TrimmedName),
		lowercase_ascii_atom(TrimmedName, Name),
		Name == q,
		trim_ows_atom(Value0, TrimmedValue),
		TrimmedValue \== '',
		atom_codes(TrimmedValue, ValueCodes),
		catch(number_codes(Quality, ValueCodes), _, fail),
		Quality >= 0.0,
		Quality =< 1.0.

	lowercase_ascii_atom(Atom0, Atom) :-
		atom_codes(Atom0, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes(Atom, Codes).

	lowercase_ascii_codes([], []).
	lowercase_ascii_codes([Code0| Codes0], [Code| Codes]) :-
		lowercase_ascii_code(Code0, Code),
		lowercase_ascii_codes(Codes0, Codes).

	lowercase_ascii_code(Code0, Code) :-
		Code0 >= 0'A,
		Code0 =< 0'Z,
		!,
		Code is Code0 + 32.
	lowercase_ascii_code(Code, Code).

	resource(File, Options, VaryAcceptEncoding, resource(File, MediaType, Headers, Size, ModifiedTime, ETag, LastModified)) :-
		os::file_size(File, Size),
		os::file_modification_time(File, ModifiedTime0),
		normalize_modification_time(ModifiedTime0, ModifiedTime),
		entity_tag(Size, ModifiedTime, ETag),
		http_date(ModifiedTime, LastModified),
		resource_headers(File, Options, VaryAcceptEncoding, ETag, LastModified, Headers, MediaType).

	resource_headers(File, Options, VaryAcceptEncoding, ETag, LastModified, Headers, MediaType) :-
		representation_headers(File, Options, RepresentationHeaders, MediaType),
		vary_headers(VaryAcceptEncoding, VaryHeaders),
		cache_headers(CacheHeaders, Options),
		append(RepresentationHeaders, VaryHeaders, HeaderPrefix0),
		append(HeaderPrefix0, CacheHeaders, HeaderPrefix),
		append(HeaderPrefix, [accept_ranges-'bytes', etag-ETag, last_modified-LastModified], Headers).

	cache_headers(Headers, Options) :-
		cache_control_headers(CacheControlHeaders, Options),
		expires_headers(ExpiresHeaders, Options),
		append(CacheControlHeaders, ExpiresHeaders, Headers).

	cache_control_headers([cache_control-Value], Options) :-
		^^option(cache_control(Directives), Options),
		Directives \== [],
		!,
		cache_control_value(Directives, Value).
	cache_control_headers([], _Options).

	expires_headers([expires-Value], Options) :-
		^^option(expires(Expires0), Options),
		Expires0 \== none,
		!,
		expires_value(Expires0, Value).
	expires_headers([], _Options).

	cache_control_value(Directives, Value) :-
		cache_control_directive_atoms(Directives, Atoms),
		atomic_list_concat(Atoms, ', ', Value).

	cache_control_directive_atoms([], []).
	cache_control_directive_atoms([Directive| Directives], [Atom| Atoms]) :-
		cache_control_directive_atom(Directive, Atom),
		cache_control_directive_atoms(Directives, Atoms).

	cache_control_directive_atom(public, 'public').
	cache_control_directive_atom(private, 'private').
	cache_control_directive_atom(no_cache, 'no-cache').
	cache_control_directive_atom(no_store, 'no-store').
	cache_control_directive_atom(no_transform, 'no-transform').
	cache_control_directive_atom(must_revalidate, 'must-revalidate').
	cache_control_directive_atom(proxy_revalidate, 'proxy-revalidate').
	cache_control_directive_atom(immutable, 'immutable').
	cache_control_directive_atom(max_age(Seconds), Atom) :-
		seconds_directive_atom('max-age=', Seconds, Atom).
	cache_control_directive_atom(s_maxage(Seconds), Atom) :-
		seconds_directive_atom('s-maxage=', Seconds, Atom).
	cache_control_directive_atom(stale_while_revalidate(Seconds), Atom) :-
		seconds_directive_atom('stale-while-revalidate=', Seconds, Atom).
	cache_control_directive_atom(stale_if_error(Seconds), Atom) :-
		seconds_directive_atom('stale-if-error=', Seconds, Atom).
	cache_control_directive_atom(extension(Directive), Directive).

	seconds_directive_atom(Prefix, Seconds, Atom) :-
		number_codes(Seconds, Codes),
		atom_codes(Suffix, Codes),
		atom_concat(Prefix, Suffix, Atom).

	expires_value(Seconds0, Value) :-
		integer(Seconds0),
		!,
		current_unix_time(CurrentTime),
		ExpiresTime is CurrentTime + Seconds0,
		http_date(ExpiresTime, Value).
	expires_value(DateTime, Value) :-
		date_time_to_unix(DateTime, ExpiresTime),
		http_date(ExpiresTime, Value).

	current_unix_time(CurrentTime) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), CurrentTime).

	vary_headers(true, [vary-'Accept-Encoding']) :-
		!.
	vary_headers(false, []).

	representation_headers(File, Options, [content_encoding-Encoding], MediaType) :-
		^^option(mime_types_strict(Strict), Options),
		mime_types::guess_file_type(File, Type, Encoding, Strict),
		Encoding \== '',
		!,
		guessed_media_type(Type, MediaType).
	representation_headers(File, Options, [], MediaType) :-
		^^option(mime_types_strict(Strict), Options),
		mime_types::guess_file_type(File, Type, _Encoding, Strict),
		guessed_media_type(Type, MediaType).

	resource_response(Request, not_acceptable(VaryAcceptEncoding), Response) :-
		!,
		not_acceptable_response(Request, VaryAcceptEncoding, Response).
	resource_response(Request, Resource, Response) :-
		request_not_modified(Request, Resource),
		!,
		not_modified_response(Request, Resource, Response).
	resource_response(Request, Resource, Response) :-
		request_header_value(Request, range, RangeValue),
		!,
		range_response(Request, RangeValue, Resource, Response).
	resource_response(Request, Resource, Response) :-
		full_response(Request, Resource, Response).

	full_response(Request, resource(File, MediaType, Headers, Size, _ModifiedTime, _ETag, _LastModified), Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), Headers, content(MediaType, file(File, 0, Size)), [], Response).

	range_response(Request, _RangeValue, Resource, Response) :-
		\+ if_range_matches(Request, Resource),
		!,
		full_response(Request, Resource, Response).
	range_response(Request, RangeValue, Resource, Response) :-
		resource_range(RangeValue, Resource, Start, End),
		!,
		partial_response(Request, Resource, Start, End, Response).
	range_response(Request, _RangeValue, Resource, Response) :-
		range_not_satisfiable_response(Request, Resource, Response).

	partial_response(Request, resource(File, MediaType, Headers0, Size, _ModifiedTime, _ETag, _LastModified), Start, End, Response) :-
		Length is End - Start + 1,
		content_range_value(Start, End, Size, ContentRange),
		prepend_header(content_range-ContentRange, Headers0, Headers),
		http_core::version(Request, Version),
		http_core::response(Version, status(206, 'Partial Content'), Headers, content(MediaType, file(File, Start, Length)), [], Response).

	not_modified_response(Request, resource(_File, MediaType, Headers0, _Size, _ModifiedTime, _ETag, _LastModified), Response) :-
		prepend_header(content_type-media_type(MediaType, []), Headers0, Headers),
		http_core::version(Request, Version),
		http_core::response(Version, status(304, 'Not Modified'), Headers, empty, [], Response).

	range_not_satisfiable_response(Request, resource(_File, _MediaType, Headers0, Size, _ModifiedTime, _ETag, _LastModified), Response) :-
		unsatisfied_content_range_value(Size, ContentRange),
		prepend_header(content_range-ContentRange, Headers0, Headers),
		http_core::version(Request, Version),
		http_core::response(Version, status(416, 'Range Not Satisfiable'), Headers, empty, [], Response).

	request_not_modified(Request, resource(_File, _MediaType, _Headers, _Size, _ModifiedTime, ETag, _LastModified)) :-
		request_header_value(Request, if_none_match, Value),
		!,
		if_none_match_matches(Value, ETag).
	request_not_modified(Request, resource(_File, _MediaType, _Headers, _Size, ModifiedTime, _ETag, _LastModified)) :-
		request_header_value(Request, if_modified_since, Value),
		http_date_seconds(Value, Seconds),
		ModifiedTime =< Seconds.

	resource_range(RangeValue, resource(_File, _MediaType, _Headers, Size, _ModifiedTime, _ETag, _LastModified), Start, End) :-
		parse_range_value(RangeValue, Size, Start, End).

	if_range_matches(Request, _Resource) :-
		\+ request_header_value(Request, if_range, _),
		!.
	if_range_matches(Request, resource(_File, _MediaType, _Headers, _Size, ModifiedTime, ETag, _LastModified)) :-
		request_header_value(Request, if_range, Value),
		trim_ows_atom(Value, TrimmedValue),
		(	strong_entity_tag_match(TrimmedValue, ETag) ->
			true
		;	http_date_seconds(TrimmedValue, Seconds),
			ModifiedTime =< Seconds
		).

	request_header_value(Request, Name, Value) :-
		http_core::header(Request, Name, Value),
		!.

	if_none_match_matches(Value, ETag) :-
		trim_ows_atom(Value, TrimmedValue),
		(	TrimmedValue == ('*') ->
			true
		;	atom::split(TrimmedValue, ',', Tags),
			etag_list_matches(Tags, ETag)
		).

	etag_list_matches([Tag| _], ETag) :-
		trim_ows_atom(Tag, TrimmedTag),
		weak_entity_tag_match(TrimmedTag, ETag),
		!.
	etag_list_matches([_| Tags], ETag) :-
		etag_list_matches(Tags, ETag).

	strong_entity_tag_match(Tag, ETag) :-
		\+ weak_entity_tag(Tag),
		\+ weak_entity_tag(ETag),
		Tag == ETag.

	weak_entity_tag(Tag) :-
		atom_codes(Tag, [0'W,0'/| _]).

	weak_entity_tag_match(Tag, ETag) :-
		normalized_entity_tag(Tag, NormalizedTag),
		normalized_entity_tag(ETag, NormalizedTag).

	normalized_entity_tag(Tag, NormalizedTag) :-
		atom_codes(Tag, [0'W,0'/| Codes]),
		!,
		atom_codes(NormalizedTag, Codes).
	normalized_entity_tag(Tag, Tag).

	parse_range_value(Value, Size, Start, End) :-
		Size > 0,
		trim_ows_atom(Value, TrimmedValue),
		atom_concat('bytes=', Spec, TrimmedValue),
		\+ sub_atom(Spec, _, 1, _, ','),
		atom::split(Spec, '-', [StartText, EndText]),
		atom_codes(StartText, StartCodes),
		atom_codes(EndText, EndCodes),
		range_bounds(StartCodes, EndCodes, Size, Start, End).

	range_bounds(StartCodes, EndCodes, Size, Start, End) :-
		StartCodes \== [],
		EndCodes \== [],
		decimal_codes_integer(StartCodes, Start),
		decimal_codes_integer(EndCodes, RequestedEnd),
		Start =< RequestedEnd,
		Start < Size,
		MaxEnd is Size - 1,
		(	RequestedEnd =< MaxEnd ->
			End = RequestedEnd
		;	End = MaxEnd
		).
	range_bounds(StartCodes, [], Size, Start, End) :-
		StartCodes \== [],
		decimal_codes_integer(StartCodes, Start),
		Start < Size,
		End is Size - 1.
	range_bounds([], EndCodes, Size, Start, End) :-
		EndCodes \== [],
		decimal_codes_integer(EndCodes, RequestedLength),
		RequestedLength > 0,
		(	RequestedLength =< Size ->
			Length = RequestedLength
		;	Length = Size
		),
		Start is Size - Length,
		End is Size - 1.

	content_range_value(Start, End, Size, ContentRange) :-
		number_codes(Start, StartCodes),
		number_codes(End, EndCodes),
		number_codes(Size, SizeCodes),
		append(StartCodes, [0'-| EndCodes], Codes0),
		append(Codes0, [0'/| SizeCodes], SuffixCodes),
		Codes = [0'b,0'y,0't,0'e,0's,32| SuffixCodes],
		atom_codes(ContentRange, Codes).

	unsatisfied_content_range_value(Size, ContentRange) :-
		number_codes(Size, SizeCodes),
		Codes = [0'b,0'y,0't,0'e,0's,32,0'*,0'/| SizeCodes],
		atom_codes(ContentRange, Codes).

	normalize_modification_time(ModifiedTime, NormalizedTime) :-
		(	integer(ModifiedTime) ->
			NormalizedTime = ModifiedTime
		;	float(ModifiedTime) ->
			NormalizedTime is floor(ModifiedTime)
		;	ModifiedTime = dt(Year, Month, Day, Hours, Minutes, Seconds),
			date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), ModifiedTime)
		).

	entity_tag(Size, ModifiedTime, ETag) :-
		number_codes(Size, SizeCodes),
		number_codes(ModifiedTime, TimeCodes),
		append(SizeCodes, [0'-| TimeCodes], Codes0),
		Codes1 = [0'W,0'/,0'"| Codes0],
		append(Codes1, [0'"], Codes),
		atom_codes(ETag, Codes).

	http_date(Seconds, Date) :-
		unix_to_date_time(Seconds, DateTime),
		format_date_time(DateTime, 0, http_date, Date).

	http_date_seconds(Date, Seconds) :-
		trim_ows_atom(Date, TrimmedDate),
		atom_codes(TrimmedDate, [_W1,_W2,_W3,0',,32,D1,D2,32,M1,M2,M3,32,Y1,Y2,Y3,Y4,32,H1,H2,0':,N1,N2,0':,S1,S2,32,0'G,0'M,0'T]),
		month_codes_token([M1,M2,M3], Month),
		decimal_codes_integer([D1,D2], Day),
		decimal_codes_integer([Y1,Y2,Y3,Y4], Year),
		decimal_codes_integer([H1,H2], Hours),
		decimal_codes_integer([N1,N2], Minutes),
		decimal_codes_integer([S1,S2], SecondsOfMinute),
		DateTime = date_time(Year, Month, Day, Hours, Minutes, SecondsOfMinute),
		valid_date_time(DateTime),
		format_date_time(DateTime, 0, http_date, TrimmedDate),
		date_time_to_unix(DateTime, Seconds).

	month_codes_token(Codes, Month) :-
		atom_codes(Token, Codes),
		memberchk(Token-Month, ['Jan'-1, 'Feb'-2, 'Mar'-3, 'Apr'-4, 'May'-5, 'Jun'-6, 'Jul'-7, 'Aug'-8, 'Sep'-9, 'Oct'-10, 'Nov'-11, 'Dec'-12]).

	decimal_codes_integer(Codes, Integer) :-
		Codes \== [],
		decimal_codes_integer(Codes, 0, Integer).

	decimal_codes_integer([], Integer, Integer).
	decimal_codes_integer([Code| Codes], Integer0, Integer) :-
		Code >= 0'0,
		Code =< 0'9,
		Digit is Code - 0'0,
		Integer1 is Integer0 * 10 + Digit,
		decimal_codes_integer(Codes, Integer1, Integer).

	trim_ows_atom(Atom, TrimmedAtom) :-
		atom_codes(Atom, Codes0),
		trim_ows_codes(Codes0, Codes),
		atom_codes(TrimmedAtom, Codes).

	trim_ows_codes(Codes0, Codes) :-
		trim_leading_ows_codes(Codes0, Codes1),
		reverse(Codes1, ReversedCodes1),
		trim_leading_ows_codes(ReversedCodes1, ReversedCodes),
		reverse(ReversedCodes, Codes).

	trim_leading_ows_codes([Code| Codes0], Codes) :-
		ows_code(Code),
		!,
		trim_leading_ows_codes(Codes0, Codes).
	trim_leading_ows_codes(Codes, Codes).

	ows_code(32).
	ows_code(0'\t).

	prepend_header(Header, Headers, [Header| Headers]).

	guessed_media_type('', 'application/octet-stream') :-
		!.
	guessed_media_type(Type, Type).

	not_found_response(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(404, 'Not Found'), [], content('text/plain', text('Not Found')), [], Response).

	not_acceptable_response(Request, VaryAcceptEncoding, Response) :-
		vary_headers(VaryAcceptEncoding, Headers),
		http_core::version(Request, Version),
		http_core::response(Version, status(406, 'Not Acceptable'), Headers, content('text/plain', text('Not Acceptable')), [], Response).

	method_not_allowed_response(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(405, 'Method Not Allowed'), [allow-'GET, HEAD'], content('text/plain', text('Method Not Allowed')), [], Response).

	valid_boolean_option(Boolean) :-
		once((
			Boolean == true;
			Boolean == false
		)).

	valid_cache_control_directives([]).
	valid_cache_control_directives([Directive| Directives]) :-
		ground(Directive),
		valid_cache_control_directive(Directive),
		valid_cache_control_directives(Directives).

	valid_cache_control_directive(public).
	valid_cache_control_directive(private).
	valid_cache_control_directive(no_cache).
	valid_cache_control_directive(no_store).
	valid_cache_control_directive(no_transform).
	valid_cache_control_directive(must_revalidate).
	valid_cache_control_directive(proxy_revalidate).
	valid_cache_control_directive(immutable).
	valid_cache_control_directive(max_age(Seconds)) :-
		valid_non_negative_integer(Seconds).
	valid_cache_control_directive(s_maxage(Seconds)) :-
		valid_non_negative_integer(Seconds).
	valid_cache_control_directive(stale_while_revalidate(Seconds)) :-
		valid_non_negative_integer(Seconds).
	valid_cache_control_directive(stale_if_error(Seconds)) :-
		valid_non_negative_integer(Seconds).
	valid_cache_control_directive(extension(Directive)) :-
		atom(Directive),
		Directive \== ''.

	valid_expires_value(Value) :-
		valid_non_negative_integer(Value).
	valid_expires_value(Value) :-
		valid_date_time(Value).

	valid_non_negative_integer(Value) :-
		integer(Value),
		Value >= 0.

	valid_atom_list(Atoms) :-
		var(Atoms),
		!,
		fail.
	valid_atom_list([]).
	valid_atom_list([Atom| Atoms]) :-
		atom(Atom),
		valid_atom_list(Atoms).

:- end_object.
