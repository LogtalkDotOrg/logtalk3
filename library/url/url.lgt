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


:- object(url(_Representation_)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-22,
		comment is 'URL validating, parsing, and normalizing predicates following RFC3986 nomenclature.',
		parameters is [
			'Representation' - 'URL and is components representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		]
	]).

	:- public(valid/1).
	:- mode(valid(++text), zero_or_one).
	:- info(valid/1, [
		comment is 'True iff the argument is a valid URL, including optional query and fragment components.',
		argnames is ['URL']
	]).

	:- public(parse/2).
	:- mode(parse(++text, -list(compound)), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses a URL into a list of its components: ``[scheme(Scheme), authority(Authority), path(Path), query(Query), fragment(Fragment)]``. Fails if the URL is invalid and cannot be parsed.',
		argnames is ['URL', 'Components']
	]).

	:- public(generate/2).
	:- mode(generate(++list(compound), -text), zero_or_one).
	:- info(generate/2, [
		comment is 'Generates a normalized URL or RFC3986 relative reference from a list of components. Component lists headed by ``scheme/1`` generate absolute URLs; lists without ``scheme/1`` generate relative references.',
		argnames is ['Components', 'URL']
	]).

	:- public(parse/3).
	:- mode(parse(++text, -list(compound), -atom), zero_or_one).
	:- info(parse/3, [
		comment is 'Parses a URL or RFC3986 relative reference into a list of its components and classifies it as one of ``url``, ``network_path``, ``absolute_path``, ``relative_path``, ``query``, or ``fragment``.',
		argnames is ['Reference', 'Components', 'Kind']
	]).

	:- public(reference_kind/2).
	:- mode(reference_kind(++text, -atom), zero_or_one).
	:- info(reference_kind/2, [
		comment is 'Classifies a valid reference as one of ``url``, ``network_path``, ``absolute_path``, ``relative_path``, ``query``, or ``fragment``.',
		argnames is ['Reference', 'Kind']
	]).

	:- public(equivalent/2).
	:- mode(equivalent(++text, ++text), zero_or_one).
	:- info(equivalent/2, [
		comment is 'True iff both arguments normalize to the same textual representation under the library normalization rules.',
		argnames is ['Reference1', 'Reference2']
	]).

	:- public(relativize/3).
	:- mode(relativize(++text, ++text, -text), zero_or_one).
	:- info(relativize/3, [
		comment is 'Relativizes a target absolute hierarchical URL against a base absolute hierarchical URL with the same scheme and authority, returning a non-empty relative reference accepted by the library. Fails when no such non-empty relative reference can be generated.',
		argnames is ['Base', 'Target', 'Relative']
	]).

	:- public(resolve/3).
	:- mode(resolve(++text, ++text, -text), zero_or_one).
	:- info(resolve/3, [
		comment is 'Resolves a reference against a base absolute hierarchical URL and returns a normalized absolute URL. The first argument must be an absolute hierarchical URL suitable for use as a base. The second argument may be either a relative reference or an absolute URL; when it is already absolute, the result is that absolute URL normalized and returned unchanged apart from normalization.',
		argnames is ['Base', 'Reference', 'Resolved']
	]).

	:- public(normalize/2).
	:- mode(normalize(++text, -text), one).
	:- info(normalize/2, [
		comment is 'Normalizes a URL or RFC3986 relative reference by standardizing its components. Normalization includes converting the scheme to lowercase when present, normalizing the authority, percent-encoding characters that require escaping, and simplifying path dot segments while preserving absolute and relative reference forms.',
		argnames is ['Reference', 'NormalizedReference']
	]).

	:- public(file_path_components/2).
	:- mode(file_path_components(++text, -list(compound)), one).
	:- info(file_path_components/2, [
		comment is 'Converts a file-system path into file URL components represented as ``[authority(Authority), path(Path)]``. Windows drive-letter and UNC paths are normalized to RFC3986-compatible file URL components.',
		argnames is ['FilePath', 'Components']
	]).

	:- uses(list, [
		append/3, member/2, reverse/2
	]).

	valid(URL) :-
		valid(_Representation_, URL),
		!.

	valid(atom, URL) :-
		atom_codes(URL, Codes),
		phrase(url, Codes).
	valid(chars, URL) :-
		chars_to_codes(URL, Codes),
		phrase(url, Codes).
	valid(codes, URL) :-
		phrase(url, URL).

	reference_kind(Reference, Kind) :-
		parse(Reference, _, Kind).

	equivalent(Reference1, Reference2) :-
		normalize(Reference1, NormalizedReference1),
		normalize(Reference2, NormalizedReference2),
		NormalizedReference1 == NormalizedReference2.

	relativize(Base, Target, Relative) :-
		normalized_absolute_reference_components(Base, [scheme(Scheme), authority(Authority)| BaseComponents]),
		normalized_absolute_reference_components(Target, [scheme(Scheme), authority(Authority)| TargetComponents]),
		empty_text(Empty),
		component_value_or_default(path, BaseComponents, Empty, BasePath),
		component_value_or_default(query, BaseComponents, Empty, BaseQuery),
		component_value_or_default(fragment, BaseComponents, Empty, BaseFragment),
		component_value_or_default(path, TargetComponents, Empty, TargetPath),
		component_value_or_default(query, TargetComponents, Empty, TargetQuery),
		component_value_or_default(fragment, TargetComponents, Empty, TargetFragment),
		relativize_reference_components(BasePath, BaseQuery, BaseFragment, TargetPath, TargetQuery, TargetFragment, Empty, RelativeComponents),
		build_url_from_components(RelativeComponents, Relative),
		Relative \== Empty,
		!.

	resolve(Base, Reference, Resolved) :-
		empty_text(Empty),
		parse(Base, [scheme(Scheme)| BaseComponents]),
		parse_reference_for_resolution(Reference, ReferenceComponents),
		resolve_reference_components(Scheme, BaseComponents, ReferenceComponents, Empty, ResolvedComponents),
		build_url_from_components(ResolvedComponents, Resolved),
		!.

	url --> scheme, [0':,0'/,0'/], authority, path, query, fragment.
	url --> file_scheme, [0':,0'/,0'/], file_authority, path.
	url --> mailto_scheme, [0':], email_address.
	url --> news_scheme, [0':], news_identifier.
	url --> tel_scheme, [0':], phone_number.
	url --> urn_scheme, [0':], urn_identifier.

	non_empty_relative_reference --> non_empty_input, relative_reference.

	non_empty_input, [Code] --> [Code].

	relative_reference --> relative_part, query, fragment.

	relative_part --> [0'/, 0'/], authority, path, !.
	relative_part --> path_absolute_reference, !.
	relative_part --> path_noscheme_reference, !.
	relative_part --> [].

	path_absolute_reference --> [0'/], path_absolute_reference_tail.

	path_absolute_reference_tail --> relative_path_segment_nz, relative_path_segments.
	path_absolute_reference_tail --> [].

	path_noscheme_reference --> relative_path_segment_nz_nc, relative_path_segments.

	relative_path_segments --> [0'/], !, relative_path_segment, relative_path_segments.
	relative_path_segments --> [].

	% RFC 3986 suffixes: _nz means non-zero length; _nc means no colon.

	relative_path_segment_nz --> [Code], {path_code(Code)}, !, relative_path_segment.

	relative_path_segment --> [Code], {path_code(Code)}, !, relative_path_segment.
	relative_path_segment --> [].

	relative_path_segment_nz_nc --> [Code], {path_noscheme_code(Code)}, !, relative_path_segment_nc.

	relative_path_segment_nc --> [Code], {path_noscheme_code(Code)}, !, relative_path_segment_nc.
	relative_path_segment_nc --> [].

	% web protocols
	scheme --> h, t, t, p, s.
	scheme --> h, t, t, p.
	scheme --> w, s, s.
	scheme --> w, s.
	% file transfer and version control
	scheme --> f, t, p, s.
	scheme --> f, t, p.
	scheme --> s, f, t, p.
	scheme --> g, i, t.
	% media streaming
	scheme --> m, m, s.
	scheme --> r, t, s, p.
	scheme --> r, t, m, p.
	% directory services
	scheme --> l, d, a, p, s.
	scheme --> l, d, a, p.
	% network services
	scheme --> s, s, h.
	scheme --> t, e, l, n, e, t.
	% databases
	scheme --> j, d, b, c.
	scheme --> m, o, n, g, o, d, b.
	scheme --> m, y, s, q, l.
	scheme --> p, o, s, t, g, r, e, s, q, l.
	% other protocols
	scheme --> g, o, p, h, e, r.
	scheme --> n, n, t, p.

	file_scheme --> f, i, l, e.

	mailto_scheme --> m, a, i, l, t, o.

	news_scheme --> n, e, w, s.

	tel_scheme --> t, e, l.

	urn_scheme --> u, r, n.

	email_address --> email_local_part, [0'@], email_domain.

	email_local_part --> email_codes.

	email_domain --> identifier, dot_identifiers.

	email_codes --> [Code], {email_code(Code)}, !, email_codes.
	email_codes --> [].

	email_code(Code) :- Code =\= 0'@, Code =\= 0'?, Code =\= 0'#, Code >= 33, Code =< 126.

	% News identifier (newsgroup name or message-id)
	news_identifier --> [Code], {news_code(Code)}, !, news_identifier.
	news_identifier --> [].

	news_code(Code) :- Code =\= 0'?, Code =\= 0'#, Code >= 33, Code =< 126.

	% Tel (phone number) - allows digits, hyphens, plus, parentheses, spaces
	phone_number --> [Code], {phone_code(Code)}, !, phone_number.
	phone_number --> [].

	phone_code(Code) :- Code @>= 0'0, Code @=< 0'9, !.
	phone_code(0'+).
	phone_code(0'-).
	phone_code(0'().
	phone_code(0')).
	phone_code(0'.).
	phone_code(32).

	% URN identifier (namespace-specific string)
	urn_identifier --> [Code], {urn_code(Code)}, !, urn_identifier.
	urn_identifier --> [].

	urn_code(Code) :- Code =\= 0'?, Code =\= 0'#, Code >= 33, Code =< 126.

	authority --> userinfo, [0'@], !, host, port.
	authority --> host, port.

	file_authority -->
		host.

	userinfo --> [Code], {userinfo_code(Code)}, !, userinfo.
	userinfo --> [].

	userinfo_code(Code) :-
		pchar_code(Code),
		Code =\= 0'@.

	host --> ip_literal.
	host --> ip_number.
	host --> reg_name.

	ip_literal --> [0'[], ipvfuture_literal, [0']].
	ip_literal --> [0'[], ipv6_literal, [0']].

	ipv6_literal --> [Code], {ipv6_literal_code(Code)}, !, ipv6_literal_rest.

	ipv6_literal_rest --> [Code], {ipv6_literal_code(Code)}, !, ipv6_literal_rest.
	ipv6_literal_rest --> [].

	ipv6_literal_code(Code) :- hex_digit_code(Code), !.
	ipv6_literal_code(0':).
	ipv6_literal_code(0'.).

	ipvfuture_literal --> ipvfuture_v, ipvfuture_hex_digits, [0'.], ipvfuture_body.

	ipvfuture_v --> [0'v].
	ipvfuture_v --> [0'V].

	ipvfuture_hex_digits --> [Code], {hex_digit_code(Code)}, !, ipvfuture_hex_digits_rest.

	ipvfuture_hex_digits_rest --> [Code], {hex_digit_code(Code)}, !, ipvfuture_hex_digits_rest.
	ipvfuture_hex_digits_rest --> [].

	ipvfuture_body --> [Code], {ipvfuture_body_code(Code)}, !, ipvfuture_body_rest.

	ipvfuture_body_rest --> [Code], {ipvfuture_body_code(Code)}, !, ipvfuture_body_rest.
	ipvfuture_body_rest --> [].

	ipvfuture_body_code(Code) :- unreserved_code(Code), !.
	ipvfuture_body_code(Code) :- sub_delimiter_code(Code), !.
	ipvfuture_body_code(0':).

	reg_name --> [Code], {reg_name_code(Code)}, !, reg_name.
	reg_name --> [].

	reg_name_code(Code) :- unreserved_code(Code), !.
	reg_name_code(Code) :- sub_delimiter_code(Code), !.
	reg_name_code(0'%).

	ip_number --> ip_subnumber, [0'.], ip_subnumber, [0'.], ip_subnumber, [0'.], ip_subnumber.

	ip_subnumber --> digit(N1), digit(N2), digit(N3), {N is N3 - 0'0 + 10*(N2 - 0'0) + 100*(N1 - 0'0), N >= 0, N =< 255}.
	ip_subnumber --> digit(N1), digit(N2), {N is N2 - 0'0 + 10*(N1 - 0'0), N >= 0, N =< 255}.
	ip_subnumber --> digit(N1), {N is N1 - 0'0, N >= 0, N =< 255}.

	port --> [0':], !, integer(Port), {0 =< Port, Port =< 65535}.
	port --> [].

	integer(Integer) -->
		digit(Digit), digits(Digits),
		{number_codes(Integer, [Digit| Digits])}.

	digit(Digit) -->
		[Digit],
		{0'0 @=< Digit, Digit @=< 0'9}.

	digits([Digit| Digits]) -->
		digit(Digit), !, digits(Digits).
	digits([]) -->
		[].

	identifier --> codes.

	codes --> [Code], {code(Code)}, !, codes.
	codes --> [].

	code(Code) :- Code @>= 0'a, Code @=< 0'z, !.
	code(Code) :- Code @>= 0'A, Code @=< 0'Z.
	code(Code) :- Code =:= 0'_.
	code(Code) :- Code =:= 0'-.
	code(Code) :- Code @>= 0'0, Code @=< 0'9.

	% Path characters can include dots and other valid URL path characters
	path_code(Code) :- code(Code), !.
	path_code(0'.).   % dot for file extensions
	path_code(0':).   % colon for Windows paths and other uses
	path_code(0'%).   % percent for percent-encoding
	path_code(0'@).   % at sign
	path_code(0'!).   % exclamation
	path_code(0'$).   % dollar
	path_code(0'&).   % ampersand
	path_code(0'\').  % single quote
	path_code(0'().   % left paren
	path_code(0')).   % right paren
	path_code(0'*).   % asterisk
	path_code(0'+).   % plus
	path_code(0',).   % comma
	path_code(0';).   % semicolon
	path_code(0'=).   % equals
	path_code(0'~).   % tilde

	path_noscheme_code(Code) :-
		path_code(Code),
		Code =\= 0':.

	dot_identifiers --> [0'.], !, identifier, dot_identifiers.
	dot_identifiers --> [].

	path --> [0'/], path_segment, path.
	path --> [].

	path_segment --> [Code], {path_code(Code)}, !, path_segment.
	path_segment --> [].

	query --> [0'?], !, query_codes.
	query --> [].

	query_codes --> [Code], {query_code(Code)}, !, query_codes.
	query_codes --> [].

	query_code(Code) :- Code =\= 0'#, Code >= 32, Code =< 126.

	fragment --> [0'#], !, fragment_codes.
	fragment --> [].

	fragment_codes --> [Code], {fragment_code(Code)}, !, fragment_codes.
	fragment_codes --> [].

	fragment_code(Code) :- Code >= 32, Code =< 126.

	parse(URL, Components) :-
		parse_absolute(_Representation_, URL, Components).

	parse(Reference, Components, Kind) :-
		parse_reference(_Representation_, Reference, Components, Kind).

	parse_absolute(atom, URL, [scheme(Scheme)| Components]) :-
		atom_codes(URL, Codes),
		phrase(scheme(Scheme), Codes, Rest),
		parse_url(Scheme, Rest, Components),
		!.
	parse_absolute(chars, URL, [scheme(Scheme)| Components]) :-
		chars_to_codes(URL, Codes),
		phrase(scheme(Scheme), Codes, Rest),
		atom_chars(SchemeAtom, Scheme),
		parse_url(SchemeAtom, Rest, Components),
		!.
	parse_absolute(codes, URL, [scheme(Scheme)| Components]) :-
		phrase(scheme(Scheme), URL, Rest),
		atom_codes(SchemeAtom, Scheme),
		parse_url(SchemeAtom, Rest, Components),
		!.

	parse_reference(atom, URL, Components, url) :-
		parse_absolute(atom, URL, Components),
		!.
	parse_reference(atom, URL, Components, Kind) :-
		atom_codes(URL, Codes),
		parse_relative_reference(Codes, Components),
		components_reference_kind(Components, Kind),
		!.
	parse_reference(chars, URL, Components, url) :-
		parse_absolute(chars, URL, Components),
		!.
	parse_reference(chars, URL, Components, Kind) :-
		chars_to_codes(URL, Codes),
		parse_relative_reference(Codes, Components),
		components_reference_kind(Components, Kind),
		!.
	parse_reference(codes, URL, Components, url) :-
		parse_absolute(codes, URL, Components),
		!.
	parse_reference(codes, URL, Components, Kind) :-
		parse_relative_reference(URL, Components),
		components_reference_kind(Components, Kind),
		!.

	parse_url(http, Codes, [authority(Authority), path(Path), query(Query), fragment(Fragment)]) :-
		phrase(http_components(Authority, Path, Query, Fragment), Codes).
	parse_url(https, Codes, [authority(Authority), path(Path), query(Query), fragment(Fragment)]) :-
		phrase(http_components(Authority, Path, Query, Fragment), Codes).
	parse_url(ws, Codes, [authority(Authority), path(Path), query(Query), fragment(Fragment)]) :-
		phrase(http_components(Authority, Path, Query, Fragment), Codes).
	parse_url(wss, Codes, [authority(Authority), path(Path), query(Query), fragment(Fragment)]) :-
		phrase(http_components(Authority, Path, Query, Fragment), Codes).
	parse_url(ftp, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(ftps, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(sftp, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(ssh, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(ldap, Codes, [authority(Authority), path(Path), query(Query)]) :-
		phrase(ldap_components(Authority, Path, Query), Codes).
	parse_url(ldaps, Codes, [authority(Authority), path(Path), query(Query)]) :-
		phrase(ldap_components(Authority, Path, Query), Codes).
	parse_url(telnet, Codes, [authority(Authority), path(Path)]) :-
		phrase(telnet_components(Authority, Path), Codes).
	parse_url(rtsp, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(rtmp, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(mms, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(gopher, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(nntp, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(git, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(jdbc, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(mongodb, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(mysql, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(postgresql, Codes, [authority(Authority), path(Path)]) :-
		phrase(ftp_components(Authority, Path), Codes).
	parse_url(file, Codes, [authority(Authority), path(Path)]) :-
		phrase(file_components(Authority, Path), Codes).
	parse_url(mailto, Codes, [address(Address)]) :-
		phrase(mailto_components(Address), Codes).
	parse_url(news, Codes, [identifier(Identifier)]) :-
		phrase(news_components(Identifier), Codes).
	parse_url(tel, Codes, [number(Number)]) :-
		phrase(tel_components(Number), Codes).
	parse_url(urn, Codes, [identifier(Identifier)]) :-
		phrase(urn_components(Identifier), Codes).

	parse_relative_reference(Codes, Components) :-
		phrase(non_empty_relative_reference, Codes),
		relative_reference_components(Codes, Components).

	relative_reference_components(Codes, Components) :-
		split_fragment_codes(Codes, ReferenceCodes, FragmentCodes),
		split_query_codes(ReferenceCodes, MainCodes, QueryCodes),
		relative_reference_main_components(MainCodes, MainComponents),
		relative_reference_tail_components(QueryCodes, FragmentCodes, TailComponents),
		append(MainComponents, TailComponents, Components).

	relative_reference_main_components([0'/,0'/| AuthorityPathCodes], Components) :-
		!,
		split_authority_path_codes(AuthorityPathCodes, AuthorityCodes, PathCodes),
		convert_to_text(_Representation_, AuthorityCodes, Authority),
		( PathCodes == [] ->
			Components = [authority(Authority)]
		; 	convert_to_text(_Representation_, PathCodes, Path),
			Components = [authority(Authority), path(Path)]
		).
	relative_reference_main_components(MainCodes, [path(Path)]) :-
		MainCodes \== [],
		!,
		convert_to_text(_Representation_, MainCodes, Path).
	relative_reference_main_components([], []).

	relative_reference_tail_components(QueryCodes, FragmentCodes, Components) :-
		convert_to_text(_Representation_, QueryCodes, Query),
		convert_to_text(_Representation_, FragmentCodes, Fragment),
		( QueryCodes == [] ->
			( FragmentCodes == [] ->
				Components = []
			; 	Components = [fragment(Fragment)]
			)
		; 	( FragmentCodes == [] ->
				Components = [query(Query)]
			; 	Components = [query(Query), fragment(Fragment)]
			)
		).

	split_fragment_codes(Codes, ReferenceCodes, FragmentCodes) :-
		append(ReferenceCodes, [0'#| FragmentCodes], Codes),
		\+ member(0'#, ReferenceCodes),
		!.
	split_fragment_codes(Codes, Codes, []).

	split_query_codes(Codes, MainCodes, QueryCodes) :-
		append(MainCodes, [0'?| QueryCodes], Codes),
		\+ member(0'?, MainCodes),
		!.
	split_query_codes(Codes, Codes, []).

	split_authority_path_codes(Codes, AuthorityCodes, [0'/| PathCodes]) :-
		append(AuthorityCodes, [0'/| PathCodes], Codes),
		\+ member(0'/, AuthorityCodes),
		!.
	split_authority_path_codes(Codes, Codes, []).

	scheme(Scheme) -->
		scheme_codes(SchemeCodes),
		{convert_to_text(_Representation_, SchemeCodes, Scheme)}.

	% scheme is case-insensitive
	scheme_codes([0'h, 0't, 0't, 0'p])                               --> h, t, t, p, [0':,0'/,0'/].
	scheme_codes([0'h, 0't, 0't, 0'p, 0's])                          --> h, t, t, p, s, [0':,0'/,0'/].
	scheme_codes([0'w, 0's])                                         --> w, s, [0':,0'/,0'/].
	scheme_codes([0'w, 0's, 0's])                                    --> w, s, s, [0':,0'/,0'/].
	scheme_codes([0'f, 0't, 0'p])                                    --> f, t, p, [0':,0'/,0'/].
	scheme_codes([0'f, 0't, 0'p, 0's])                               --> f, t, p, s, [0':,0'/,0'/].
	scheme_codes([0's, 0'f, 0't, 0'p])                               --> s, f, t, p, [0':,0'/,0'/].
	scheme_codes([0's, 0's, 0'h])                                    --> s, s, h, [0':,0'/,0'/].
	scheme_codes([0'l, 0'd, 0'a, 0'p])                               --> l, d, a, p, [0':,0'/,0'/].
	scheme_codes([0'l, 0'd, 0'a, 0'p, 0's])                          --> l, d, a, p, s, [0':,0'/,0'/].
	scheme_codes([0't, 0'e, 0'l, 0'n, 0'e, 0't])                     --> t, e, l, n, e, t, [0':,0'/,0'/].
	scheme_codes([0'r, 0't, 0's, 0'p])                               --> r, t, s, p, [0':,0'/,0'/].
	scheme_codes([0'r, 0't, 0'm, 0'p])                               --> r, t, m, p, [0':,0'/,0'/].
	scheme_codes([0'm, 0'm, 0's])                                    --> m, m, s, [0':,0'/,0'/].
	scheme_codes([0'g, 0'o, 0'p, 0'h, 0'e, 0'r])                     --> g, o, p, h, e, r, [0':,0'/,0'/].
	scheme_codes([0'n, 0'n, 0't, 0'p])                               --> n, n, t, p, [0':,0'/,0'/].
	scheme_codes([0'g, 0'i, 0't])                                    --> g, i, t, [0':,0'/,0'/].
	scheme_codes([0'j, 0'd, 0'b, 0'c])                               --> j, d, b, c, [0':,0'/,0'/].
	scheme_codes([0'm, 0'o, 0'n, 0'g, 0'o, 0'd, 0'b])                --> m, o, n, g, o, d, b, [0':,0'/,0'/].
	scheme_codes([0'm, 0'y, 0's, 0'q, 0'l])                          --> m, y, s, q, l, [0':,0'/,0'/].
	scheme_codes([0'p, 0'o, 0's, 0't, 0'g, 0'r, 0'e, 0's, 0'q, 0'l]) --> p, o, s, t, g, r, e, s, q, l, [0':,0'/,0'/].
	scheme_codes([0'f, 0'i, 0'l, 0'e])                               --> f, i, l, e, [0':,0'/,0'/].
	scheme_codes([0'm, 0'a, 0'i, 0'l, 0't, 0'o])                     --> m, a, i, l, t, o, [0':].
	scheme_codes([0'n, 0'e, 0'w, 0's])                               --> n, e, w, s, [0':].
	scheme_codes([0't, 0'e, 0'l])                                    --> t, e, l, [0':].
	scheme_codes([0'u, 0'r, 0'n])                                    --> u, r, n, [0':].

	a --> [0'a].
	a --> [0'A].

	b --> [0'b].
	b --> [0'B].

	c --> [0'c].
	c --> [0'C].

	d --> [0'd].
	d --> [0'D].

	e --> [0'e].
	e --> [0'E].

	f --> [0'f].
	f --> [0'F].

	g --> [0'g].
	g --> [0'G].

	h --> [0'h].
	h --> [0'H].

	i --> [0'i].
	i --> [0'I].

	j --> [0'j].
	j --> [0'J].

	l --> [0'l].
	l --> [0'L].

	m --> [0'm].
	m --> [0'M].

	n --> [0'n].
	n --> [0'N].

	o --> [0'o].
	o --> [0'O].

	p --> [0'p].
	p --> [0'P].

	q --> [0'q].
	q --> [0'Q].

	r --> [0'r].
	r --> [0'R].

	s --> [0's].
	s --> [0'S].

	t --> [0't].
	t --> [0'T].

	u --> [0'u].
	u --> [0'U].

	w --> [0'w].
	w --> [0'W].

	y --> [0'y].
	y --> [0'Y].

	http_components(Authority, Path, Query, Fragment) -->
		authority_component(Authority),
		path_component(Path),
		query_component(Query),
		fragment_component(Fragment).

	ftp_components(Authority, Path) -->
		authority_component(Authority),
		path_component(Path).

	file_components(Authority, Path) -->
		file_authority_component(Authority),
		path_component(Path).

	ldap_components(Authority, Path, Query) -->
		authority_component(Authority),
		path_component(Path),
		query_component(Query).

	telnet_components(Authority, Path) -->
		authority_component(Authority),
		path_component(Path).

	mailto_components(Address) -->
		email_address(Address).

	news_components(Identifier) -->
		news_identifier_component(Identifier).

	tel_components(Number) -->
		phone_number_component(Number).

	urn_components(Identifier) -->
		urn_identifier_component(Identifier).

	authority_component(Authority) -->
		authority_codes(AuthorityCodes),
		{convert_to_text(_Representation_, AuthorityCodes, Authority)}.

	file_authority_component(Authority) -->
		authority_codes(AuthorityCodes),
		{phrase(file_authority, AuthorityCodes), convert_to_text(_Representation_, AuthorityCodes, Authority)}.

	authority_codes([Code| Codes]) -->
		[Code],
		{Code =\= 0'/, Code =\= 0'?, Code =\= 0'#},
		!,
		authority_codes(Codes).
	authority_codes([]) -->
		[].

	path_component(Path) -->
		[0'/], !,
		path_codes(Codes),
		{convert_to_text(_Representation_, [0'/| Codes], Path)}.
	path_component(Path) -->
		{convert_to_text(_Representation_, [], Path)}.

	news_identifier_component(Identifier) -->
		remaining_codes(Codes),
		{convert_to_text(_Representation_, Codes, Identifier)}.

	phone_number_component(Number) -->
		remaining_codes(Codes),
		{convert_to_text(_Representation_, Codes, Number)}.

	urn_identifier_component(Identifier) -->
		remaining_codes(Codes),
		{convert_to_text(_Representation_, Codes, Identifier)}.

	query_component(Query) -->
		[0'?], !,
		codes_until(0'#, QueryCodes),
		{convert_to_text(_Representation_, QueryCodes, Query)}.
	query_component(Query) -->
		{convert_to_text(_Representation_, [], Query)}.

	fragment_component(Fragment) -->
		[0'#], !,
		remaining_codes(FragmentCodes),
		{convert_to_text(_Representation_, FragmentCodes, Fragment)}.
	fragment_component(Fragment) -->
		{convert_to_text(_Representation_, [], Fragment)}.

	remaining_codes([]) -->
		[].
	remaining_codes([C|Cs]) -->
		[C],
		remaining_codes(Cs).

	codes_until(End, [C|Cs]) -->
		[C],
		{ End \== C },
		!,
		codes_until(End, Cs).
	codes_until(_, []) -->
		[].

	path_codes([Code| Codes]) -->
		[Code],
		{ Code \== 0'?, Code \== 0'# },
		!,
		path_codes(Codes).
	path_codes([]) -->
		[].

	email_address(Address) -->
		email_local_part(Local), [0'@], email_domain(Domain),
		{append(Local, [0'@| Domain], Codes), convert_to_text(_Representation_, Codes, Address)}.

	email_local_part(Codes) --> email_codes(Codes).

	email_domain(Codes) -->
		identifier(Codes0), dot_identifiers(Codes1),
		{append(Codes0, Codes1, Codes)}.

	email_codes([Code| Codes]) -->
		[Code], {email_code(Code)}, !, email_codes(Codes).
	email_codes([]) -->
		[].

	identifier(Codes) --> codes(Codes).

	codes([Code| Codes]) -->
		[Code], {code(Code)}, !, codes(Codes).
	codes([]) -->
		[].

	dot_identifiers(Codes) -->
		[0'.], identifier(Codes0), {append([0'.| Codes0], Codes1, Codes)}, dot_identifiers(Codes1).
	dot_identifiers([]) -->
		[].

	generate(Components, URL) :-
		build_url_from_components(Components, URL),
		!.

	normalize(URL, NormalizedURL) :-
		parse(URL, Components, _),
		build_url_from_components(Components, NormalizedURL),
		!.

	file_path_components(FilePath, [authority(Authority), path(Path)]) :-
		convert_to_text(_Representation_, FilePathCodes0, FilePath),
		normalize_file_path_codes(FilePathCodes0, FilePathCodes),
		file_path_components_codes(FilePathCodes, AuthorityCodes, PathCodes),
		convert_to_text(_Representation_, AuthorityCodes, Authority),
		convert_to_text(_Representation_, PathCodes, Path).

	normalize_authority(Authority, NormalizedAuthority) :-
		convert_to_text(_Representation_, AuthorityCodes, Authority),
		normalize_authority_codes(AuthorityCodes, NormalizedAuthorityCodes),
		convert_to_text(_Representation_, NormalizedAuthorityCodes, NormalizedAuthority).

	normalize_authority_codes(AuthorityCodes, NormalizedAuthorityCodes) :-
		split_authority_userinfo_codes(AuthorityCodes, UserInfoCodes, HostPortCodes),
		split_host_port_codes(HostPortCodes, HostCodes, PortCodes),
		downcase_codes(HostCodes, NormalizedHostCodes),
		append(UserInfoCodes, NormalizedHostCodes, NormalizedAuthorityCodes0),
		append(NormalizedAuthorityCodes0, PortCodes, NormalizedAuthorityCodes).

	split_authority_userinfo_codes(Codes, UserInfoCodes, HostPortCodes) :-
		append(UserInfoCodes0, [0'@| HostPortCodes], Codes),
		\+ member(0'@, HostPortCodes),
		!,
		append(UserInfoCodes0, [0'@], UserInfoCodes).
	split_authority_userinfo_codes(Codes, [], Codes).

	split_host_port_codes([0'[| Codes], HostCodes, PortCodes) :-
		append(HostBodyCodes, [0']| RemainingCodes], Codes),
		!,
		append([0'[| HostBodyCodes], [0']], HostCodes),
		( RemainingCodes = [0':| PortDigitCodes] ->
			PortCodes = [0':| PortDigitCodes]
		; 	PortCodes = []
		).
	split_host_port_codes(Codes, HostCodes, PortCodes) :-
		append(HostCodes, [0':| PortDigitCodes], Codes),
		PortDigitCodes \== [],
		\+ member(0':, PortDigitCodes),
		!,
		PortCodes = [0':| PortDigitCodes].
	split_host_port_codes(Codes, Codes, []).

	normalize_path(Path, NormalizedPath) :-
		convert_to_text(_Representation_, [0'/], Path),
		!,
		convert_to_text(_Representation_, [0'/], NormalizedPath).
	normalize_path(Path, NormalizedPath) :-
		convert_to_text(_Representation_, PathCodes, Path),
		normalize_path_codes(PathCodes, NormalizedPathCodes0),
		preserve_absolute_path_codes(PathCodes, NormalizedPathCodes0, NormalizedPathCodes),
		convert_to_text(_Representation_, NormalizedPathCodes, NormalizedPath).

	preserve_absolute_path_codes([0'/|_], [0'/|_]= NormalizedPathCodes, NormalizedPathCodes) :-
		!.
	preserve_absolute_path_codes([0'/|_], NormalizedPathCodes, [0'/| NormalizedPathCodes]) :-
		!.
	preserve_absolute_path_codes(_, NormalizedPathCodes, NormalizedPathCodes).

	normalize_file_path_codes([], []).
	normalize_file_path_codes([0'\\| Codes0], [0'/| Codes]) :-
		!,
		normalize_file_path_codes(Codes0, Codes).
	normalize_file_path_codes([Code| Codes0], [Code| Codes]) :-
		normalize_file_path_codes(Codes0, Codes).

	file_path_components_codes([0'/, 0'/| Codes], AuthorityCodes, [0'/| ShareCodes]) :-
		append(AuthorityCodes, [0'/| ShareCodes], Codes),
		AuthorityCodes \== [],
		\+ member(0'/, AuthorityCodes),
		!.
	file_path_components_codes([Drive, 0':| Rest], [], [0'/, Drive, 0':| PathCodes]) :-
		drive_letter_code(Drive),
		drive_file_path_codes(Rest, PathCodes),
		!.
	file_path_components_codes([0'/| _]= Codes, [], Codes) :-
		!.
	file_path_components_codes(Codes, [], Codes).

	drive_file_path_codes([0'/| Codes], [0'/| Codes]) :-
		!.
	drive_file_path_codes(Codes, Codes).

	drive_letter_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z,
		!.
	drive_letter_code(Code) :-
		Code >= 0'a,
		Code =< 0'z.

	normalize_query(Query, NormalizedQuery) :-
		normalize_text(query, Query, NormalizedQuery).

	normalize_fragment(Fragment, NormalizedFragment) :-
		normalize_text(fragment, Fragment, NormalizedFragment).

	normalize_address(Address, NormalizedAddress) :-
		normalize_text(mailto_address, Address, NormalizedAddress).

	normalize_number(Number, NormalizedNumber) :-
		normalize_text(phone_number, Number, NormalizedNumber).

	normalize_identifier(Identifier, NormalizedIdentifier) :-
		normalize_text(opaque_identifier, Identifier, NormalizedIdentifier).

	normalize_text(Component, Text, NormalizedText) :-
		convert_to_text(_Representation_, Codes, Text),
		normalize_component_codes(Component, Codes, NormalizedCodes),
		convert_to_text(_Representation_, NormalizedCodes, NormalizedText).

	normalize_path_codes(PathCodes, NormalizedPathCodes) :-
		% Check if path ends with a slash
		(	append(_, [0'/], PathCodes) ->
			TrailingSlash = true
		;	TrailingSlash = false
		),
		(	PathCodes = [0'/| _] ->
			Absolute = true
		;	Absolute = false
		),
		% Split path into segments
		split_path(PathCodes, Segments),
		% Process segments (remove '.', handle '..')
		process_segments(Segments, Absolute, ProcessedSegments),
		encode_path_segments(ProcessedSegments, EncodedSegments),
		% Join segments back into a path
		join_segments(EncodedSegments, TrailingSlash, NormalizedPathCodes).

	encode_path_segments([], []).
	encode_path_segments([Segment| Segments], [EncodedSegment| EncodedSegments]) :-
		normalize_component_codes(path_segment, Segment, EncodedSegment),
		encode_path_segments(Segments, EncodedSegments).

	normalize_component_codes(_, [], []) :-
		!.
	normalize_component_codes(Component, [0'%, High0, Low0| Codes], [Code| NormalizedCodes]) :-
		normalized_hex_code(High0, High),
		normalized_hex_code(Low0, Low),
		hex_digit_code_to_decimal(High, HighValue),
		hex_digit_code_to_decimal(Low, LowValue),
		Code is 16*HighValue + LowValue,
		unreserved_code(Code),
		allowed_component_code(Component, Code),
		!,
		normalize_component_codes(Component, Codes, NormalizedCodes).
	normalize_component_codes(Component, [0'%, High0, Low0| Codes], [0'%, High, Low| NormalizedCodes]) :-
		normalized_hex_code(High0, High),
		normalized_hex_code(Low0, Low),
		!,
		normalize_component_codes(Component, Codes, NormalizedCodes).
	normalize_component_codes(Component, [Code| Codes], [Code| NormalizedCodes]) :-
		allowed_component_code(Component, Code),
		!,
		normalize_component_codes(Component, Codes, NormalizedCodes).
	normalize_component_codes(Component, [Code| Codes], [0'%, High, Low| NormalizedCodes]) :-
		percent_encoded_byte(Code, High, Low),
		!,
		normalize_component_codes(Component, Codes, NormalizedCodes).
	normalize_component_codes(Component, [Code| Codes], [Code| NormalizedCodes]) :-
		Code > 255,
		normalize_component_codes(Component, Codes, NormalizedCodes).

	allowed_component_code(path_segment, Code) :-
		pchar_code(Code).
	allowed_component_code(query, Code) :-
		pchar_code(Code).
	allowed_component_code(query, 0'/).
	allowed_component_code(query, 0'?).
	allowed_component_code(fragment, Code) :-
		pchar_code(Code).
	allowed_component_code(fragment, 0'/).
	allowed_component_code(fragment, 0'?).
	allowed_component_code(mailto_address, Code) :-
		email_code(Code).
	allowed_component_code(mailto_address, 0'@).
	allowed_component_code(phone_number, Code) :-
		phone_number_output_code(Code).
	allowed_component_code(opaque_identifier, Code) :-
		opaque_identifier_code(Code).

	% RFC3986 pchar production used by path segments and as part of query/fragment.
	pchar_code(Code) :-
		unreserved_code(Code),
		!.
	pchar_code(Code) :-
		sub_delimiter_code(Code),
		!.
	pchar_code(0':).
	pchar_code(0'@).

	unreserved_code(Code) :-
		code(Code),
		!.
	unreserved_code(0'.).
	unreserved_code(0'~).

	sub_delimiter_code(0'!).
	sub_delimiter_code(0'$).
	sub_delimiter_code(0'&).
	sub_delimiter_code(0'\').
	sub_delimiter_code(0'().
	sub_delimiter_code(0')).
	sub_delimiter_code(0'*).
	sub_delimiter_code(0'+).
	sub_delimiter_code(0',).
	sub_delimiter_code(0';).
	sub_delimiter_code(0'=).

	phone_number_output_code(Code) :-
		Code @>= 0'0,
		Code @=< 0'9,
		!.
	phone_number_output_code(0'+).
	phone_number_output_code(0'-).
	phone_number_output_code(0'().
	phone_number_output_code(0')).
	phone_number_output_code(0'.).

	opaque_identifier_code(Code) :-
		Code =\= 0'?,
		Code =\= 0'#,
		Code >= 33,
		Code =< 126.

	normalized_hex_code(Code, Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	normalized_hex_code(Code, Code) :-
		Code >= 0'A,
		Code =< 0'F,
		!.
	normalized_hex_code(Code, UpperCode) :-
		Code >= 0'a,
		Code =< 0'f,
		UpperCode is Code - 32.

	hex_digit_code_to_decimal(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hex_digit_code_to_decimal(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		Value is Code - 0'A + 10.

	hex_digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	hex_digit_code(Code) :-
		Code >= 0'a,
		Code =< 0'f,
		!.
	hex_digit_code(Code) :-
		Code >= 0'A,
		Code =< 0'F.

	percent_encoded_byte(Code, High, Low) :-
		Code >= 0,
		Code =< 255,
		HighNibble is Code // 16,
		LowNibble is Code mod 16,
		decimal_to_hex_digit_code(HighNibble, High),
		decimal_to_hex_digit_code(LowNibble, Low).

	decimal_to_hex_digit_code(Value, Code) :-
		Value >= 0,
		Value =< 9,
		!,
		Code is 0'0 + Value.
	decimal_to_hex_digit_code(Value, Code) :-
		Code is 0'A + Value - 10.

	split_path(PathCodes, Segments) :-
		split_path(PathCodes, [], Segments).

	split_path([], CurrentSegment, [CurrentSegment]).
	split_path([0'/|Rest], CurrentSegment, [CurrentSegment|Segments]) :-
		!,
		split_path(Rest, [], Segments).
	split_path([Code|Rest], CurrentSegment, Segments) :-
		append(CurrentSegment, [Code], NewSegment),
		split_path(Rest, NewSegment, Segments).

	process_segments(Segments, Absolute, ProcessedSegments) :-
		process_segments(Segments, Absolute, [], ProcessedSegments).

	process_segments([], _, Acc, Processed) :-
		reverse(Acc, Processed).
	process_segments([[]|Segments], Absolute, Acc, Processed) :-
		!,
		% Skip empty segments
		process_segments(Segments, Absolute, Acc, Processed).
	process_segments([[0'., 0'.]|Segments], Absolute, Acc, Processed) :-
		!,
		process_parent_segment(Absolute, Acc, NextAcc),
		process_segments(Segments, Absolute, NextAcc, Processed).
	process_segments([[0'.]|Segments], Absolute, Acc, Processed) :-
		!,
		% Skip '.' segments
		process_segments(Segments, Absolute, Acc, Processed).
	process_segments([Segment|Segments], Absolute, Acc, Processed) :-
		% Keep normal segments
		process_segments(Segments, Absolute, [Segment|Acc], Processed).

	process_parent_segment(_, [Segment|Acc], Acc) :-
		Segment \== [0'., 0'.],
		!.
	process_parent_segment(true, Acc, Acc) :-
		!.
	process_parent_segment(false, Acc, [[0'., 0'.]|Acc]).

	join_segments(Segments, TrailingSlash, PathCodes) :-
		join_segments(Segments, [], TrailingSlash, PathCodes).

	join_segments([], Acc, true, PathCodes) :-
		!,
		% Add trailing slash if needed
		append(Acc, [0'/], PathCodes).
	join_segments([], Acc, false, Acc).
	join_segments([Segment], Acc, TrailingSlash, PathCodes) :-
		!,
		append(Acc, Segment, Acc1),
		join_segments([], Acc1, TrailingSlash, PathCodes).
	join_segments([Segment|Segments], Acc, TrailingSlash, PathCodes) :-
		append(Acc, Segment, Acc1),
		append(Acc1, [0'/], Acc2),
		join_segments(Segments, Acc2, TrailingSlash, PathCodes).

	build_url_from_components(Components, URL) :-
		components_generation_kind(Components, Kind),
		build_url_from_components(Kind, Components, [], URLCodes),
		convert_to_text(_Representation_, URLCodes, URL).

	components_generation_kind([scheme(_)|_], url) :-
		!.
	components_generation_kind(_, relative_reference).

	components_reference_kind([scheme(_)|_], url) :-
		!.
	components_reference_kind([authority(_)|_], network_path) :-
		!.
	components_reference_kind([path(Path)|_], absolute_path) :-
		absolute_path(Path),
		!.
	components_reference_kind([path(_)|_], relative_path) :-
		!.
	components_reference_kind([query(_)|_], query) :-
		!.
	components_reference_kind([fragment(_)|_], fragment).

	component_output_codes(scheme(Scheme), SchemeCodes) :-
		downcase_text(Scheme, NormalizedScheme),
		convert_to_text(_Representation_, SchemeCodes, NormalizedScheme).
	component_output_codes(authority(Authority), AuthorityCodes) :-
		normalize_authority(Authority, NormalizedAuthority),
		convert_to_text(_Representation_, AuthorityCodes, NormalizedAuthority).
	component_output_codes(path(Path), PathCodes) :-
		normalize_path(Path, NormalizedPath),
		convert_to_text(_Representation_, PathCodes, NormalizedPath).
	component_output_codes(query(Query), QueryCodes) :-
		normalize_query(Query, NormalizedQuery),
		convert_to_text(_Representation_, QueryCodes, NormalizedQuery).
	component_output_codes(fragment(Fragment), FragmentCodes) :-
		normalize_fragment(Fragment, NormalizedFragment),
		convert_to_text(_Representation_, FragmentCodes, NormalizedFragment).
	component_output_codes(address(Address), AddressCodes) :-
		normalize_address(Address, NormalizedAddress),
		convert_to_text(_Representation_, AddressCodes, NormalizedAddress).
	component_output_codes(number(Number), NumberCodes) :-
		normalize_number(Number, NormalizedNumber),
		convert_to_text(_Representation_, NumberCodes, NormalizedNumber).
	component_output_codes(identifier(Identifier), IdentifierCodes) :-
		normalize_identifier(Identifier, NormalizedIdentifier),
		convert_to_text(_Representation_, IdentifierCodes, NormalizedIdentifier).

	build_url_from_components(_, [], URLCodes, URLCodes) :-
		!.
	% Handle schemes that use only colon (mailto, news, tel, urn)
	build_url_from_components(url, [scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [address(_)| _],
		!,
		component_output_codes(scheme(Scheme), SchemeCodes),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(url, Components, Acc2, URLCodes).
	build_url_from_components(url, [scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [number(_)| _],
		!,
		component_output_codes(scheme(Scheme), SchemeCodes),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(url, Components, Acc2, URLCodes).
	build_url_from_components(url, [scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [identifier(_)| _],
		!,
		component_output_codes(scheme(Scheme), SchemeCodes),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(url, Components, Acc2, URLCodes).
	% Handle standard schemes with authority (http, https, ftp, etc.)
	build_url_from_components(url, [scheme(Scheme)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(scheme(Scheme), SchemeCodes),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':,0'/,0'/], Acc2),
		build_url_from_components(url, Components, Acc2, URLCodes).
	build_url_from_components(relative_reference, [authority(Authority)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(authority(Authority), AuthorityCodes),
		append(Acc, [0'/,0'/| AuthorityCodes], Acc1),
		build_url_from_components(relative_reference, Components, Acc1, URLCodes).
	build_url_from_components(_, [authority(Authority)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(authority(Authority), AuthorityCodes),
		append(Acc, AuthorityCodes, Acc1),
		build_url_from_components(url, Components, Acc1, URLCodes).
	build_url_from_components(url, [path(Path)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(path(Path), PathCodes),
		(	PathCodes \== [] ->
			(	PathCodes = [0'/|_] ->
				append(Acc, PathCodes, Acc1)
			;	append(Acc, [0'/], Acc0),
				append(Acc0, PathCodes, Acc1)
			)
		;	Acc1 = Acc
		),
		build_url_from_components(url, Components, Acc1, URLCodes).
	build_url_from_components(relative_reference, [path(Path)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(path(Path), PathCodes),
		append_relative_reference_path_codes(Acc, PathCodes, Acc1),
		build_url_from_components(relative_reference, Components, Acc1, URLCodes).
	build_url_from_components(Type, [query(Query)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(query(Query), QueryCodes),
		(	QueryCodes \== [] ->
			append(Acc, [0'?], Acc0),
			append(Acc0, QueryCodes, Acc1)
		;	Acc1 = Acc
		),
		build_url_from_components(Type, Components, Acc1, URLCodes).
	build_url_from_components(Type, [fragment(Fragment)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(fragment(Fragment), FragmentCodes),
		(	FragmentCodes \== [] ->
			append(Acc, [0'#], Acc0),
			append(Acc0, FragmentCodes, Acc1)
		;	Acc1 = Acc
		),
		build_url_from_components(Type, Components, Acc1, URLCodes).
	% Handle mailto address component
	build_url_from_components(Type, [address(Address)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(address(Address), AddressCodes),
		append(Acc, AddressCodes, Acc1),
		build_url_from_components(Type, Components, Acc1, URLCodes).
	% Handle tel number component
	build_url_from_components(Type, [number(Number)| Components], Acc, URLCodes) :-
		!,
		component_output_codes(number(Number), NumberCodes),
		append(Acc, NumberCodes, Acc1),
		build_url_from_components(Type, Components, Acc1, URLCodes).
	% Handle news/urn identifier component
	build_url_from_components(Type, [identifier(Identifier)| Components], Acc, URLCodes) :-
		component_output_codes(identifier(Identifier), IdentifierCodes),
		append(Acc, IdentifierCodes, Acc1),
		build_url_from_components(Type, Components, Acc1, URLCodes).

	append_relative_reference_path_codes(Acc, [], Acc) :-
		!.
	append_relative_reference_path_codes([0'/,0'/|_]= Acc, [0'/|_]= PathCodes, Acc1) :-
		!,
		append(Acc, PathCodes, Acc1).
	append_relative_reference_path_codes([0'/,0'/|_]= Acc, PathCodes, Acc1) :-
		!,
		append(Acc, [0'/| PathCodes], Acc1).
	append_relative_reference_path_codes(Acc, PathCodes, Acc1) :-
		append(Acc, PathCodes, Acc1).

	normalized_absolute_reference_components(Reference, Components) :-
		normalize(Reference, NormalizedReference),
		parse(NormalizedReference, Components),
		Components = [scheme(_), authority(_)| _].

	relativize_reference_components(BasePath, BaseQuery, BaseFragment, TargetPath, TargetQuery, TargetFragment, Empty, [fragment(TargetFragment)]) :-
		BasePath == TargetPath,
		BaseQuery == TargetQuery,
		TargetFragment \== Empty,
		TargetFragment \== BaseFragment,
		!.
	relativize_reference_components(BasePath, BaseQuery, BaseFragment, TargetPath, TargetQuery, TargetFragment, Empty, RelativeComponents) :-
		BasePath == TargetPath,
		TargetQuery \== Empty,
		( TargetQuery \== BaseQuery ; TargetFragment \== BaseFragment ),
		!,
		append_optional_fragment([query(TargetQuery)], TargetFragment, Empty, RelativeComponents).
	relativize_reference_components(BasePath, _BaseQuery, _BaseFragment, TargetPath, TargetQuery, TargetFragment, Empty, RelativeComponents) :-
		relativize_reference_path(BasePath, TargetPath, RelativePath),
		RelativePath \== Empty,
		append_optional_query_fragment([path(RelativePath)], TargetQuery, TargetFragment, Empty, RelativeComponents).

	append_optional_query_fragment(Components0, Query, Fragment, Empty, Components) :-
		append_optional_query(Components0, Query, Empty, Components1),
		append_optional_fragment(Components1, Fragment, Empty, Components).

	append_optional_query(Components0, Query, Empty, Components) :-
		( Query == Empty ->
			Components = Components0
		; 	append(Components0, [query(Query)], Components)
		).

	append_optional_fragment(Components0, Fragment, Empty, Components) :-
		( Fragment == Empty ->
			Components = Components0
		; 	append(Components0, [fragment(Fragment)], Components)
		).

	relativize_reference_path(BasePath, TargetPath, RelativePath) :-
		convert_to_text(_Representation_, BasePathCodes, BasePath),
		convert_to_text(_Representation_, TargetPathCodes, TargetPath),
		relativize_reference_path_codes(BasePathCodes, TargetPathCodes, RelativePathCodes),
		convert_to_text(_Representation_, RelativePathCodes, RelativePath).

	relativize_reference_path_codes(BasePathCodes, TargetPathCodes, RelativePathCodes) :-
		path_segments_codes(BasePathCodes, BaseSegments, BaseTrailingSlash),
		path_segments_codes(TargetPathCodes, TargetSegments, TargetTrailingSlash),
		base_directory_segments(BaseSegments, BaseTrailingSlash, BaseDirectorySegments),
		split_common_path_prefix(BaseDirectorySegments, TargetSegments, BaseRemainderSegments, TargetRemainderSegments),
		parent_path_segments(BaseRemainderSegments, ParentSegments),
		append(ParentSegments, TargetRemainderSegments, RelativeSegments),
		relative_path_codes(RelativeSegments, TargetTrailingSlash, TargetPathCodes, RelativePathCodes).

	path_segments_codes([], [], false) :-
		!.
	path_segments_codes([0'/], [], true) :-
		!.
	path_segments_codes([0'/| PathCodes], Segments, TrailingSlash) :-
		!,
		split_path(PathCodes, Segments0),
		remove_trailing_empty_segment(Segments0, Segments, TrailingSlash).
	path_segments_codes(PathCodes, Segments, TrailingSlash) :-
		split_path(PathCodes, Segments0),
		remove_trailing_empty_segment(Segments0, Segments, TrailingSlash).

	remove_trailing_empty_segment(Segments0, Segments, true) :-
		append(Segments, [[]], Segments0),
		!.
	remove_trailing_empty_segment(Segments, Segments, false).

	base_directory_segments(Segments, true, Segments) :-
		!.
	base_directory_segments(Segments, false, DirectorySegments) :-
		remove_last_path_segment(Segments, DirectorySegments).

	remove_last_path_segment([], []) :-
		!.
	remove_last_path_segment([_], []) :-
		!.
	remove_last_path_segment([Segment| Segments], [Segment| DirectorySegments]) :-
		remove_last_path_segment(Segments, DirectorySegments).

	split_common_path_prefix([Segment| BaseSegments], [Segment| TargetSegments], BaseRemainderSegments, TargetRemainderSegments) :-
		!,
		split_common_path_prefix(BaseSegments, TargetSegments, BaseRemainderSegments, TargetRemainderSegments).
	split_common_path_prefix(BaseSegments, TargetSegments, BaseSegments, TargetSegments).

	parent_path_segments([], []).
	parent_path_segments([_| Segments], [[0'.,0'.]| ParentSegments]) :-
		parent_path_segments(Segments, ParentSegments).

	relative_path_codes([], _TargetTrailingSlash, _TargetPathCodes, []) :-
		!,
		fail.
	relative_path_codes(RelativeSegments, TargetTrailingSlash, TargetPathCodes, RelativePathCodes) :-
		RelativeSegments \== [],
		join_segments(RelativeSegments, TargetTrailingSlash, RelativePathCodes0),
		( 	relative_path_noscheme_codes(RelativePathCodes0) ->
			RelativePathCodes = RelativePathCodes0
		; 	TargetPathCodes = [0'/|_],
			RelativePathCodes = TargetPathCodes
		),
		!.

	relative_path_noscheme_codes(RelativePathCodes) :-
		split_path(RelativePathCodes, [FirstSegment| _]),
		\+ member(0':, FirstSegment).

	parse_reference_for_resolution(Reference, []) :-
		convert_to_text(_Representation_, [], Reference),
		!.
	parse_reference_for_resolution(Reference, Components) :-
		parse(Reference, Components, _).

	resolve_reference_components(_, _, ReferenceComponents, _, ReferenceComponents) :-
		ReferenceComponents = [scheme(_)| _],
		!.
	resolve_reference_components(Scheme, BaseComponents, ReferenceComponents, Empty, ResolvedComponents) :-
		hierarchical_base_components(BaseComponents, BaseAuthority, BasePath, BaseQuery),
		resolve_relative_reference_components(Scheme, BaseAuthority, BasePath, BaseQuery, ReferenceComponents, Empty, ResolvedComponents).

	hierarchical_base_components(BaseComponents, BaseAuthority, BasePath, BaseQuery) :-
		component_value(authority, BaseComponents, BaseAuthority),
		component_value_or_default(path, BaseComponents, '', BasePath),
		component_value_or_default(query, BaseComponents, '', BaseQuery).

	resolve_relative_reference_components(Scheme, _BaseAuthority, _BasePath, _BaseQuery, [authority(ReferenceAuthority)| ReferenceComponents], Empty, ResolvedComponents) :-
		!,
		component_value_or_default(path, ReferenceComponents, Empty, ReferencePath),
		component_value_or_default(query, ReferenceComponents, Empty, ReferenceQuery),
		component_value_or_default(fragment, ReferenceComponents, Empty, ReferenceFragment),
		ResolvedComponents = [scheme(Scheme), authority(ReferenceAuthority), path(ReferencePath), query(ReferenceQuery), fragment(ReferenceFragment)].
	resolve_relative_reference_components(Scheme, BaseAuthority, BasePath, _BaseQuery, ReferenceComponents, Empty, ResolvedComponents) :-
		component_value(path, ReferenceComponents, ReferencePath),
		!,
		resolve_reference_path(BasePath, ReferencePath, ResolvedPath),
		component_value_or_default(query, ReferenceComponents, Empty, ReferenceQuery),
		component_value_or_default(fragment, ReferenceComponents, Empty, ReferenceFragment),
		ResolvedComponents = [scheme(Scheme), authority(BaseAuthority), path(ResolvedPath), query(ReferenceQuery), fragment(ReferenceFragment)].
	resolve_relative_reference_components(Scheme, BaseAuthority, BasePath, _BaseQuery, ReferenceComponents, Empty, ResolvedComponents) :-
		component_present(query, ReferenceComponents),
		!,
		component_value(query, ReferenceComponents, ReferenceQuery),
		component_value_or_default(fragment, ReferenceComponents, Empty, ReferenceFragment),
		ResolvedComponents = [scheme(Scheme), authority(BaseAuthority), path(BasePath), query(ReferenceQuery), fragment(ReferenceFragment)].
	resolve_relative_reference_components(Scheme, BaseAuthority, BasePath, BaseQuery, ReferenceComponents, Empty, ResolvedComponents) :-
		component_value_or_default(fragment, ReferenceComponents, Empty, ReferenceFragment),
		ResolvedComponents = [scheme(Scheme), authority(BaseAuthority), path(BasePath), query(BaseQuery), fragment(ReferenceFragment)].

	empty_text(Empty) :-
		convert_to_text(_Representation_, [], Empty).

	resolve_reference_path(_BasePath, ReferencePath, ResolvedPath) :-
		absolute_path(ReferencePath),
		!,
		normalize_path(ReferencePath, ResolvedPath).
	resolve_reference_path(BasePath, ReferencePath, ResolvedPath) :-
		merge_reference_path(BasePath, ReferencePath, MergedPath),
		normalize_path(MergedPath, ResolvedPath).

	absolute_path(Path) :-
		convert_to_text(_Representation_, PathCodes, Path),
		PathCodes = [0'/| _].

	merge_reference_path(BasePath, ReferencePath, MergedPath) :-
		convert_to_text(_Representation_, BasePathCodes, BasePath),
		convert_to_text(_Representation_, ReferencePathCodes, ReferencePath),
		merge_reference_path_codes(BasePathCodes, ReferencePathCodes, MergedPathCodes),
		convert_to_text(_Representation_, MergedPathCodes, MergedPath).

	merge_reference_path_codes([], ReferencePathCodes, [0'/| ReferencePathCodes]) :-
		!.
	merge_reference_path_codes(BasePathCodes, ReferencePathCodes, MergedPathCodes) :-
		remove_last_segment_codes(BasePathCodes, BasePrefixCodes),
		append(BasePrefixCodes, ReferencePathCodes, MergedPathCodes).

	remove_last_segment_codes(PathCodes, PrefixCodes) :-
		reverse(PathCodes, ReversedPathCodes),
		drop_until_slash(ReversedPathCodes, ReversedPrefixCodes),
		reverse(ReversedPrefixCodes, PrefixCodes).

	drop_until_slash([0'/| ReversedPrefixCodes], [0'/| ReversedPrefixCodes]) :-
		!.
	drop_until_slash([_| ReversedPathCodes], ReversedPrefixCodes) :-
		drop_until_slash(ReversedPathCodes, ReversedPrefixCodes).
	drop_until_slash([], []).

	component_present(Name, [Component| _]) :-
		functor(Component, Name, 1),
		!.
	component_present(Name, [_| Components]) :-
		component_present(Name, Components).

	component_value(Name, [Component| _], Value) :-
		functor(Component, Name, 1),
		!,
		arg(1, Component, Value).
	component_value(Name, [_| Components], Value) :-
		component_value(Name, Components, Value).

	component_value_or_default(Name, Components, Default, Value) :-
		(	component_value(Name, Components, Value) ->
			true
		; 	Value = Default
		).

	:- private(downcase_text/2).
	:- mode(downcase_text(+text, -text), one).
	:- info(downcase_text/2, [
		comment is 'Converts text to lowercase (ASCII only). Only uppercase letters A-Z are converted to lowercase.',
		argnames is ['Text', 'LowerText']
	]).

	downcase_text(Text, LowerText) :-
		convert_to_text(_Representation_, Codes, Text),
		downcase_codes(Codes, LowerCodes),
		convert_to_text(_Representation_, LowerCodes, LowerText).

	downcase_codes([], []).
	downcase_codes([Code| Codes], [LowerCode| LowerCodes]) :-
		(	Code >= 0'A, Code =< 0'Z ->
			LowerCode is Code + 32
		;	LowerCode = Code
		),
		downcase_codes(Codes, LowerCodes).

	% auxiliary predicates

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	convert_to_text(atom, Data0, Data) :-
		atom_codes(Data, Data0).
	convert_to_text(chars, Data0, Data) :-
		(	var(Data) ->
			codes_to_chars(Data0, Data)
		;	chars_to_codes(Data, Data0)
		).
	convert_to_text(codes, Data, Data).

:- end_object.
