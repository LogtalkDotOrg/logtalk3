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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-22,
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
		comment is 'Generates a URL from a list of its components: ``[scheme(Scheme), authority(Authority), path(Path), query(Query), fragment(Fragment)]`` for standard URLs, or scheme-specific components for mailto, news, tel, and urn URLs. Fails if the components are invalid.',
		argnames is ['Components', 'URL']
	]).

	:- public(normalize/2).
	:- mode(normalize(++text, -text), one).
	:- info(normalize/2, [
		comment is 'Normalizes a URL by standardizing its components. Normalization includes converting scheme and authority to lowercase, ensuring proper path separators, and handling relative paths.',
		argnames is ['URL', 'NormalizedURL']
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	valid(URL) :-
		valid(_Representation_, URL).

	valid(atom, URL) :-
		atom_codes(URL, Codes),
		phrase(url, Codes).
	valid(chars, URL) :-
		chars_to_codes(URL, Codes),
		phrase(url, Codes).
	valid(codes, URL) :-
		phrase(url, URL).

	url --> scheme, [0':,0'/,0'/], authority, path, query, fragment.
	url --> file_scheme, [0':,0'/,0'/], path.
	url --> mailto_scheme, [0':], email_address.
	url --> news_scheme, [0':], news_identifier.
	url --> tel_scheme, [0':], phone_number.
	url --> urn_scheme, [0':], urn_identifier.

	% web protocols
	scheme --> h, t, t, p.
	scheme --> h, t, t, p, s.
	scheme --> w, s.
	scheme --> w, s, s.
	% file transfer and version control
	scheme --> f, t, p.
	scheme --> f, t, p, s.
	scheme --> s, f, t, p.
	scheme --> g, i, t.
	% media streaming
	scheme --> m, m, s.
	scheme --> r, t, s, p.
	scheme --> r, t, m, p.
	% directory services
	scheme --> l, d, a, p.
	scheme --> l, d, a, p, s.
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
	phone_code(0' ).

	% URN identifier (namespace-specific string)
	urn_identifier --> [Code], {urn_code(Code)}, !, urn_identifier.
	urn_identifier --> [].

	urn_code(Code) :- Code =\= 0'?, Code =\= 0'#, Code >= 33, Code =< 126.

	authority --> userinfo, [0'@], !, host, port.
	authority --> host, port.

	userinfo --> [Code], {userinfo_code(Code)}, !, userinfo.
	userinfo --> [].

	userinfo_code(Code) :- code(Code), !.
	userinfo_code(0':).  % colon separates user and password

	host --> ip_number.
	host --> identifier, dot_identifiers.

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

	dot_identifiers --> [0'.], !, identifier, dot_identifiers.
	dot_identifiers --> [].

	path --> [0'/], path_segment, path.
	path --> [].

	path_segment --> [Code], {path_code(Code)}, !, path_segment.
	path_segment --> [].

	query --> [0'?], query_codes.
	query --> [].

	query_codes --> [Code], {query_code(Code)}, !, query_codes.
	query_codes --> [].

	query_code(Code) :- Code =\= 0'#, Code >= 32, Code =< 126.

	fragment --> [0'#], fragment_codes.
	fragment --> [].

	fragment_codes --> [Code], {fragment_code(Code)}, !, fragment_codes.
	fragment_codes --> [].

	fragment_code(Code) :- Code >= 32, Code =< 126.

	parse(URL, Components) :-
		parse(_Representation_, URL, Components).

	parse(atom, URL, [scheme(Scheme)| Components]) :-
		atom_codes(URL, Codes),
		phrase(scheme(Scheme), Codes, Rest),
		parse_url(Scheme, Rest, Components).
	parse(chars, URL, [scheme(Scheme)| Components]) :-
		chars_to_codes(URL, Codes),
		phrase(scheme(Scheme), Codes, Rest),
		atom_chars(SchemeAtom, Scheme),
		parse_url(SchemeAtom, Rest, Components).
	parse(codes, URL, [scheme(Scheme)| Components]) :-
		phrase(scheme(Scheme), URL, Rest),
		atom_codes(SchemeAtom, Scheme),
		parse_url(SchemeAtom, Rest, Components).

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
		authority_component(Authority),
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
		codes_until(0'/, AuthorityCodes),
		{convert_to_text(_Representation_, AuthorityCodes, Authority)}.

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
		[0'.], identifier(Codes0), dot_identifiers(Codes1),
		{append([0'.| Codes0], Codes1, Codes)}.
	dot_identifiers([]) -->
		[].

	generate(Components, URL) :-
		build_url_from_components(Components, URL).

	normalize(URL, NormalizedURL) :-
		parse(URL, Components),
		normalize_components(Components, NormalizedComponents),
		build_url_from_components(NormalizedComponents, NormalizedURL).

	normalize_components([], []).
	normalize_components([Component| Components], [NormalizedComponent| NormalizedComponents]) :-
		normalize_component(Component, NormalizedComponent),
		normalize_components(Components, NormalizedComponents).

	normalize_component(scheme(Scheme), scheme(NormalizedScheme)) :-
		downcase_text(Scheme, NormalizedScheme).
	normalize_component(authority(Authority), authority(Authority)).
	normalize_component(path(Path), path(NormalizedPath)) :-
		normalize_path(Path, NormalizedPath).
	normalize_component(query(Query), query(Query)).
	normalize_component(fragment(Fragment), fragment(Fragment)).

	normalize_path(Path, NormalizedPath) :-
		convert_to_text(_Representation_, PathCodes, Path),
		normalize_path_codes(PathCodes, NormalizedPathCodes),
		convert_to_text(_Representation_, NormalizedPathCodes, NormalizedPath).

	normalize_path_codes(PathCodes, NormalizedPathCodes) :-
		% Check if path ends with a slash
		(	append(_, [0'/], PathCodes) ->
			TrailingSlash = true
		;	TrailingSlash = false
		),
		% Split path into segments
		split_path(PathCodes, Segments),
		% Process segments (remove '.', handle '..')
		process_segments(Segments, ProcessedSegments),
		% Join segments back into a path
		join_segments(ProcessedSegments, TrailingSlash, NormalizedPathCodes).

	split_path(PathCodes, Segments) :-
		split_path(PathCodes, [], Segments).

	split_path([], CurrentSegment, [CurrentSegment]).
	split_path([0'/|Rest], CurrentSegment, [CurrentSegment|Segments]) :-
		split_path(Rest, [], Segments).
	split_path([Code|Rest], CurrentSegment, Segments) :-
		append(CurrentSegment, [Code], NewSegment),
		split_path(Rest, NewSegment, Segments).

	process_segments(Segments, ProcessedSegments) :-
		process_segments(Segments, [], ProcessedSegments).

	process_segments([], Acc, Processed) :-
		reverse(Acc, Processed).
	process_segments([[]|Segments], Acc, Processed) :-
		% Skip empty segments
		process_segments(Segments, Acc, Processed).
	process_segments([[0'., 0'.]|Segments], [_|Acc], Processed) :-
		% Handle '..' by removing the previous segment
		process_segments(Segments, Acc, Processed).
	process_segments([[0'.]|Segments], Acc, Processed) :-
		% Skip '.' segments
		process_segments(Segments, Acc, Processed).
	process_segments([Segment|Segments], Acc, Processed) :-
		% Keep normal segments
		process_segments(Segments, [Segment|Acc], Processed).

	join_segments(Segments, TrailingSlash, PathCodes) :-
		join_segments(Segments, [], TrailingSlash, PathCodes).

	join_segments([], Acc, true, PathCodes) :-
		% Add trailing slash if needed
		append(Acc, [0'/], PathCodes).
	join_segments([], Acc, false, Acc).
	join_segments([Segment|[]], Acc, TrailingSlash, PathCodes) :-
		append(Acc, Segment, Acc1),
		join_segments([], Acc1, TrailingSlash, PathCodes).
	join_segments([Segment|Segments], Acc, TrailingSlash, PathCodes) :-
		append(Acc, Segment, Acc1),
		append(Acc1, [0'/], Acc2),
		join_segments(Segments, Acc2, TrailingSlash, PathCodes).

	build_url_from_components(Components, URL) :-
		build_url_from_components(Components, [], URLCodes),
		convert_to_text(_Representation_, URLCodes, URL).

	build_url_from_components([], Acc, Acc).
	% Handle schemes that use only colon (mailto, news, tel, urn)
	build_url_from_components([scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [address(_)| _],
		!,
		convert_to_text(_Representation_, SchemeCodes, Scheme),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(Components, Acc2, URLCodes).
	build_url_from_components([scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [number(_)| _],
		!,
		convert_to_text(_Representation_, SchemeCodes, Scheme),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(Components, Acc2, URLCodes).
	build_url_from_components([scheme(Scheme)| Components], Acc, URLCodes) :-
		Components = [identifier(_)| _],
		!,
		convert_to_text(_Representation_, SchemeCodes, Scheme),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':], Acc2),
		build_url_from_components(Components, Acc2, URLCodes).
	% Handle standard schemes with authority (http, https, ftp, etc.)
	build_url_from_components([scheme(Scheme)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, SchemeCodes, Scheme),
		append(Acc, SchemeCodes, Acc1),
		append(Acc1, [0':,0'/,0'/], Acc2),
		build_url_from_components(Components, Acc2, URLCodes).
	build_url_from_components([authority(Authority)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, AuthorityCodes, Authority),
		append(Acc, AuthorityCodes, Acc1),
		build_url_from_components(Components, Acc1, URLCodes).
	build_url_from_components([path(Path)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, PathCodes, Path),
		(	PathCodes \== [] ->
			(	PathCodes = [0'/|_] ->
				append(Acc, PathCodes, Acc1)
			;	append(Acc, [0'/], Acc0),
				append(Acc0, PathCodes, Acc1)
			)
		;	Acc1 = Acc
		),
		build_url_from_components(Components, Acc1, URLCodes).
	build_url_from_components([query(Query)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, QueryCodes, Query),
		(	QueryCodes \== [] ->
			append(Acc, [0'?], Acc0),
			append(Acc0, QueryCodes, Acc1)
		;	Acc1 = Acc
		),
		build_url_from_components(Components, Acc1, URLCodes).
	build_url_from_components([fragment(Fragment)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, FragmentCodes, Fragment),
		(	FragmentCodes \== [] ->
			append(Acc, [0'#], Acc0),
			append(Acc0, FragmentCodes, Acc1)
		;	Acc1 = Acc
		),
		build_url_from_components(Components, Acc1, URLCodes).
	% Handle mailto address component
	build_url_from_components([address(Address)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, AddressCodes, Address),
		append(Acc, AddressCodes, Acc1),
		build_url_from_components(Components, Acc1, URLCodes).
	% Handle tel number component
	build_url_from_components([number(Number)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, NumberCodes, Number),
		append(Acc, NumberCodes, Acc1),
		build_url_from_components(Components, Acc1, URLCodes).
	% Handle news/urn identifier component
	build_url_from_components([identifier(Identifier)| Components], Acc, URLCodes) :-
		convert_to_text(_Representation_, IdentifierCodes, Identifier),
		append(Acc, IdentifierCodes, Acc1),
		build_url_from_components(Components, Acc1, URLCodes).

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
		codes_to_chars(Data0, Data).
	convert_to_text(codes, Data, Data).

:- end_object.
