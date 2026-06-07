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


:- category(http_origin_site_helpers,
	extends(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'Internal shared helpers for parsing HTTP origins and absolute URLs, deriving request endpoints, and comparing endpoints for schemeful same-site semantics.'
	]).

	:- protected(absolute_url_context/2).
	:- mode(absolute_url_context(+atom, -compound), zero_or_one).
	:- info(absolute_url_context/2, [
		comment is 'Parses an absolute HTTP or HTTPS URL atom into a normalized endpoint and request path context.',
		argnames is ['URL', 'Context']
	]).

	:- protected(origin_endpoint/2).
	:- mode(origin_endpoint(+atom, -compound), zero_or_one).
	:- info(origin_endpoint/2, [
		comment is 'Parses an Origin header atom into a normalized HTTP or HTTPS endpoint.',
		argnames is ['Origin', 'Endpoint']
	]).

	:- protected(request_endpoint/2).
	:- mode(request_endpoint(+compound, -compound), zero_or_one).
	:- info(request_endpoint/2, [
		comment is 'Derives a normalized HTTP or HTTPS endpoint from normalized request properties.',
		argnames is ['Request', 'Endpoint']
	]).

	:- protected(same_site/2).
	:- mode(same_site(+compound, +compound), zero_or_one).
	:- info(same_site/2, [
		comment is 'Succeeds when two normalized endpoints or URL contexts are schemefully same-site using bundled public suffix rules, including exact, wildcard, and exception cases.',
		argnames is ['Left', 'Right']
	]).

	:- protected(host_labels/2).
	:- mode(host_labels(+atom, -list(atom)), zero_or_one).
	:- info(host_labels/2, [
		comment is 'Splits a normalized host atom into non-empty dot-separated labels.',
		argnames is ['Host', 'Labels']
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, reverse/2, take/3
	]).

	:- uses(http_core, [
		property/2 as http_property/2, request/7 as http_request/7, target/2 as http_target/2
	]).

	absolute_url_context(URL, http_url_context(Scheme, Host, Port, Path)) :-
		atom(URL),
		url(atom)::parse(URL, Components),
		url_components_endpoint(Components, allow_userinfo, Scheme, Host, Port),
		components_path(Components, Path).

	origin_endpoint(Origin, http_endpoint(Scheme, Host, Port)) :-
		atom(Origin),
		url(atom)::parse(Origin, Components),
		url_components_endpoint(Components, reject_userinfo, Scheme, Host, Port),
		\+ origin_extra_components(Components).

	request_endpoint(Request, Endpoint) :-
		( 	request_properties_endpoint(Request, Endpoint) ->
			true
		; 	request_target_endpoint(Request, Endpoint)
		).

	same_site(Left, Right) :-
		site_key(Left, SiteKey),
		site_key(Right, SiteKey).

	host_labels(Host, Labels) :-
		atom(Host),
		atom::split(Host, '.', Labels),
		Labels \== [],
		\+ empty_label(Labels).

	url_components_endpoint(Components, UserinfoMode, Scheme, Host, Port) :-
		memberchk(scheme(Scheme0), Components),
		normalize_atom_ascii(Scheme0, Scheme),
		default_scheme_port(Scheme, DefaultPort),
		memberchk(authority(Authority), Components),
		authority_endpoint(Authority, UserinfoMode, Host, ExplicitPort),
		( 	ExplicitPort == none ->
			Port = DefaultPort
		; 	Port = ExplicitPort
		),
		validate_endpoint(Host, ExplicitPort).

	authority_endpoint(Authority, UserinfoMode, Host, Port) :-
		atom(Authority),
		atom_codes(Authority, AuthorityCodes0),
		authority_codes(UserinfoMode, AuthorityCodes0, AuthorityCodes),
		AuthorityCodes \== [],
		parse_authority_codes(AuthorityCodes, HostCodes0, Port),
		^^lowercase_ascii_codes(HostCodes0, HostCodes),
		atom_codes(Host, HostCodes).

	authority_codes(allow_userinfo, AuthorityCodes0, AuthorityCodes) :-
		strip_userinfo_codes(AuthorityCodes0, AuthorityCodes).
	authority_codes(reject_userinfo, AuthorityCodes, AuthorityCodes) :-
		\+ member(0'@, AuthorityCodes).

	components_path(Components, Path) :-
		( 	member(path(Path0), Components) ->
			normalize_request_path(Path0, Path)
		; 	Path = '/'
		).

	origin_extra_components(Components) :-
		member(path(Path), Components),
		Path \== '',
		Path \== '/'.
	origin_extra_components(Components) :-
		member(query(Query), Components),
		Query \== ''.
	origin_extra_components(Components) :-
		member(fragment(Fragment), Components),
		Fragment \== ''.

	validate_endpoint(Host, none) :-
		!,
		catch(http_request(get, authority(Host), http(1, 1), [], empty, [], _Request), _, fail).
	validate_endpoint(Host, Port) :-
		catch(http_request(get, authority(Host, Port), http(1, 1), [], empty, [], _Request), _, fail).

	request_properties_endpoint(Request, http_endpoint(Scheme, Host, Port)) :-
		http_property(Request, scheme(Scheme)),
		( 	http_property(Request, host(Host, Port)) ->
			true
		; 	http_property(Request, host(Host)),
			default_scheme_port(Scheme, Port)
		).

	request_target_endpoint(Request, http_endpoint(Scheme, Host, Port)) :-
		http_target(Request, absolute(Components)),
		url_components_endpoint(Components, reject_userinfo, Scheme, Host, Port).

	normalize_atom_ascii(Atom, NormalizedAtom) :-
		atom_codes(Atom, Codes),
		^^lowercase_ascii_codes(Codes, LowercaseCodes),
		atom_codes(NormalizedAtom, LowercaseCodes).

	normalize_request_path('', '/') :-
		!.
	normalize_request_path(Path, Path).

	parse_authority_codes([0'[| AuthorityCodes], HostCodes, Port) :-
		split_once(0'], AuthorityCodes, HostCodes, RestCodes),
		!,
		parse_bracketed_port_codes(RestCodes, Port).
	parse_authority_codes(AuthorityCodes, HostCodes, Port) :-
		split_last_colon(AuthorityCodes, HostCodes, PortCodes),
		PortCodes \== [],
		digit_codes(PortCodes),
		!,
		number_codes(Port, PortCodes).
	parse_authority_codes(AuthorityCodes, AuthorityCodes, none).

	parse_bracketed_port_codes([], none) :-
		!.
	parse_bracketed_port_codes([0':| PortCodes], Port) :-
		PortCodes \== [],
		digit_codes(PortCodes),
		number_codes(Port, PortCodes).

	default_scheme_port(http, 80).
	default_scheme_port(https, 443).

	site_key(EndpointOrContext, http_site(Scheme, HostKey)) :-
		endpoint_host(EndpointOrContext, Scheme, Host),
		host_site_key(Host, HostKey).

	endpoint_host(http_endpoint(Scheme, Host, _Port), Scheme, Host).
	endpoint_host(http_url_context(Scheme, Host, _Port, _Path), Scheme, Host).

	host_site_key(Host, Host) :-
		literal_or_single_label_host(Host),
		!.
	host_site_key(Host, SiteHost) :-
		host_labels(Host, Labels),
		registrable_domain_labels(Labels, SiteLabels),
		host_labels_atom(SiteLabels, SiteHost).

	registrable_domain_labels(Labels, Labels) :-
		public_suffix_length(Labels, Length),
		length(Labels, Length),
		!.
	registrable_domain_labels(Labels, SiteLabels) :-
		public_suffix_length(Labels, Length),
		SiteLength is Length + 1,
		reverse(Labels, ReversedLabels),
		take(SiteLength, ReversedLabels, ReversedSiteLabels),
		reverse(ReversedSiteLabels, SiteLabels).

	public_suffix_length(Labels, Length) :-
		reverse(Labels, ReversedLabels),
		( 	exception_public_suffix_length(ReversedLabels, Length) ->
			true
		; 	longest_public_suffix_length(ReversedLabels, Length) ->
			true
		; 	Length = 1
		).

	exception_public_suffix_length(ReversedLabels, Length) :-
		public_suffix_exception_reversed_(Rule),
		reversed_prefix(Rule, ReversedLabels),
		length(Rule, RuleLength),
		Length is RuleLength - 1,
		\+ (
			public_suffix_exception_reversed_(LongerRule),
			length(LongerRule, LongerLength),
			LongerLength > RuleLength,
			reversed_prefix(LongerRule, ReversedLabels)
		).

	longest_public_suffix_length(ReversedLabels, Length) :-
		public_suffix_candidate_length(ReversedLabels, Length),
		\+ (
			public_suffix_candidate_length(ReversedLabels, LongerLength),
			LongerLength > Length
		).

	public_suffix_candidate_length(ReversedLabels, Length) :-
		public_suffix_exact_reversed_(Rule),
		reversed_prefix(Rule, ReversedLabels),
		length(Rule, Length).
	public_suffix_candidate_length(ReversedLabels, Length) :-
		public_suffix_wildcard_base_reversed_(Rule),
		reversed_prefix(Rule, ReversedLabels),
		length(ReversedLabels, LabelsLength),
		length(Rule, RuleLength),
		LabelsLength > RuleLength,
		Length is RuleLength + 1.

	host_labels_atom([Label], Label) :-
		!.
	host_labels_atom([Label| Labels], Atom) :-
		host_labels_atom(Labels, LabelsAtom),
		atom_concat(Label, '.', Prefix),
		atom_concat(Prefix, LabelsAtom, Atom).

	reversed_prefix([], _Labels).
	reversed_prefix([Label| Prefix], [Label| Labels]) :-
		reversed_prefix(Prefix, Labels).

	% Bundled public suffix rules that materially affect same-site boundaries.
	public_suffix_exact_reversed_([uk, ac]).
	public_suffix_exact_reversed_([uk, co]).
	public_suffix_exact_reversed_([uk, gov]).
	public_suffix_exact_reversed_([uk, ltd]).
	public_suffix_exact_reversed_([uk, me]).
	public_suffix_exact_reversed_([uk, mod]).
	public_suffix_exact_reversed_([uk, net]).
	public_suffix_exact_reversed_([uk, nhs]).
	public_suffix_exact_reversed_([uk, org]).
	public_suffix_exact_reversed_([uk, plc]).
	public_suffix_exact_reversed_([uk, police]).
	public_suffix_exact_reversed_([uk, sch]).
	public_suffix_exact_reversed_([au, asn]).
	public_suffix_exact_reversed_([au, com]).
	public_suffix_exact_reversed_([au, edu]).
	public_suffix_exact_reversed_([au, gov]).
	public_suffix_exact_reversed_([au, id]).
	public_suffix_exact_reversed_([au, net]).
	public_suffix_exact_reversed_([au, org]).
	public_suffix_exact_reversed_([br, com]).
	public_suffix_exact_reversed_([br, edu]).
	public_suffix_exact_reversed_([br, gov]).
	public_suffix_exact_reversed_([br, mil]).
	public_suffix_exact_reversed_([br, net]).
	public_suffix_exact_reversed_([br, org]).
	public_suffix_exact_reversed_([cn, ac]).
	public_suffix_exact_reversed_([cn, com]).
	public_suffix_exact_reversed_([cn, edu]).
	public_suffix_exact_reversed_([cn, gov]).
	public_suffix_exact_reversed_([cn, mil]).
	public_suffix_exact_reversed_([cn, net]).
	public_suffix_exact_reversed_([cn, org]).
	public_suffix_exact_reversed_([in, ac]).
	public_suffix_exact_reversed_([in, co]).
	public_suffix_exact_reversed_([in, edu]).
	public_suffix_exact_reversed_([in, firm]).
	public_suffix_exact_reversed_([in, gen]).
	public_suffix_exact_reversed_([in, gov]).
	public_suffix_exact_reversed_([in, ind]).
	public_suffix_exact_reversed_([in, mil]).
	public_suffix_exact_reversed_([in, net]).
	public_suffix_exact_reversed_([in, nic]).
	public_suffix_exact_reversed_([in, org]).
	public_suffix_exact_reversed_([in, res]).
	public_suffix_exact_reversed_([jp, ac]).
	public_suffix_exact_reversed_([jp, ad]).
	public_suffix_exact_reversed_([jp, co]).
	public_suffix_exact_reversed_([jp, ed]).
	public_suffix_exact_reversed_([jp, go]).
	public_suffix_exact_reversed_([jp, gr]).
	public_suffix_exact_reversed_([jp, lg]).
	public_suffix_exact_reversed_([jp, ne]).
	public_suffix_exact_reversed_([jp, or]).
	public_suffix_exact_reversed_([kr, ac]).
	public_suffix_exact_reversed_([kr, co]).
	public_suffix_exact_reversed_([kr, es]).
	public_suffix_exact_reversed_([kr, go]).
	public_suffix_exact_reversed_([kr, hs]).
	public_suffix_exact_reversed_([kr, kg]).
	public_suffix_exact_reversed_([kr, mil]).
	public_suffix_exact_reversed_([kr, ms]).
	public_suffix_exact_reversed_([kr, ne]).
	public_suffix_exact_reversed_([kr, or]).
	public_suffix_exact_reversed_([kr, pe]).
	public_suffix_exact_reversed_([kr, re]).
	public_suffix_exact_reversed_([kr, sc]).
	public_suffix_exact_reversed_([mx, com]).
	public_suffix_exact_reversed_([mx, edu]).
	public_suffix_exact_reversed_([mx, gob]).
	public_suffix_exact_reversed_([mx, net]).
	public_suffix_exact_reversed_([mx, org]).
	public_suffix_exact_reversed_([nz, ac]).
	public_suffix_exact_reversed_([nz, co]).
	public_suffix_exact_reversed_([nz, cri]).
	public_suffix_exact_reversed_([nz, geek]).
	public_suffix_exact_reversed_([nz, gen]).
	public_suffix_exact_reversed_([nz, govt]).
	public_suffix_exact_reversed_([nz, health]).
	public_suffix_exact_reversed_([nz, iwi]).
	public_suffix_exact_reversed_([nz, kiwi]).
	public_suffix_exact_reversed_([nz, maori]).
	public_suffix_exact_reversed_([nz, mil]).
	public_suffix_exact_reversed_([nz, org]).
	public_suffix_exact_reversed_([nz, parliament]).
	public_suffix_exact_reversed_([nz, school]).
	public_suffix_exact_reversed_([sg, com]).
	public_suffix_exact_reversed_([sg, edu]).
	public_suffix_exact_reversed_([sg, gov]).
	public_suffix_exact_reversed_([sg, idn]).
	public_suffix_exact_reversed_([sg, net]).
	public_suffix_exact_reversed_([sg, org]).
	public_suffix_exact_reversed_([sg, per]).
	public_suffix_exact_reversed_([tr, av]).
	public_suffix_exact_reversed_([tr, bbs]).
	public_suffix_exact_reversed_([tr, bel]).
	public_suffix_exact_reversed_([tr, biz]).
	public_suffix_exact_reversed_([tr, com]).
	public_suffix_exact_reversed_([tr, dr]).
	public_suffix_exact_reversed_([tr, edu]).
	public_suffix_exact_reversed_([tr, gen]).
	public_suffix_exact_reversed_([tr, gov]).
	public_suffix_exact_reversed_([tr, info]).
	public_suffix_exact_reversed_([tr, k12]).
	public_suffix_exact_reversed_([tr, name]).
	public_suffix_exact_reversed_([tr, net]).
	public_suffix_exact_reversed_([tr, org]).
	public_suffix_exact_reversed_([tr, pol]).
	public_suffix_exact_reversed_([tr, tel]).
	public_suffix_exact_reversed_([tr, tv]).
	public_suffix_exact_reversed_([tr, web]).
	public_suffix_exact_reversed_([za, ac]).
	public_suffix_exact_reversed_([za, co]).
	public_suffix_exact_reversed_([za, edu]).
	public_suffix_exact_reversed_([za, gov]).
	public_suffix_exact_reversed_([za, law]).
	public_suffix_exact_reversed_([za, mil]).
	public_suffix_exact_reversed_([za, net]).
	public_suffix_exact_reversed_([za, ngo]).
	public_suffix_exact_reversed_([za, nom]).
	public_suffix_exact_reversed_([za, org]).
	public_suffix_exact_reversed_([za, school]).
	public_suffix_exact_reversed_([za, tm]).
	public_suffix_exact_reversed_([za, web]).
	public_suffix_exact_reversed_([io, github]).
	public_suffix_exact_reversed_([io, gitlab]).
	public_suffix_exact_reversed_([app, netlify]).
	public_suffix_exact_reversed_([app, run]).
	public_suffix_exact_reversed_([app, vercel]).
	public_suffix_exact_reversed_([app, web]).
	public_suffix_exact_reversed_([com, blogspot]).
	public_suffix_exact_reversed_([uk, co, blogspot]).
	public_suffix_exact_reversed_([net, cloudfront]).
	public_suffix_exact_reversed_([com, firebaseapp]).
	public_suffix_exact_reversed_([dev, fly]).
	public_suffix_exact_reversed_([com, herokuapp]).
	public_suffix_exact_reversed_([site, notion]).
	public_suffix_exact_reversed_([com, onrender]).
	public_suffix_exact_reversed_([dev, pages]).
	public_suffix_exact_reversed_([sh, surge]).
	public_suffix_exact_reversed_([dev, workers]).
	public_suffix_exact_reversed_([net, azurewebsites]).

	public_suffix_wildcard_base_reversed_([jp, kawasaki]).
	public_suffix_wildcard_base_reversed_([jp, kitakyushu]).

	public_suffix_exception_reversed_([jp, kawasaki, city]).
	public_suffix_exception_reversed_([jp, kitakyushu, city]).

	literal_or_single_label_host(Host) :-
		ipv6_host(Host),
		!.
	literal_or_single_label_host(Host) :-
		ipv4_host(Host),
		!.
	literal_or_single_label_host(Host) :-
		host_labels(Host, [_Label]).

	ipv6_host(Host) :-
		atom_codes(Host, Codes),
		memberchk(0':, Codes).

	ipv4_host(Host) :-
		host_labels(Host, [A, B, C, D]),
		ipv4_octet_atom(A),
		ipv4_octet_atom(B),
		ipv4_octet_atom(C),
		ipv4_octet_atom(D).

	ipv4_octet_atom(Atom) :-
		atom_codes(Atom, Codes),
		digit_codes(Codes),
		number_codes(Value, Codes),
		Value >= 0,
		Value =< 255.

	empty_label(Labels) :-
		member(Label, Labels),
		Label == ''.

	strip_userinfo_codes(AuthorityCodes0, AuthorityCodes) :-
		reverse(AuthorityCodes0, ReversedAuthorityCodes0),
		( 	split_once(0'@, ReversedAuthorityCodes0, ReversedAuthorityCodes, _IgnoredUserinfoCodes) ->
			reverse(ReversedAuthorityCodes, AuthorityCodes)
		; 	AuthorityCodes = AuthorityCodes0
		).

	split_last_colon(AuthorityCodes, HostCodes, PortCodes) :-
		reverse(AuthorityCodes, ReversedAuthorityCodes),
		split_once(0':, ReversedAuthorityCodes, ReversedPortCodes, ReversedHostCodes),
		reverse(ReversedHostCodes, HostCodes),
		reverse(ReversedPortCodes, PortCodes).

	split_once(Separator, [Separator| AfterCodes], [], AfterCodes) :-
		!.
	split_once(Separator, [Code| Codes], [Code| BeforeCodes], AfterCodes) :-
		split_once(Separator, Codes, BeforeCodes, AfterCodes).

	digit_codes([]).
	digit_codes([Code| Codes]) :-
		Code >= 0'0,
		Code =< 0'9,
		digit_codes(Codes).

:- end_category.
