%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_discovery,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-27,
		comment is 'OpenID Provider discovery metadata helpers.'
	]).

	:- uses(list, [
		append/2
	]).

	:- public(discovery_url/2).
	:- mode(discovery_url(+atom, -atom), one_or_error).
	:- info(discovery_url/2, [
		comment is 'Builds the discovery metadata URL for an issuer.',
		argnames is ['Issuer', 'URL'],
		exceptions is [
			'``Issuer`` is a variable' - instantiation_error,
			'``Issuer`` is not an absolute issuer URL' - domain_error(open_id_issuer, 'Issuer')
		]
	]).

	:- public(provider/3).
	:- mode(provider(+atom, +term, -compound), one_or_error).
	:- info(provider/3, [
		comment is 'Builds validated provider metadata from discovery JSON using default options.',
		argnames is ['ExpectedIssuer', 'JSON', 'Provider'],
		exceptions is [
			'The discovered issuer does not match ``ExpectedIssuer``' - domain_error(open_id_issuer, 'Issuer'),
			'A provider metadata URL is not secure' - domain_error(open_id_provider_metadata_url, 'URL')
		]
	]).

	:- public(provider/4).
	:- mode(provider(+atom, +term, -compound, +list(compound)), one_or_error).
	:- info(provider/4, [
		comment is 'Builds validated provider metadata from discovery JSON using the given options.',
		argnames is ['ExpectedIssuer', 'JSON', 'Provider', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The discovered issuer does not match ``ExpectedIssuer``' - domain_error(open_id_issuer, 'Issuer'),
			'A provider metadata URL is not secure' - domain_error(open_id_provider_metadata_url, 'URL')
		]
	]).

	:- public(property/3).
	:- mode(property(+compound, +atom, -term), zero_or_one).
	:- info(property/3, [
		comment is 'Looks up a property in validated provider metadata.',
		argnames is ['Provider', 'Name', 'Value']
	]).

	discovery_url(Issuer, URL) :-
		^^ensure_absolute_url(open_id_issuer, Issuer),
		strip_trailing_slash(Issuer, NormalizedIssuer),
		atom_concat(NormalizedIssuer, '/.well-known/openid-configuration', URL).

	provider(ExpectedIssuer, JSON, Provider) :-
		provider(ExpectedIssuer, JSON, Provider, []).

	provider(ExpectedIssuer, JSON, Provider, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^json_member(issuer, JSON, Issuer),
		(	Issuer == ExpectedIssuer ->
			true
		;	domain_error(open_id_issuer, Issuer)
		),
		required_url(authorization_endpoint, JSON, AuthorizationEndpoint, MergedOptions),
		required_url(token_endpoint, JSON, TokenEndpoint, MergedOptions),
		required_url(jwks_uri, JSON, JWKsURI, MergedOptions),
		optional_url_property(userinfo_endpoint, JSON, MergedOptions, UserInfoProperties),
		optional_url_property(end_session_endpoint, JSON, MergedOptions, EndSessionProperties),
		^^json_member_default(id_token_signing_alg_values_supported, JSON, ['RS256', 'ES256'], Algorithms),
		append(
			[
				[
					issuer(Issuer),
					authorization_endpoint(AuthorizationEndpoint),
					token_endpoint(TokenEndpoint),
					jwks_uri(JWKsURI)
				],
				UserInfoProperties,
				EndSessionProperties,
				[
					id_token_signing_alg_values_supported(Algorithms),
					raw(JSON)
				]
			],
			Properties
		),
		Provider = provider(Properties).

	property(Provider, Name, Value) :-
		^^provider_property(Provider, Name, Value).

	required_url(Name, JSON, URL, Options) :-
		^^json_member(Name, JSON, URL),
		^^ensure_secure_url(open_id_provider_metadata_url, URL, Options).

	optional_url_property(Name, JSON, Options, [Property]) :-
		^^json_member(Name, JSON, URL),
		!,
		^^ensure_secure_url(open_id_provider_metadata_url, URL, Options),
		Property =.. [Name, URL].
	optional_url_property(_, _, _, []).

	strip_trailing_slash(Issuer, NormalizedIssuer) :-
		atom_concat(NormalizedIssuer, '/', Issuer),
		!,
		strip_trailing_slash(NormalizedIssuer, NormalizedIssuer0),
		NormalizedIssuer = NormalizedIssuer0.
	strip_trailing_slash(Issuer, Issuer).

:- end_object.
