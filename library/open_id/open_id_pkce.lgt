%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_pkce,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'PKCE and authorization-request helpers for OpenID Connect clients.'
	]).

	:- public(code_verifier/2).
	:- mode(code_verifier(-atom, +list(compound)), one_or_error).
	:- info(code_verifier/2, [
		comment is 'Returns a valid PKCE code verifier, either provided in options or freshly generated.',
		argnames is ['Verifier', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Verifier`` is a variable' - instantiation_error,
			'``Verifier`` is not a valid PKCE code verifier' - domain_error(open_id_code_verifier, 'Verifier')
		]
	]).

	:- public(code_challenge/2).
	:- mode(code_challenge(+atom, -atom), one_or_error).
	:- info(code_challenge/2, [
		comment is 'Computes the S256 PKCE code challenge for a code verifier.',
		argnames is ['Verifier', 'Challenge'],
		exceptions is [
			'``Verifier`` is a variable' - instantiation_error,
			'``Verifier`` is not a valid PKCE code verifier' - domain_error(open_id_code_verifier, 'Verifier')
		]
	]).

	:- public(authorization_url/5).
	:- mode(authorization_url(+compound, +compound, -atom, -compound, +list(compound)), one_or_error).
	:- info(authorization_url/5, [
		comment is 'Builds an authorization URL and session data for an Authorization Code plus PKCE flow.',
		argnames is ['Provider', 'Request', 'URL', 'Session', 'Options'],
		exceptions is [
			'``Request`` is neither a direct options list nor a request wrapper term containing an options list' - domain_error(open_id_options, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Request`` is missing a required authorization request option' - domain_error(open_id_authorization_request, missing('Name')),
			'A request value that must be an atom is not an atom' - type_error(atom, 'Value'),
			'A scope value is neither an atom nor a list of atoms' - domain_error(open_id_space_separated_atom, 'Value'),
			'The PKCE code verifier is a variable' - instantiation_error,
			'The PKCE code verifier is not valid' - domain_error(open_id_code_verifier, 'Verifier')
		]
	]).

	:- uses(crypto, [
		random_bytes/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	code_verifier(Verifier, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		(	^^option(code_verifier(Verifier0), MergedOptions) ->
			Verifier = Verifier0
		;	random_bytes(32, Bytes),
			^^base64url_atom_bytes(Verifier, Bytes)
		),
		validate_code_verifier(Verifier).

	code_challenge(Verifier, Challenge) :-
		validate_code_verifier(Verifier),
		atom_codes(Verifier, Bytes),
		sha256::digest(Bytes, Digest),
		^^base64url_atom_bytes(Challenge, Digest).

	authorization_url(Provider, Request0, URL, Session, Options) :-
		^^normalize_options_term(Request0, Request),
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		required_request_option(client_id, Request, ClientId),
		required_request_option(redirect_uri, Request, RedirectURI),
		(	^^option(scope(Scope1), Request) ->
			Scope0 = Scope1
		;	Scope0 = [openid]
		),
		^^space_atom(Scope0, Scope),
		request_or_option_atom(state, Request, MergedOptions, State),
		request_or_option_atom(nonce, Request, MergedOptions, Nonce),
		code_verifier(Verifier, MergedOptions),
		code_challenge(Verifier, Challenge),
		^^provider_property(Provider, authorization_endpoint, Endpoint),
		(	^^option(extra(Extra0), Request) ->
			Extra = Extra0
		;	Extra = []
		),
		Parameters = [
			response_type-code,
			client_id-ClientId,
			redirect_uri-RedirectURI,
			scope-Scope,
			state-State,
			nonce-Nonce,
			code_challenge-Challenge,
			code_challenge_method-'S256'
		| Extra],
		^^query_atom(Parameters, Query),
		append_query(Endpoint, Query, URL),
		Session = session([
			state(State),
			nonce(Nonce),
			code_verifier(Verifier),
			redirect_uri(RedirectURI),
			client_id(ClientId)
		]).

	required_request_option(Name, Options, Value) :-
		(	option_term(Name, Value, Option),
			^^option(Option, Options) ->
			true
		;	domain_error(open_id_authorization_request, missing(Name))
		).

	request_or_option_atom(Name, Request, Options, Value) :-
		(	option_term(Name, Value0, Option),
			^^option(Option, Request) ->
			Value = Value0
		;	option_term(Name, Value0, Option),
			^^option(Option, Options) ->
			Value = Value0
		;	random_bytes(16, Bytes),
			^^base64url_atom_bytes(Value, Bytes)
		),
		(	atom(Value) ->
			true
		;	type_error(atom, Value)
		).

	option_term(Name, Value, Term) :-
		Term =.. [Name, Value].

	append_query(Endpoint, Query, URL) :-
		(	sub_atom(Endpoint, _, _, _, '?') ->
			atomic_list_concat([Endpoint, '&', Query], URL)
		;	atomic_list_concat([Endpoint, '?', Query], URL)
		).

	validate_code_verifier(Verifier) :-
		(	var(Verifier) ->
			instantiation_error
		;	atom(Verifier),
			atom_length(Verifier, Length),
			Length >= 43,
			Length =< 128 ->
			true
		;	domain_error(open_id_code_verifier, Verifier)
		).

:- end_object.
