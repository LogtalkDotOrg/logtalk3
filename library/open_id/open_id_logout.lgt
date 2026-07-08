%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_logout,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'RP-initiated logout URL helpers for OpenID Connect clients.'
	]).

	:- uses(list, [
		append/2
	]).

	:- public(logout_url/4).
	:- mode(logout_url(+compound, +compound, -atom, +list(compound)), one_or_error).
	:- info(logout_url/4, [
		comment is 'Builds a logout URL for an RP-initiated logout request.',
		argnames is ['Provider', 'Request', 'URL', 'Options'],
		exceptions is [
			'``Request`` is neither a direct options list nor a request wrapper term containing an options list' - domain_error(open_id_options, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The provider metadata is missing an end-session endpoint' - domain_error(open_id_provider, missing(end_session_endpoint)),
			'A request value that must be an atom is not an atom' - type_error(atom, 'Value'),
			'A ``ui_locales`` value is neither an atom nor a list of atoms' - domain_error(open_id_space_separated_atom, 'Value'),
			'The ``post_logout_redirect_uri`` is not secure' - domain_error(open_id_post_logout_redirect_uri, 'URL')
		]
	]).

	logout_url(Provider, Request0, URL, Options) :-
		^^normalize_options_term(Request0, Request),
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		provider_required_property(Provider, end_session_endpoint, Endpoint),
		logout_parameters(Request, MergedOptions, Parameters),
		(	Parameters == [] ->
			URL = Endpoint
		;	^^query_atom(Parameters, Query),
			^^append_query(Endpoint, Query, URL)
		).

	provider_required_property(Provider, Name, Value) :-
		(	^^provider_property(Provider, Name, Value) ->
			true
		;	domain_error(open_id_provider, missing(Name))
		).

	logout_parameters(Request, Options, Parameters) :-
		optional_atom_parameter(id_token_hint, Request, IdTokenHintParameters),
		optional_url_parameter(post_logout_redirect_uri, open_id_post_logout_redirect_uri, Request, Options, RedirectParameters),
		optional_atom_parameter(state, Request, StateParameters),
		optional_atom_parameter(client_id, Request, ClientIdParameters),
		optional_atom_parameter(logout_hint, Request, LogoutHintParameters),
		optional_space_parameter(ui_locales, Request, UILocalesParameters),
		append([
			IdTokenHintParameters,
			RedirectParameters,
			StateParameters,
			ClientIdParameters,
			LogoutHintParameters,
			UILocalesParameters
		], Parameters).

	optional_atom_parameter(Name, Request, [Name-Value]) :-
		option_term(Name, Value, Option),
		^^option(Option, Request),
		!,
		validate_atom(Value).
	optional_atom_parameter(_, _, []).

	optional_url_parameter(Name, Domain, Request, Options, [Name-Value]) :-
		option_term(Name, Value, Option),
		^^option(Option, Request),
		!,
		^^ensure_secure_url(Domain, Value, Options).
	optional_url_parameter(_, _, _, _, []).

	optional_space_parameter(Name, Request, [Name-Value]) :-
		option_term(Name, Value0, Option),
		^^option(Option, Request),
		!,
		^^space_atom(Value0, Value).
	optional_space_parameter(_, _, []).

	option_term(Name, Value, Term) :-
		Term =.. [Name, Value].

	validate_atom(Value) :-
		(	atom(Value) ->
			true
		;	type_error(atom, Value)
		).

:- end_object.
