%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_response,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Authorization callback parsing and state validation helpers.'
	]).

	:- public(authorization_response/3).
	:- mode(authorization_response(+atom, -compound, +list(compound)), one_or_error).
	:- info(authorization_response/3, [
		comment is 'Parses an authorization callback URL into either an authorization response or an authorization error term.',
		argnames is ['CallbackURL', 'Response', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``CallbackURL`` is a variable' - instantiation_error,
			'``CallbackURL`` is not a valid URL or relative reference' - domain_error(open_id_authorization_response, 'CallbackURL'),
			'The callback URL does not contain a query component and fragment parsing is not allowed' - domain_error(open_id_authorization_response, missing(parameters)),
			'The callback response is missing a required member' - domain_error(open_id_authorization_response, missing('Name')),
			'The callback issuer does not match the provider issuer' - domain_error(open_id_authorization_response, issuer_mismatch('Expected', 'Actual'))
		]
	]).

	:- public(authorization_code/4).
	:- mode(authorization_code(+atom, +compound, -atom, +list(compound)), one_or_error).
	:- info(authorization_code/4, [
		comment is 'Parses an authorization callback URL, validates the returned state against the session, and returns the authorization code.',
		argnames is ['CallbackURL', 'Session', 'Code', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Session`` is missing required state data' - domain_error(open_id_session, missing('Session', state)),
			'The callback response is an authorization error' - domain_error(open_id_authorization_response, authorization_error('Response')),
			'The callback response state does not match the session state' - domain_error(open_id_authorization_response, state_mismatch('Expected', 'Actual')),
			'The callback response is missing a required member' - domain_error(open_id_authorization_response, missing('Name'))
		]
	]).

	:- uses(http_core, [
		parse_request/2, property/2
	]).

	:- uses(list, [
		append/2, member/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	authorization_response(CallbackURL, Response, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		callback_pairs(CallbackURL, MergedOptions, Pairs),
		(	response_pair(Pairs, error, Error) ->
			error_response(Pairs, Error, Response)
		;	success_response(Pairs, Response)
		),
		validate_issuer(Response, MergedOptions).

	authorization_code(CallbackURL, Session, Code, Options) :-
		authorization_response(CallbackURL, Response, Options),
		(	Response = authorization_error(_) ->
			domain_error(open_id_authorization_response, authorization_error(Response))
		;	Response = authorization_response(Properties),
			response_property(Properties, code, Code),
			session_property(Session, state, ExpectedState),
			response_property(Properties, state, ActualState),
			(	ExpectedState == ActualState ->
				true
			;	domain_error(open_id_authorization_response, state_mismatch(ExpectedState, ActualState))
			)
		).

	callback_pairs(CallbackURL, Options, Pairs) :-
		parse_callback_components(CallbackURL, Components),
		(	member(query(Query), Components),
			Query \== '' ->
			query_pairs(Query, Pairs)
		;	^^option(allow_fragment_response(true), Options),
			member(fragment(Fragment), Components),
			Fragment \== '' ->
			query_pairs(Fragment, Pairs)
		;	domain_error(open_id_authorization_response, missing(parameters))
		).

	parse_callback_components(CallbackURL, Components) :-
		(	var(CallbackURL) ->
			instantiation_error
		;	url(atom)::parse(CallbackURL, Components, _Kind) ->
			true
		;	domain_error(open_id_authorization_response, CallbackURL)
		).

	query_pairs(Query, Pairs) :-
		atomic_list_concat(['GET /?', Query, ' HTTP/1.1\r\nHost: localhost\r\n\r\n'], Message),
		parse_request(atom(Message), Request),
		(	property(Request, query_pairs(Pairs0)) ->
			Pairs = Pairs0
		;	Pairs = []
		).

	success_response(Pairs, authorization_response(Properties)) :-
		required_response_pair(Pairs, code, Code),
		optional_response_properties(state, Pairs, StateProperties),
		optional_response_properties(iss, Pairs, IssuerProperties),
		optional_response_properties(session_state, Pairs, SessionStateProperties),
		append(
			[
				[code(Code)],
				StateProperties,
				IssuerProperties,
				SessionStateProperties,
				[raw(Pairs)]
			],
			Properties
		).

	error_response(Pairs, Error, authorization_error(Properties)) :-
		optional_response_properties(error_description, Pairs, DescriptionProperties),
		optional_response_properties(error_uri, Pairs, URIProperties),
		optional_response_properties(state, Pairs, StateProperties),
		optional_response_properties(iss, Pairs, IssuerProperties),
		optional_response_properties(session_state, Pairs, SessionStateProperties),
		append(
			[
				[error(Error)],
				DescriptionProperties,
				URIProperties,
				StateProperties,
				IssuerProperties,
				SessionStateProperties,
				[raw(Pairs)]
			],
			Properties
		).

	validate_issuer(Response, Options) :-
		(	^^option(provider(ProviderProperties), Options),
			response_issuer(Response, Issuer) ->
			^^provider_property(provider(ProviderProperties), issuer, ExpectedIssuer),
			(	Issuer == ExpectedIssuer ->
				true
			;	domain_error(open_id_authorization_response, issuer_mismatch(ExpectedIssuer, Issuer))
			)
		;	true
		).

	response_issuer(authorization_response(Properties), Issuer) :-
		response_property(Properties, iss, Issuer).
	response_issuer(authorization_error(Properties), Issuer) :-
		response_property(Properties, iss, Issuer).

	required_response_pair(Pairs, Name, Value) :-
		(	response_pair(Pairs, Name, Value) ->
			true
		;	domain_error(open_id_authorization_response, missing(Name))
		).

	response_pair([Name-Value| _], Name, Value) :-
		!.
	response_pair([_| Pairs], Name, Value) :-
		response_pair(Pairs, Name, Value).

	optional_response_properties(Name, Pairs, [Property]) :-
		response_pair(Pairs, Name, Value),
		!,
		Property =.. [Name, Value].
	optional_response_properties(_, _, []).

	response_property(Properties, Name, Value) :-
		Property =.. [Name, Value],
		^^option(Property, Properties),
		!.
	response_property(_Properties, Name, _Value) :-
		domain_error(open_id_authorization_response, missing(Name)).

	session_property(session(Properties), Name, Value) :-
		Property =.. [Name, Value],
		^^option(Property, Properties),
		!.
	session_property(Session, Name, _Value) :-
		domain_error(open_id_session, missing(Session, Name)).

:- end_object.
