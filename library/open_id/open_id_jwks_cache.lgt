%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_jwks_cache,
	imports(open_id_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'JWKS caching and key-rotation helpers.'
	]).

	:- public(cached_jwks/3).
	:- mode(cached_jwks(+compound, -term, +list(compound)), one_or_error).
	:- info(cached_jwks/3, [
		comment is 'Returns a cached JWKS for a provider, refreshing it when the cache is stale or missing.',
		argnames is ['Provider', 'JWKSet', 'Options']
	]).

	:- public(verify_id_token/4).
	:- mode(verify_id_token(+atom, +compound, -term, +list(compound)), zero_or_one_or_error).
	:- info(verify_id_token/4, [
		comment is 'Verifies an ID-token using cached JWKS data, refreshing the cache once when the token key identifier is unknown.',
		argnames is ['Token', 'Provider', 'Claims', 'Options']
	]).

	:- public(clear/0).
	:- mode(clear, one).
	:- info(clear/0, [
		comment is 'Clears all cached JWKS entries.',
		argnames is []
	]).

	:- public(clear/1).
	:- mode(clear(+compound), one_or_error).
	:- info(clear/1, [
		comment is 'Clears the cached JWKS entry for a provider.',
		argnames is ['Provider']
	]).

	:- private(cached_jwks_/3).
	:- dynamic(cached_jwks_/3).
	:- mode(cached_jwks_(?atom, ?number, ?term), zero_or_more).
	:- info(cached_jwks_/3, [
		comment is 'Cached JWKS entries indexed by URI and fetch time.',
		argnames is ['URI', 'FetchedAt', 'JWKSet']
	]).

	:- uses(os, [
		time_stamp/1
	]).

	cached_jwks(Provider, JWKSet, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		provider_required_property(Provider, jwks_uri, URI),
		current_time(MergedOptions, Now),
		^^option(jwks_cache_ttl(TTL), MergedOptions),
		(	cached_jwks_(URI, FetchedAt, Cached),
			Now =< FetchedAt + TTL ->
			JWKSet = Cached
		;	refresh_jwks(Provider, URI, MergedOptions, JWKSet)
		).

	verify_id_token(Token, Provider, Claims, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		cached_jwks(Provider, JWKSet, MergedOptions),
		catch(
			open_id_jwt::verify_id_token(Token, Provider, JWKSet, Claims, MergedOptions),
			Error,
			retry_verification(Error, Token, Provider, Claims, MergedOptions)
		).

	clear :-
		retractall(cached_jwks_(_, _, _)).

	clear(Provider) :-
		provider_required_property(Provider, jwks_uri, URI),
		retractall(cached_jwks_(URI, _, _)).

	provider_required_property(Provider, Name, Value) :-
		(	^^provider_property(Provider, Name, Value) ->
			true
		;	domain_error(open_id_provider, missing(Name))
		).

	current_time(Options, Now) :-
		(	^^option(now(Now), Options) ->
			true
		;	time_stamp(Now)
		).

	refresh_jwks(Provider, URI, Options, JWKSet) :-
		open_id_client::jwks(Provider, JWKSet, Options),
		retractall(cached_jwks_(URI, _, _)),
		current_time(Options, Now),
		assertz(cached_jwks_(URI, Now, JWKSet)).

	retry_verification(Error, Token, Provider, Claims, Options) :-
		refreshable_key_error(Error),
		^^option(refresh_on_unknown_kid(true), Options),
		!,
		provider_required_property(Provider, jwks_uri, URI),
		refresh_jwks(Provider, URI, Options, JWKSet),
		open_id_jwt::verify_id_token(Token, Provider, JWKSet, Claims, Options).
	retry_verification(Error, _Token, _Provider, _Claims, _Options) :-
		throw(Error).

	refreshable_key_error(error(existence_error(jwt_jwk, _), _)).
	refreshable_key_error(error(existence_error(open_id_jwk, _), _)).

:- end_object.
