%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(jwt_claims,
	imports(jwt_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Reusable JWT registered-claim validation helpers.'
	]).

	:- public(validate_claims/3).
	:- mode(validate_claims(+term, +list(compound), +list(compound)), one_or_error).
	:- info(validate_claims/3, [
		comment is 'Validates JWT claims using registered-claim defaults, a policy list, and options.',
		argnames is ['Claims', 'Policy', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'``Policy`` contains an invalid claim policy' - domain_error(jwt_claim_policy, 'Policy'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time'),
			'``Policy`` contains an invalid time-claim kind' - domain_error(jwt_time_claim_kind, 'Kind')
		]
	]).

	:- public(validate_claim/3).
	:- mode(validate_claim(+term, +compound, +list(compound)), one_or_error).
	:- meta_predicate(validate_claim(*, *, *)).
	:- info(validate_claim/3, [
		comment is 'Validates a single JWT claim policy against a claims JSON term.',
		argnames is ['Claims', 'ClaimPolicy', 'Options'],
		exceptions is [
			'A required claim is missing' - domain_error(jwt_claims, missing('Name')),
			'A claim has an invalid value' - domain_error(jwt_claim('Name'), 'Value'),
			'``ClaimPolicy`` is not a valid claim policy' - domain_error(jwt_claim_policy, 'ClaimPolicy'),
			'A time claim value is not a number' - type_error(time_number, 'Name'-'Time'),
			'``ClaimPolicy`` contains an invalid time-claim kind' - domain_error(jwt_time_claim_kind, 'Kind')
		]
	]).

	:- public(claim/3).
	:- mode(claim(+term, +atom, -term), zero_or_one).
	:- info(claim/3, [
		comment is 'Looks up a claim value by name in a claims JSON term.',
		argnames is ['Claims', 'Name', 'Value']
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		time_stamp/1
	]).

	validate_claims(Claims, Policy, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		validate_required_exp(Claims, MergedOptions),
		validate_required_claims(Claims, MergedOptions),
		validate_claim_policy(Policy, Claims, MergedOptions).

	validate_claim(Claims, claim(Name, required), _Options) :-
		!,
		required_claim(Name, Claims, _).
	validate_claim(Claims, claim(Name, expected(Expected)), _Options) :-
		!,
		required_claim(Name, Claims, Value),
		(	Value == Expected ->
			true
		;	domain_error(jwt_claim(Name), Value)
		).
	validate_claim(Claims, claim(Name, one_of(ExpectedValues)), _Options) :-
		!,
		required_claim(Name, Claims, Value),
		(	member(Value, ExpectedValues) ->
			true
		;	domain_error(jwt_claim(Name), Value)
		).
	validate_claim(Claims, claim(Name, contains(Expected)), _Options) :-
		!,
		required_claim(Name, Claims, Value),
		validate_contains(Name, Value, Expected).
	validate_claim(Claims, claim(Name, time(Kind)), Options) :-
		!,
		current_time(Options, Now),
		^^option(clock_skew(ClockSkew), Options),
		validate_time_claim(Name, Claims, Now, ClockSkew, Kind, Options).
	validate_claim(Claims, claim(Name, custom(Verifier)), Options) :-
		!,
		required_claim(Name, Claims, Value),
		call(Verifier, Claims, Name, Value, Options).
	validate_claim(_Claims, Policy, _Options) :-
		domain_error(jwt_claim_policy, Policy).

	claim(Claims, Name, Value) :-
		^^json_member(Name, Claims, Value).

	validate_required_exp(Claims, Options) :-
		(	^^option(allow_missing_exp(true), Options) ->
			(	^^json_member(exp, Claims, _) ->
				validate_claim(Claims, claim(exp, time(expiration)), Options)
			;	true
			)
		;	validate_claim(Claims, claim(exp, time(expiration)), Options)
		),
		!.

	validate_required_claims(Claims, Options) :-
		^^option(required_claims(RequiredClaims), Options),
		validate_required_claims_(RequiredClaims, Claims).

	validate_required_claims_([], _Claims).
	validate_required_claims_([Claim| Claims], JSON) :-
		(	^^json_member(Claim, JSON, _) ->
			validate_required_claims_(Claims, JSON)
		;	domain_error(jwt_claims, missing(Claim))
		).

	validate_claim_policy([], _Claims, _Options).
	validate_claim_policy([Policy| Policies], Claims, Options) :-
		validate_claim(Claims, Policy, Options),
		validate_claim_policy(Policies, Claims, Options).

	required_claim(Name, Claims, Value) :-
		(	^^json_member(Name, Claims, Value) ->
			true
		;	domain_error(jwt_claims, missing(Name))
		).

	validate_contains(Name, Value, Expected) :-
		atom(Value),
		!,
		(	Value == Expected ->
			true
		;	domain_error(jwt_claim(Name), Value)
		).
	validate_contains(_Name, Values, Expected) :-
		list::valid(Values),
		member(Expected, Values),
		!.
	validate_contains(Name, Value, _) :-
		domain_error(jwt_claim(Name), Value).

	current_time(Options, Now) :-
		(	^^option(now(Now), Options) ->
			true
		;	time_stamp(Now)
		).

	validate_time_claim(Name, Claims, Now, ClockSkew, expiration, _Options) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Now =< Time + ClockSkew ->
			true
		;	domain_error(jwt_claim(Name), Time)
		).
	validate_time_claim(Name, Claims, Now, ClockSkew, not_before, _Options) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Now + ClockSkew >= Time ->
			true
		;	domain_error(jwt_claim(Name), Time)
		).
	validate_time_claim(Name, Claims, Now, ClockSkew, issued_at, Options) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Time =< Now + ClockSkew ->
			validate_max_age(Name, Time, Now, Options)
		;	domain_error(jwt_claim(Name), Time)
		).
	validate_time_claim(_Name, _Claims, _Now, _ClockSkew, Kind, _Options) :-
		domain_error(jwt_time_claim_kind, Kind).

	validate_max_age(Name, Time, Now, Options) :-
		(	^^option(max_age(MaxAge), Options) ->
			(	Now =< Time + MaxAge ->
				true
			;	domain_error(jwt_claim(Name), Time)
			)
		;	true
		).

	validate_time_number(Name, Time) :-
		(	number(Time) ->
			true
		;	type_error(time_number, Name-Time)
		).

:- end_object.
