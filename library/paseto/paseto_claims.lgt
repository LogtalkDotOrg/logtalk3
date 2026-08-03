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


:- object(paseto_claims,
	imports(paseto_claims_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'PASETO JSON claims lookup and validation predicates.'
	]).

	:- public(validate_claims/3).
	:- mode(validate_claims(+term, +list(compound), +list(compound)), one_or_error).
	:- info(validate_claims/3, [
		comment is 'Validates claims using registered-claim defaults, a policy list, and options.',
		argnames is ['Claims', 'Policy', 'Options']
	]).

	:- public(validate_claim/3).
	:- mode(validate_claim(+term, +compound, +list(compound)), one_or_error).
	:- meta_predicate(validate_claim(*, *, *)).
	:- info(validate_claim/3, [
		comment is 'Validates one claim policy against a claims object.',
		argnames is ['Claims', 'ClaimPolicy', 'Options']
	]).

	:- public(claim/3).
	:- mode(claim(+term, +atom, -term), zero_or_one).
	:- info(claim/3, [
		comment is 'Looks up a claim value by name.',
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
		^^json_object(Claims),
		validate_required_exp(Claims, MergedOptions),
		validate_required_claims(Claims, MergedOptions),
		validate_claim_policy(Policy, Claims, MergedOptions).

	validate_claim(Claims, claim(Name, required), _) :-
		!,
		required_claim(Name, Claims, _).
	validate_claim(Claims, claim(Name, expected(Expected)), _) :-
		!,
		required_claim(Name, Claims, Value),
		(	Value == Expected ->
			true
		;	domain_error(paseto_claim(Name), Value)
		).
	validate_claim(Claims, claim(Name, one_of(ExpectedValues)), _) :-
		!,
		required_claim(Name, Claims, Value),
		(	member(Value, ExpectedValues) ->
			true
		;	domain_error(paseto_claim(Name), Value)
		).
	validate_claim(Claims, claim(Name, contains(Expected)), _) :-
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
	validate_claim(_, Policy, _) :-
		domain_error(paseto_claim_policy, Policy).

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
		^^option(required_claims(Names), Options),
		validate_required_claims_(Names, Claims).

	validate_required_claims_([], _) :-
		!.
	validate_required_claims_([Name| Names], Claims) :-
		(	^^json_member(Name, Claims, _) ->
			validate_required_claims_(Names, Claims)
		;	domain_error(paseto_claims, missing(Name))
		).

	validate_claim_policy([], _, _) :-
		!.
	validate_claim_policy([Policy| Policies], Claims, Options) :-
		validate_claim(Claims, Policy, Options),
		validate_claim_policy(Policies, Claims, Options).

	required_claim(Name, Claims, Value) :-
		(	^^json_member(Name, Claims, Value) ->
			true
		;	domain_error(paseto_claims, missing(Name))
		).

	validate_contains(Name, Value, Expected) :-
		atom(Value),
		!,
		(	Value == Expected ->
			true
		;	domain_error(paseto_claim(Name), Value)
		).
	validate_contains(_, Values, Expected) :-
		list::valid(Values), member(Expected, Values), !.
	validate_contains(Name, Value, _) :-
		domain_error(paseto_claim(Name), Value).

	current_time(Options, Now) :-
		(	^^option(now(Now), Options) ->
			true
		;	time_stamp(Now)
		).

	validate_time_claim(Name, Claims, Now, ClockSkew, expiration, _) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Now =< Time + ClockSkew ->
			true
		;	domain_error(paseto_claim(Name), Time)
		).
	validate_time_claim(Name, Claims, Now, ClockSkew, not_before, _) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Now + ClockSkew >= Time ->
			true
		;	domain_error(paseto_claim(Name), Time)
		).
	validate_time_claim(Name, Claims, Now, ClockSkew, issued_at, Options) :-
		!,
		required_claim(Name, Claims, Time),
		validate_time_number(Name, Time),
		(	Time =< Now + ClockSkew ->
			validate_max_age(Name, Time, Now, Options)
		;	domain_error(paseto_claim(Name), Time)
		).
	validate_time_claim(_, _, _, _, Kind, _) :-
		domain_error(paseto_time_claim_kind, Kind).

	validate_max_age(Name, Time, Now, Options) :-
		(	^^option(max_age(MaxAge), Options) ->
			(	Now =< Time + MaxAge ->
				true
			;	domain_error(paseto_claim(Name), Time)
			)
		;	true
		).

	validate_time_number(Name, Time) :-
		(	number(Time) ->
			true
		;	type_error(time_number, Name-Time)
		).

:- end_object.
