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


:- object(http_cors,
	imports([options, http_origin_site_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Transport-neutral CORS request classification, preflight response generation, and response decoration helpers for normalized HTTP messages.'
	]).

	:- public(request_origin/2).
	:- mode(request_origin(+compound, -atom), zero_or_one).
	:- info(request_origin/2, [
		comment is 'Returns the single request ``Origin`` header value when present.',
		argnames is ['Request', 'Origin']
	]).

	:- public(is_cors_request/1).
	:- mode(is_cors_request(+compound), zero_or_one).
	:- info(is_cors_request/1, [
		comment is 'Succeeds when the request carries a single ``Origin`` header value.',
		argnames is ['Request']
	]).

	:- public(is_preflight_request/1).
	:- mode(is_preflight_request(+compound), zero_or_one).
	:- info(is_preflight_request/1, [
		comment is 'Succeeds when the request is a CORS preflight request with method ``options`` and a single ``Access-Control-Request-Method`` header value.',
		argnames is ['Request']
	]).

	:- public(preflight_response/3).
	:- mode(preflight_response(+compound, -compound, +list(compound)), one_or_error).
	:- info(preflight_response/3, [
		comment is 'Builds a normalized CORS preflight response using the given policy options. Allowed preflight requests return ``200 OK`` with the relevant ``Access-Control-*`` headers and denied requests return ``403 Forbidden``. Non-preflight requests raise an error.',
		argnames is ['Request', 'Response', 'Options'],
		exceptions is [
			'``Request`` is not a CORS preflight request' - domain_error(http_cors_preflight_request, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid CORS option combination' - domain_error(http_cors_options, 'Options'),
			'The generated response violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header')
		]
	]).

	:- public(add_response_headers/4).
	:- mode(add_response_headers(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(add_response_headers/4, [
		comment is 'Decorates a normalized response with the relevant CORS response headers when the request matches and the policy allows it. Denied requests preserve or add cache-relevant ``Vary`` metadata but do not add permission headers.',
		argnames is ['Request', 'Response0', 'Response', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid CORS option combination' - domain_error(http_cors_options, 'Options'),
			'``Response0`` is not a valid normalized HTTP response term' - domain_error(http_response, 'Response0'),
			'The decorated response violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header')
		]
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	request_origin(Request, Origin) :-
		single_header_value(Request, origin, Origin).

	is_cors_request(Request) :-
		request_origin(Request, _).

	is_preflight_request(Request) :-
		Request = request(Method, _, _, _, _, _),
		Method == options,
		request_origin(Request, _),
		requested_method(Request, _).

	preflight_response(Request, Response, UserOptions) :-
		resolve_options(Request, UserOptions, Options),
		(	is_preflight_request(Request) ->
			Request = request(_, _, Version, _, _, _),
			(	preflight_headers(Request, Options, Headers0, VaryTokens) ->
				finalize_headers(VaryTokens, Headers0, Headers),
				http_core::response(Version, status(200, 'OK'), Headers, empty, [], Response)
			;	preflight_denied_vary_tokens(Request, Options, VaryTokens),
				finalize_headers(VaryTokens, [], Headers),
				http_core::response(Version, status(403, 'Forbidden'), Headers, empty, [], Response)
			)
		;	domain_error(http_cors_preflight_request, Request)
		), !.

	add_response_headers(Request, Response0, Response, UserOptions) :-
		resolve_options(Request, UserOptions, Options),
		(	is_preflight_request(Request) ->
			(	preflight_headers(Request, Options, Headers, VaryTokens) ->
				merge_response_headers(Response0, Headers, VaryTokens, Response)
			;	preflight_denied_vary_tokens(Request, Options, VaryTokens),
				merge_denied_response_headers(VaryTokens, Response0, Response)
			)
		;	actual_headers(Request, Response0, Options, Headers, VaryTokens) ->
			merge_response_headers(Response0, Headers, VaryTokens, Response)
		;	actual_denied_vary_tokens(Options, VaryTokens),
			merge_denied_response_headers(VaryTokens, Response0, Response)
		),
		!.

	valid_option(allowed_origins(Origins)) :-
		(	Origins == any ->
			true
		;	valid_origin_list(Origins)
		).
	valid_option(allowed_methods(Methods)) :-
		(	Methods == any ->
			true
		;	wildcard_method_list(Methods) ->
			true
		;	valid_method_list(Methods)
		).
	valid_option(allowed_headers(Headers)) :-
		(	Headers == any ->
			true
		;	Headers == requested ->
			true
		;	valid_header_name_list(Headers)
		).
	valid_option(expose_headers(Headers)) :-
		(	Headers == any ->
			true
		;	valid_header_name_list(Headers)
		).
	valid_option(allow_credentials(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(max_age(Seconds)) :-
		(	Seconds == none ->
			true
		;	integer(Seconds),
			Seconds >= 0
		).

	default_option(allowed_origins([])).
	default_option(allowed_methods([get])).
	default_option(allowed_headers(requested)).
	default_option(expose_headers([])).
	default_option(allow_credentials(false)).
	default_option(max_age(none)).

	fix_option(allowed_origins(Origins0), allowed_origins(Origins)) :-
		unique_preserving_order(Origins0, Origins).
	fix_option(allowed_methods(Methods0), allowed_methods(any)) :-
		unique_preserving_order(Methods0, ['*']),
		!.
	fix_option(allowed_methods(Methods0), allowed_methods(Methods)) :-
		unique_preserving_order(Methods0, Methods).
	fix_option(allowed_headers(Headers0), allowed_headers(any)) :-
		Headers0 \== requested,
		Headers0 \== any,
		unique_preserving_order(Headers0, ['*']),
		!.
	fix_option(allowed_headers(Headers0), allowed_headers(Headers)) :-
		Headers0 \== requested,
		Headers0 \== any,
		unique_preserving_order(Headers0, Headers).
	fix_option(expose_headers(Headers0), expose_headers(any)) :-
		Headers0 \== any,
		unique_preserving_order(Headers0, ['*']),
		!.
	fix_option(expose_headers(Headers0), expose_headers(Headers)) :-
		Headers0 \== any,
		unique_preserving_order(Headers0, Headers).

	resolve_options(Request, UserOptions, Options) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, BaseOptions),
		request_route_options(Request, RouteOptions),
		(	RouteOptions == [] ->
			Options0 = BaseOptions
		;	^^check_options(RouteOptions),
			overlay_options(RouteOptions, BaseOptions, Options0)
		),
		fix_options_list(Options0, Options0a),
		constrain_allowed_methods(Request, Options0a, Options1),
		validate_option_semantics(Options1),
		Options = Options1.

	fix_options_list([], []).
	fix_options_list([Option| Options], [FixedOption| FixedOptions]) :-
		(	fix_option(Option, FixedOption) ->
			true
		;	FixedOption = Option
		),
		fix_options_list(Options, FixedOptions).

	overlay_options(Overrides, BaseOptions, Options) :-
		filter_overridden_options(BaseOptions, Overrides, FilteredOptions),
		append(Overrides, FilteredOptions, Options).

	filter_overridden_options([], _Overrides, []).
	filter_overridden_options([BaseOption| BaseOptions], Overrides, FilteredOptions) :-
		(	overridden_option(BaseOption, Overrides) ->
			FilteredOptions = Tail
		;	FilteredOptions = [BaseOption| Tail]
		),
		filter_overridden_options(BaseOptions, Overrides, Tail).

	overridden_option(Option, [Override| _]) :-
		same_option_kind(Option, Override),
		!.
	overridden_option(Option, [_| Overrides]) :-
		overridden_option(Option, Overrides).

	same_option_kind(Option1, Option2) :-
		functor(Option1, Functor, Arity),
		functor(Option2, Functor, Arity).

	constrain_allowed_methods(Request, Options0, Options) :-
		^^option(allowed_methods(AllowedMethods0), Options0),
		(	AllowedMethods0 == any ->
			(	request_effective_methods(Request, EffectiveMethods0) ->
				filter_non_options_methods(EffectiveMethods0, AllowedMethods),
				replace_option(allowed_methods(AllowedMethods), Options0, Options)
			;	domain_error(http_cors_options, [allowed_methods(any)])
			)
		;	request_effective_methods(Request, EffectiveMethods0) ->
			filter_non_options_methods(EffectiveMethods0, EffectiveMethods),
			intersection_preserving_order(AllowedMethods0, EffectiveMethods, AllowedMethods),
			replace_option(allowed_methods(AllowedMethods), Options0, Options)
		;	Options = Options0
		).

	replace_option(Option, Options0, Options) :-
		remove_same_kind_options(Options0, Option, FilteredOptions),
		Options = [Option| FilteredOptions].

	remove_same_kind_options([], _Option, []).
	remove_same_kind_options([Candidate| Candidates], Option, FilteredOptions) :-
		(	same_option_kind(Candidate, Option) ->
			FilteredOptions = Tail
		;	FilteredOptions = [Candidate| Tail]
		),
		remove_same_kind_options(Candidates, Option, Tail).

	validate_option_semantics(Options) :-
		validate_allowed_headers_semantics(Options),
		validate_expose_headers_semantics(Options).

	validate_allowed_headers_semantics(Options) :-
		^^option(allowed_headers(AllowedHeaders), Options),
		validate_header_wildcard_option(allowed_headers, AllowedHeaders).

	validate_expose_headers_semantics(Options) :-
		^^option(expose_headers(ExposeHeaders), Options),
		validate_header_wildcard_option(expose_headers, ExposeHeaders).

	validate_header_wildcard_option(_OptionName, requested) :-
		!.
	validate_header_wildcard_option(_OptionName, any) :-
		!.
	validate_header_wildcard_option(_OptionName, Headers) :-
		\+ member('*', Headers),
		!.
	validate_header_wildcard_option(OptionName, Headers) :-
		Option =.. [OptionName, Headers],
		domain_error(http_cors_options, [Option]).

	actual_headers(Request, Response0, Options, Headers, VaryTokens) :-
		request_origin(Request, Origin),
		allowed_origin(Origin, Options, AllowOrigin, VaryTokens),
		^^option(allow_credentials(AllowCredentials), Options),
		actual_expose_headers_value(Response0, Options, AllowCredentials, ExposeHeadersValue),
		actual_header_list(AllowOrigin, AllowCredentials, ExposeHeadersValue, Headers).

	preflight_headers(Request, Options, Headers, VaryTokens) :-
		request_origin(Request, Origin),
		requested_method(Request, RequestedMethod),
		requested_header_names(Request, RequestedHeaders),
		allowed_origin(Origin, Options, AllowOrigin, OriginVaryTokens),
		^^option(allowed_methods(AllowedMethods), Options),
		memberchk(RequestedMethod, AllowedMethods),
		allowed_headers_value(RequestedHeaders, Options, AllowHeadersValue),
		^^option(allow_credentials(AllowCredentials), Options),
		^^option(max_age(MaxAge), Options),
		response_allow_header(Request, AllowHeaderValue),
		preflight_header_list(AllowOrigin, AllowedMethods, AllowHeadersValue, AllowCredentials, MaxAge, AllowHeaderValue, Headers),
		preflight_vary_tokens(OriginVaryTokens, Request, VaryTokens).

	allowed_origin(Origin, Options, AllowOrigin, VaryTokens) :-
		^^option(allowed_origins(any), Options),
		!,
		^^option(allow_credentials(AllowCredentials), Options),
		(	AllowCredentials == true ->
			AllowOrigin = Origin,
			VaryTokens = [origin]
		;	AllowOrigin = ('*'),
			VaryTokens = []
		).
	allowed_origin(Origin, Options, Origin, [origin]) :-
		^^option(allowed_origins(Origins), Options),
		memberchk(Origin, Origins).
	allowed_origin(Origin, Options, Origin, [origin]) :-
		^^option(allowed_origins(Origins), Options),
		origin_matches_pattern(Origin, Origins).

	allowed_headers_value([], _Options, none) :-
		!.
	allowed_headers_value(RequestedHeaders, Options, Value) :-
		^^option(allowed_headers(requested), Options),
		!,
		header_name_list_atom(RequestedHeaders, Value).
	allowed_headers_value(RequestedHeaders, Options, Value) :-
		^^option(allowed_headers(any), Options),
		!,
		wildcard_allowed_headers_value(RequestedHeaders, Options, Value).
	allowed_headers_value(RequestedHeaders, Options, Value) :-
		^^option(allowed_headers(AllowedHeaders), Options),
		header_subset(RequestedHeaders, AllowedHeaders),
		header_name_list_atom(AllowedHeaders, Value).

	wildcard_allowed_headers_value(RequestedHeaders, Options, '*') :-
		^^option(allow_credentials(false), Options),
		\+ member(authorization, RequestedHeaders),
		!.
	wildcard_allowed_headers_value(RequestedHeaders, _Options, Value) :-
		header_name_list_atom(RequestedHeaders, Value).

	header_subset([], _AllowedHeaders).
	header_subset([Header| Headers], AllowedHeaders) :-
		memberchk(Header, AllowedHeaders),
		header_subset(Headers, AllowedHeaders).

	preflight_vary_tokens(OriginVaryTokens, Request, VaryTokens) :-
		(	requested_headers_header_present(Request) ->
			HeaderTokens = [access_control_request_headers]
		;	HeaderTokens = []
		),
		append(OriginVaryTokens, [access_control_request_method| HeaderTokens], VaryTokens).

	preflight_denied_vary_tokens(Request, Options, VaryTokens) :-
		origin_vary_tokens(Options, OriginVaryTokens),
		preflight_vary_tokens(OriginVaryTokens, Request, VaryTokens).

	actual_denied_vary_tokens(Options, VaryTokens) :-
		origin_vary_tokens(Options, VaryTokens).

	origin_vary_tokens(Options, []) :-
		^^option(allowed_origins(any), Options),
		!.
	origin_vary_tokens(_Options, [origin]).

	response_allow_header(Request, Value) :-
		request_effective_methods(Request, EffectiveMethods),
		method_list_atom(EffectiveMethods, Value).
	response_allow_header(_Request, none).

	actual_expose_headers_value(_Response, Options, false, '*') :-
		^^option(expose_headers(any), Options),
		!.
	actual_expose_headers_value(Response, Options, _AllowCredentials, Value) :-
		^^option(expose_headers(any), Options),
		!,
		exposed_header_names(Response, Names),
		(	Names == [] ->
			Value = none
		;	header_name_list_atom(Names, Value)
		).
	actual_expose_headers_value(_Response, Options, _AllowCredentials, none) :-
		^^option(expose_headers([]), Options),
		!.
	actual_expose_headers_value(_Response, Options, _AllowCredentials, Value) :-
		^^option(expose_headers(Headers), Options),
		header_name_list_atom(Headers, Value).

	exposed_header_names(response(_, _, Headers, _, _), Names) :-
		header_names(Headers, HeaderNames0),
		remove_forbidden_exposed_header_names(HeaderNames0, HeaderNames1),
		unique_preserving_order(HeaderNames1, Names).

	remove_forbidden_exposed_header_names([], []).
	remove_forbidden_exposed_header_names([Name| Names], FilteredNames) :-
		(	forbidden_exposed_header_name(Name) ->
			FilteredNames = Tail
		;	FilteredNames = [Name| Tail]
		),
		remove_forbidden_exposed_header_names(Names, Tail).

	forbidden_exposed_header_name(set_cookie).
	forbidden_exposed_header_name(set_cookie2).
	forbidden_exposed_header_name(vary).
	forbidden_exposed_header_name(access_control_allow_origin).
	forbidden_exposed_header_name(access_control_allow_credentials).
	forbidden_exposed_header_name(access_control_expose_headers).

	actual_header_list(AllowOrigin, AllowCredentials, ExposeHeadersValue, Headers) :-
		Headers = [access_control_allow_origin-AllowOrigin| Headers0],
		(	AllowCredentials == true ->
			Headers0 = [access_control_allow_credentials-'true'| Headers1]
		;	Headers0 = Headers1
		),
		(	ExposeHeadersValue == none ->
			Headers1 = []
		;	Headers1 = [access_control_expose_headers-ExposeHeadersValue]
		).

	preflight_header_list(AllowOrigin, AllowedMethods, AllowHeadersValue, AllowCredentials, MaxAge, AllowHeaderValue, Headers) :-
		method_list_atom(AllowedMethods, AllowedMethodsValue),
		Headers = [access_control_allow_methods-AllowedMethodsValue, access_control_allow_origin-AllowOrigin| Headers0],
		(	AllowHeadersValue == none ->
			Headers0 = Headers1
		;	Headers0 = [access_control_allow_headers-AllowHeadersValue| Headers1]
		),
		(	AllowCredentials == true ->
			Headers1 = [access_control_allow_credentials-'true'| Headers2]
		;	Headers1 = Headers2
		),
		(	MaxAge == none ->
			Headers2 = Headers3
		;	integer_atom(MaxAge, MaxAgeAtom),
			Headers2 = [access_control_max_age-MaxAgeAtom| Headers3]
		),
		(	AllowHeaderValue == none ->
			Headers3 = []
		;	Headers3 = [allow-AllowHeaderValue]
		).

	finalize_headers([], Headers, Headers).
	finalize_headers([VaryToken0| VaryTokens0], Headers, [vary-VaryValue| Headers]) :-
		unique_preserving_order([VaryToken0| VaryTokens0], VaryTokens),
		VaryTokens \== [],
		header_name_list_atom(VaryTokens, VaryValue).

	merge_denied_response_headers([], Response, Response).
	merge_denied_response_headers([VaryToken| VaryTokens], Response0, Response) :-
		merge_response_headers(Response0, [], [VaryToken| VaryTokens], Response).

	merge_response_headers(Response0, GeneratedHeaders, VaryTokens0, Response) :-
		Response0 = response(Version, Status, Headers0, Body, Properties),
		collect_vary_tokens(Headers0, ExistingVaryTokens),
		header_names(GeneratedHeaders, GeneratedNames0),
		(	ExistingVaryTokens == ('*') ->
			MergedHeaders = [vary-('*')| GeneratedHeaders],
			GeneratedNames = [vary| GeneratedNames0]
		;	append(ExistingVaryTokens, VaryTokens0, MergedVaryTokens0),
			unique_preserving_order(MergedVaryTokens0, MergedVaryTokens),
			(	MergedVaryTokens == [] ->
				MergedHeaders = GeneratedHeaders,
				GeneratedNames = GeneratedNames0
			;	header_name_list_atom(MergedVaryTokens, VaryValue),
				MergedHeaders = [vary-VaryValue| GeneratedHeaders],
				GeneratedNames = [vary| GeneratedNames0]
			)
		),
		remove_headers_by_names(Headers0, GeneratedNames, RemainingHeaders),
		append(MergedHeaders, RemainingHeaders, Headers),
		http_core::response(Version, Status, Headers, Body, Properties, Response).

	header_names([], []).
	header_names([Name-_| Headers], [Name| Names]) :-
		header_names(Headers, Names).

	remove_headers_by_names([], _Names, []).
	remove_headers_by_names([Name-Value| Headers], Names, RemainingHeaders) :-
		(	member(Name, Names) ->
			RemainingHeaders = Tail
		;	RemainingHeaders = [Name-Value| Tail]
		),
		remove_headers_by_names(Headers, Names, Tail).

	collect_vary_tokens(Headers, Tokens) :-
		collect_vary_tokens_list(Headers, Tokens0),
		(	Tokens0 == ('*') ->
			Tokens = ('*')
		;	unique_preserving_order(Tokens0, Tokens)
		).

	collect_vary_tokens_list([], []).
	collect_vary_tokens_list([vary-Value| _], '*') :-
		vary_star_value(Value),
		!.
	collect_vary_tokens_list([vary-Value| Headers], Tokens) :-
		!,
		collect_vary_tokens_list(Headers, RestTokens),
		(	RestTokens == ('*') ->
			Tokens = ('*')
		;	parse_field_name_list(Value, Names) ->
			append(Names, RestTokens, Tokens)
		;	Tokens = RestTokens
		),
		!.
	collect_vary_tokens_list([_| Headers], Tokens) :-
		collect_vary_tokens_list(Headers, Tokens).

	vary_star_value(Value) :-
		atom(Value),
		atom_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		Codes == [0'*].

	single_header_value(request(_, _, _, Headers, _, _), Name, Value) :-
		single_header_value(Headers, Name, none, Value).

	single_header_value([], _Name, found(Value), Value) :-
		!.
	single_header_value([], _Name, none, _) :-
		fail.
	single_header_value([Name-HeaderValue| Headers], Name, none, Value) :-
		!,
		single_header_value(Headers, Name, found(HeaderValue), Value).
	single_header_value([Name-_| _], Name, found(_), _) :-
		!,
		fail.
	single_header_value([_| Headers], Name, State, Value) :-
		single_header_value(Headers, Name, State, Value).

	single_property_value(request(_, _, _, _, _, Properties), Functor, Value) :-
		single_property_value(Properties, Functor, none, Value).

	single_property_value([], _Functor, found(Value), Value) :-
		!.
	single_property_value([], _Functor, none, _) :-
		fail.
	single_property_value([Property| Properties], Functor, none, Value) :-
		property_functor_argument(Property, Functor, PropertyValue),
		!,
		single_property_value(Properties, Functor, found(PropertyValue), Value).
	single_property_value([Property| _], Functor, found(_), _) :-
		property_functor_argument(Property, Functor, _),
		!,
		fail.
	single_property_value([_| Properties], Functor, State, Value) :-
		single_property_value(Properties, Functor, State, Value).

	property_functor_argument(Property, Functor, Value) :-
		nonvar(Property),
		functor(Property, Functor, 1),
		arg(1, Property, Value).

	request_route_options(Request, RouteOptions) :-
		(	single_property_value(Request, cors, RouteOptions) ->
			true
		;	RouteOptions = []
		).

	request_effective_methods(Request, EffectiveMethods) :-
		single_property_value(Request, effective_methods, EffectiveMethods).

	requested_method(Request, Method) :-
		single_header_value(Request, access_control_request_method, Value),
		normalize_method_atom(Value, Method).

	requested_header_names(Request, Names) :-
		(	single_header_value(Request, access_control_request_headers, Value) ->
			parse_field_name_list(Value, Names)
		;	Names = []
		).

	requested_headers_header_present(Request) :-
		single_header_value(Request, access_control_request_headers, _).

	parse_field_name_list(Value, Names) :-
		atom(Value),
		atom_codes(Value, Codes),
		split_comma_codes(Codes, Segments0),
		normalize_field_name_segments(Segments0, Names0),
		unique_preserving_order(Names0, Names).

	split_comma_codes(Codes, Segments) :-
		split_comma_codes(Codes, [], [], Segments0),
		reverse(Segments0, Segments).

	split_comma_codes([], Current0, Segments0, [Current| Segments0]) :-
		reverse(Current0, Current).
	split_comma_codes([0',| Codes], Current0, Segments0, Segments) :-
		!,
		reverse(Current0, Current),
		split_comma_codes(Codes, [], [Current| Segments0], Segments).
	split_comma_codes([Code| Codes], Current0, Segments0, Segments) :-
		split_comma_codes(Codes, [Code| Current0], Segments0, Segments).

	normalize_field_name_segments([], []).
	normalize_field_name_segments([Segment| Segments], [Name| Names]) :-
		trim_ows_codes(Segment, TrimmedSegment),
		TrimmedSegment \== [],
		normalize_field_name_codes(TrimmedSegment, NormalizedCodes),
		atom_codes(Name, NormalizedCodes),
		valid_header_name_atom(Name),
		normalize_field_name_segments(Segments, Names).

	normalize_field_name_codes([], []).
	normalize_field_name_codes([0'-| Codes], [0'_| NormalizedCodes]) :-
		!,
		normalize_field_name_codes(Codes, NormalizedCodes).
	normalize_field_name_codes([Code| Codes], [NormalizedCode| NormalizedCodes]) :-
		lowercase_ascii_code(Code, NormalizedCode),
		normalize_field_name_codes(Codes, NormalizedCodes).

	normalize_method_atom(Value, Method) :-
		atom(Value),
		atom_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes1),
		Codes1 \== [],
		normalize_method_codes(Codes1, Codes),
		atom_codes(Method, Codes),
		valid_method_atom(Method).

	normalize_method_codes([], []).
	normalize_method_codes([0'-| Codes], [0'_| NormalizedCodes]) :-
		!,
		normalize_method_codes(Codes, NormalizedCodes).
	normalize_method_codes([Code| Codes], [NormalizedCode| NormalizedCodes]) :-
		lowercase_ascii_code(Code, NormalizedCode),
		normalize_method_codes(Codes, NormalizedCodes).

	method_list_atom(Methods, Atom) :-
		method_display_atoms(Methods, Atoms),
		join_atoms(Atoms, ', ', Atom).

	method_display_atoms([], []).
	method_display_atoms([Method| Methods], [Display| Atoms]) :-
		method_display_atom(Method, Display),
		method_display_atoms(Methods, Atoms).

	method_display_atom(Method, Display) :-
		atom_codes(Method, Codes0),
		method_display_codes(Codes0, Codes),
		atom_codes(Display, Codes).

	method_display_codes([], []).
	method_display_codes([0'_| Codes], [0'-| DisplayCodes]) :-
		!,
		method_display_codes(Codes, DisplayCodes).
	method_display_codes([Code| Codes], [DisplayCode| DisplayCodes]) :-
		uppercase_ascii_code(Code, DisplayCode),
		method_display_codes(Codes, DisplayCodes).

	header_name_list_atom(Names, Atom) :-
		header_name_display_atoms(Names, Atoms),
		join_atoms(Atoms, ', ', Atom).

	header_name_display_atoms([], []).
	header_name_display_atoms([Name| Names], [Display| Atoms]) :-
		header_name_display_atom(Name, Display),
		header_name_display_atoms(Names, Atoms).

	header_name_display_atom(Name, Display) :-
		atom_codes(Name, Codes0),
		header_name_display_codes(Codes0, true, Codes),
		atom_codes(Display, Codes).

	header_name_display_codes([], _Capitalize, []).
	header_name_display_codes([0'_| Codes], _Capitalize, [0'-| DisplayCodes]) :-
		!,
		header_name_display_codes(Codes, true, DisplayCodes).
	header_name_display_codes([Code| Codes], true, [DisplayCode| DisplayCodes]) :-
		!,
		uppercase_ascii_code(Code, DisplayCode),
		header_name_display_codes(Codes, false, DisplayCodes).
	header_name_display_codes([Code| Codes], false, [DisplayCode| DisplayCodes]) :-
		lowercase_ascii_code(Code, DisplayCode),
		header_name_display_codes(Codes, false, DisplayCodes).

	join_atoms([Atom], _Separator, Atom) :-
		!.
	join_atoms([], _Separator, '') :-
		!.
	join_atoms(Atoms, Separator, Atom) :-
		atom_codes(Separator, SeparatorCodes),
		join_atom_codes(Atoms, SeparatorCodes, Codes, []),
		atom_codes(Atom, Codes).

	join_atom_codes([], _SeparatorCodes, Tail, Tail).
	join_atom_codes([Atom], _SeparatorCodes, Codes, Tail) :-
		atom_codes(Atom, AtomCodes),
		append_codes(AtomCodes, Codes, Tail).
	join_atom_codes([Atom| Atoms], SeparatorCodes, Codes, Tail) :-
		atom_codes(Atom, AtomCodes),
		append_codes(AtomCodes, Codes, Codes1),
		append_codes(SeparatorCodes, Codes1, Codes2),
		join_atom_codes(Atoms, SeparatorCodes, Codes2, Tail).

	append_codes([], Codes, Codes).
	append_codes([Code| Rest], [Code| Codes], Tail) :-
		append_codes(Rest, Codes, Tail).

	unique_preserving_order(List, Unique) :-
		unique_preserving_order(List, [], Unique, []).

	unique_preserving_order([], _Seen, Unique, Unique).
	unique_preserving_order([Element| Elements], Seen0, Unique0, Unique) :-
		(	member(Element, Seen0) ->
			Seen1 = Seen0,
			Unique1 = Unique0
		;	Seen1 = [Element| Seen0],
			Unique0 = [Element| Unique1]
		),
		unique_preserving_order(Elements, Seen1, Unique1, Unique).

	intersection_preserving_order([], _Allowed, []).
	intersection_preserving_order([Element| Elements], Allowed, Intersection) :-
		(	member(Element, Allowed) ->
			Intersection = [Element| Rest]
		;	Intersection = Rest
		),
		intersection_preserving_order(Elements, Allowed, Rest).

	filter_non_options_methods([], []).
	filter_non_options_methods([Method| Methods], FilteredMethods) :-
		(	Method == options ->
			FilteredMethods = Rest
		;	FilteredMethods = [Method| Rest]
		),
		filter_non_options_methods(Methods, Rest).

	valid_origin_list([]).
	valid_origin_list([Origin| Origins]) :-
		valid_origin(Origin),
		valid_origin_list(Origins).

	valid_origin(Origin) :-
		atom(Origin),
		Origin \== '',
		atom_codes(Origin, Codes),
		(	contains_wildcard_code(Codes) ->
			wildcard_origin_pattern(Origin, _Scheme, _BaseLabels, _Port)
		;	true
		).

	origin_matches_pattern(Origin, [Pattern| _]) :-
		wildcard_origin_match(Origin, Pattern),
		!.
	origin_matches_pattern(Origin, [_| Patterns]) :-
		origin_matches_pattern(Origin, Patterns).

	wildcard_origin_match(Origin, Pattern) :-
		wildcard_origin_pattern(Pattern, Scheme, BaseLabels, Port),
		origin_endpoint_components(Origin, Scheme, HostLabels, Port),
		HostLabels = [_| BaseLabels].

	wildcard_origin_pattern(Pattern, Scheme, BaseLabels, Port) :-
		atom(Pattern),
		split_wildcard_origin(Pattern, BaseOrigin, BaseAuthorityCodes),
		\+ contains_wildcard_code(BaseAuthorityCodes),
		^^origin_endpoint(BaseOrigin, http_endpoint(Scheme, BaseHost, Port)),
		^^host_labels(BaseHost, BaseLabels).

	origin_endpoint_components(Origin, Scheme, HostLabels, Port) :-
		^^origin_endpoint(Origin, http_endpoint(Scheme, Host, Port)),
		^^host_labels(Host, HostLabels).

	split_wildcard_origin(Pattern, BaseOrigin, BaseAuthorityCodes) :-
		atom::split(Pattern, '://', [Scheme, WildcardAuthority]),
		Scheme \== '',
		WildcardAuthority \== '',
		atom_codes(WildcardAuthority, [0'*, 0'.| BaseAuthorityCodes]),
		BaseAuthorityCodes \== [],
		atom_codes(BaseAuthority, BaseAuthorityCodes),
		atom_concat(Scheme, '://', Prefix),
		atom_concat(Prefix, BaseAuthority, BaseOrigin).

	contains_wildcard_code([0'*| _]) :-
		!.
	contains_wildcard_code([_| Codes]) :-
		contains_wildcard_code(Codes).

	wildcard_method_list(Methods) :-
		ground(Methods),
		wildcard_method_list_nonvar(Methods).

	wildcard_method_list_nonvar(['*']).
	wildcard_method_list_nonvar(['*'| Methods]) :-
		wildcard_method_list_nonvar(Methods).

	valid_method_list([]).
	valid_method_list([Method| Methods]) :-
		valid_method_atom(Method),
		valid_method_list(Methods).

	valid_method_atom(Method) :-
		atom(Method),
		atom_codes(Method, [Code| Codes]),
		lowercase_alpha(Code),
		valid_method_codes(Codes).

	valid_method_codes([]).
	valid_method_codes([Code| Codes]) :-
		(	lowercase_alpha(Code)
		;	digit_code(Code)
		;	Code =:= 0'_
		),
		valid_method_codes(Codes).

	valid_header_name_list([]).
	valid_header_name_list([Header| Headers]) :-
		valid_header_name_atom(Header),
		valid_header_name_list(Headers).

	valid_header_name_atom(Name) :-
		atom(Name),
		atom_codes(Name, [Code| Codes]),
		normalized_header_name_code(Code),
		valid_header_name_codes(Codes).

	valid_header_name_codes([]).
	valid_header_name_codes([Code| Codes]) :-
		normalized_header_name_code(Code),
		valid_header_name_codes(Codes).

	normalized_header_name_code(Code) :-
		lowercase_alpha(Code),
		!.
	normalized_header_name_code(Code) :-
		digit_code(Code),
		!.
	normalized_header_name_code(0'_).
	normalized_header_name_code(0'!).
	normalized_header_name_code(0'#).
	normalized_header_name_code(0'$).
	normalized_header_name_code(0'%).
	normalized_header_name_code(0'&).
	normalized_header_name_code(39).
	normalized_header_name_code(0'*).
	normalized_header_name_code(0'+).
	normalized_header_name_code(0'.).
	normalized_header_name_code(0'^).
	normalized_header_name_code(96).
	normalized_header_name_code(0'|).
	normalized_header_name_code(0'~).

	lowercase_alpha(Code) :-
		Code >= 0'a,
		Code =< 0'z.

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	ows_code(0' ).
	ows_code(0'\t).

	trim_ows_codes(Codes, TrimmedCodes) :-
		trim_leading_ows_codes(Codes, LeadingTrimmedCodes),
		trim_trailing_ows_codes(LeadingTrimmedCodes, TrimmedCodes).

	trim_leading_ows_codes([Code| Codes], TrimmedCodes) :-
		ows_code(Code),
		!,
		trim_leading_ows_codes(Codes, TrimmedCodes).
	trim_leading_ows_codes(Codes, Codes).

	trim_trailing_ows_codes(Codes, TrimmedCodes) :-
		reverse(Codes, ReversedCodes),
		trim_leading_ows_codes(ReversedCodes, ReversedTrimmedCodes),
		reverse(ReversedTrimmedCodes, TrimmedCodes).

	lowercase_ascii_code(Code, LowercaseCode) :-
		Code >= 0'A,
		Code =< 0'Z,
		!,
		LowercaseCode is Code + 32.
	lowercase_ascii_code(Code, Code).

	uppercase_ascii_code(Code, UppercaseCode) :-
		Code >= 0'a,
		Code =< 0'z,
		!,
		UppercaseCode is Code - 32.
	uppercase_ascii_code(Code, Code).

	integer_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes).

:- end_object.
