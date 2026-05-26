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


:- category(http_router_htmx).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-26,
		comment is 'Optional HTMX middleware helpers for router objects importing the ``http_router`` category.'
	]).

	:- protected(annotate_htmx_request/2).
	:- mode(annotate_htmx_request(+compound, -compound), one_or_error).
	:- info(annotate_htmx_request/2, [
		comment is 'Router middleware helper that annotates a normalized request with derived HTMX properties and continues the middleware chain.',
		argnames is ['Request', 'Action']
	]).

	:- protected(add_htmx_response_headers/3).
	:- mode(add_htmx_response_headers(+compound, +compound, -compound), one_or_error).
	:- info(add_htmx_response_headers/3, [
		comment is 'Router response-middleware helper that decorates a normalized response using ``htmx_response_options/1`` request or response properties.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- uses(list, [
		append/3
	]).

	annotate_htmx_request(Request, continue(AnnotatedRequest)) :-
		http_htmx::request_properties(Request, HTMXProperties),
		( 	HTMXProperties == [] ->
			AnnotatedRequest = Request
		; 	annotated_request(Request, HTMXProperties, AnnotatedRequest)
		).

	add_htmx_response_headers(Request, Response0, Response) :-
		request_htmx_response_options(Request, RequestOptions),
		response_htmx_response_options(Response0, ResponseOptions),
		overlay_options(ResponseOptions, RequestOptions, Options),
		( 	Options == [] ->
			Response = Response0
		; 	http_htmx::add_response_headers(Request, Response0, Response, Options)
		).

	annotated_request(Request, HTMXProperties, AnnotatedRequest) :-
		http::method(Request, Method),
		http::target(Request, Target),
		http::version(Request, Version),
		http::headers(Request, Headers),
		http::body(Request, Body),
		findall(Property, http::property(Request, Property), Properties0),
		overlay_properties(HTMXProperties, Properties0, Properties),
		http::request(Method, Target, Version, Headers, Body, Properties, AnnotatedRequest).

	request_htmx_response_options(Request, Options) :-
		http::property(Request, htmx_response_options(Options)),
		!.
	request_htmx_response_options(_Request, []).

	response_htmx_response_options(Response, Options) :-
		http::property(Response, htmx_response_options(Options)),
		!.
	response_htmx_response_options(_Response, []).

	overlay_options(Overrides, Options0, Options) :-
		filter_overridden_options(Options0, Overrides, FilteredOptions),
		append(Overrides, FilteredOptions, Options).

	filter_overridden_options([], _Overrides, []).
	filter_overridden_options([Option| Options0], Overrides, Options) :-
		( 	overridden_option(Option, Overrides) ->
			Options = Tail
		; 	Options = [Option| Tail]
		),
		filter_overridden_options(Options0, Overrides, Tail).

	overridden_option(Option, [Override| _]) :-
		same_option_kind(Option, Override),
		!.
	overridden_option(Option, [_| Overrides]) :-
		overridden_option(Option, Overrides).

	same_option_kind(Option1, Option2) :-
		functor(Option1, Functor, Arity),
		functor(Option2, Functor, Arity).

	overlay_properties(Overrides, Properties0, Properties) :-
		filter_overridden_properties(Properties0, Overrides, FilteredProperties),
		append(Overrides, FilteredProperties, Properties).

	filter_overridden_properties([], _Overrides, []).
	filter_overridden_properties([Property| Properties0], Overrides, Properties) :-
		( 	overridden_property(Property, Overrides) ->
			Properties = Tail
		; 	Properties = [Property| Tail]
		),
		filter_overridden_properties(Properties0, Overrides, Tail).

	overridden_property(Property, [Override| _]) :-
		same_property_kind(Property, Override),
		!.
	overridden_property(Property, [_| Overrides]) :-
		overridden_property(Property, Overrides).

	same_property_kind(Property1, Property2) :-
		functor(Property1, Functor, Arity),
		functor(Property2, Functor, Arity).

:- end_category.
