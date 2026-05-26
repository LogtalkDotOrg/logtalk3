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


:- object(http_htmx,
	imports([options, http_json_term_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-26,
		comment is 'Transport-neutral HTMX request classification, HTML reply helpers, and response decoration helpers for normalized HTTP messages.'
	]).

	:- public(is_htmx_request/1).
	:- mode(is_htmx_request(+compound), zero_or_one).
	:- info(is_htmx_request/1, [
		comment is 'Succeeds when the request carries a truthy ``HX-Request`` header value.',
		argnames is ['Request']
	]).

	:- public(is_boosted_request/1).
	:- mode(is_boosted_request(+compound), zero_or_one).
	:- info(is_boosted_request/1, [
		comment is 'Succeeds when the request carries a truthy ``HX-Boosted`` header value.',
		argnames is ['Request']
	]).

	:- public(is_history_restore_request/1).
	:- mode(is_history_restore_request(+compound), zero_or_one).
	:- info(is_history_restore_request/1, [
		comment is 'Succeeds when the request carries a truthy ``HX-History-Restore-Request`` header value.',
		argnames is ['Request']
	]).

	:- public(current_url/2).
	:- mode(current_url(+compound, -atom), zero_or_one).
	:- info(current_url/2, [
		comment is 'Returns the single request ``HX-Current-URL`` header value when present.',
		argnames is ['Request', 'URL']
	]).

	:- public(prompt/2).
	:- mode(prompt(+compound, -atom), zero_or_one).
	:- info(prompt/2, [
		comment is 'Returns the single request ``HX-Prompt`` header value when present.',
		argnames is ['Request', 'Prompt']
	]).

	:- public(target/2).
	:- mode(target(+compound, -atom), zero_or_one).
	:- info(target/2, [
		comment is 'Returns the single request ``HX-Target`` header value when present.',
		argnames is ['Request', 'Target']
	]).

	:- public(trigger/2).
	:- mode(trigger(+compound, -atom), zero_or_one).
	:- info(trigger/2, [
		comment is 'Returns the single request ``HX-Trigger`` header value when present.',
		argnames is ['Request', 'Trigger']
	]).

	:- public(trigger_name/2).
	:- mode(trigger_name(+compound, -atom), zero_or_one).
	:- info(trigger_name/2, [
		comment is 'Returns the single request ``HX-Trigger-Name`` header value when present.',
		argnames is ['Request', 'Name']
	]).

	:- public(request_properties/2).
	:- mode(request_properties(+compound, -list(compound)), one_or_error).
	:- info(request_properties/2, [
		comment is 'Returns a list of derived HTMX request properties computed from the normalized request headers.',
		argnames is ['Request', 'Properties']
	]).

	:- public(reply/3).
	:- mode(reply(+compound, +term, -compound), one_or_error).
	:- info(reply/3, [
		comment is 'Builds a normalized ``text/html`` response from a pre-rendered HTML atom or an ``html`` term/list using the default options.',
		argnames is ['Request', 'Content', 'Response']
	]).

	:- public(reply/4).
	:- mode(reply(+compound, +term, -compound, +list(compound)), one_or_error).
	:- info(reply/4, [
		comment is 'Builds a normalized ``text/html`` response from a pre-rendered HTML atom or an ``html`` term/list using the given options.',
		argnames is ['Request', 'Content', 'Response', 'Options']
	]).

	:- public(page_fragment_reply/4).
	:- mode(page_fragment_reply(+compound, +term, +term, -compound), one_or_error).
	:- info(page_fragment_reply/4, [
		comment is 'Builds a normalized ``text/html`` response using the full-page content for ordinary and boosted HTMX requests and the fragment content for non-boosted HTMX requests, with the default options.',
		argnames is ['Request', 'PageContent', 'FragmentContent', 'Response']
	]).

	:- public(page_fragment_reply/5).
	:- mode(page_fragment_reply(+compound, +term, +term, -compound, +list(compound)), one_or_error).
	:- info(page_fragment_reply/5, [
		comment is 'Builds a normalized ``text/html`` response using the full-page content for ordinary and boosted HTMX requests and the fragment content for non-boosted HTMX requests, with the given options.',
		argnames is ['Request', 'PageContent', 'FragmentContent', 'Response', 'Options']
	]).

	:- public(add_response_headers/4).
	:- mode(add_response_headers(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(add_response_headers/4, [
		comment is 'Decorates a normalized response with the relevant HTMX response headers when the request is an HTMX request.',
		argnames is ['Request', 'Response0', 'Response', 'Options']
	]).

	:- uses(list, [
		append/2, append/3, member/2
	]).

	:- uses(term_io, [
		with_output_to/2
	]).

	is_htmx_request(Request) :-
		validate_request(Request),
		request_boolean_header(Request, hx_request).

	is_boosted_request(Request) :-
		validate_request(Request),
		request_boolean_header(Request, hx_boosted).

	is_history_restore_request(Request) :-
		validate_request(Request),
		request_boolean_header(Request, hx_history_restore_request).

	current_url(Request, URL) :-
		validate_request(Request),
		request_text_header(Request, hx_current_url, URL).

	prompt(Request, Prompt) :-
		validate_request(Request),
		request_text_header(Request, hx_prompt, Prompt).

	target(Request, Target) :-
		validate_request(Request),
		request_text_header(Request, hx_target, Target).

	trigger(Request, Trigger) :-
		validate_request(Request),
		request_text_header(Request, hx_trigger, Trigger).

	trigger_name(Request, Name) :-
		validate_request(Request),
		request_text_header(Request, hx_trigger_name, Name).

	request_properties(Request, Properties) :-
		validate_request(Request),
		findall(Property, request_property(Request, Property), Properties).

	request_property(Request, htmx_request(true)) :-
		request_boolean_header(Request, hx_request).
	request_property(Request, htmx_boosted(true)) :-
		request_boolean_header(Request, hx_boosted).
	request_property(Request, htmx_target(Target)) :-
		request_text_header(Request, hx_target, Target).
	request_property(Request, htmx_trigger(Trigger)) :-
		request_text_header(Request, hx_trigger, Trigger).
	request_property(Request, htmx_trigger_name(Name)) :-
		request_text_header(Request, hx_trigger_name, Name).
	request_property(Request, htmx_current_url(URL)) :-
		request_text_header(Request, hx_current_url, URL).
	request_property(Request, htmx_prompt(Prompt)) :-
		request_text_header(Request, hx_prompt, Prompt).
	request_property(Request, htmx_history_restore_request(true)) :-
		request_boolean_header(Request, hx_history_restore_request).

	reply(Request, Content, Response) :-
		reply(Request, Content, Response, []).

	reply(Request, Content, Response, UserOptions) :-
		validate_request(Request),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		render_content(Content, HTML),
		^^option(status(Status), Options),
		^^option(headers(Headers), Options),
		http::version(Request, Version),
		http::response(Version, Status, Headers, content('text/html', text(HTML)), [], Response0),
		add_response_headers(Request, Response0, Response, Options).

	page_fragment_reply(Request, PageContent, FragmentContent, Response) :-
		page_fragment_reply(Request, PageContent, FragmentContent, Response, []).

	page_fragment_reply(Request, PageContent, FragmentContent, Response, UserOptions) :-
		validate_request(Request),
		( 	is_htmx_request(Request),
			\+ is_boosted_request(Request) ->
			Content = FragmentContent
		; 	Content = PageContent
		),
		reply(Request, Content, Response, UserOptions).

	add_response_headers(Request, Response0, Response, UserOptions) :-
		validate_request(Request),
		validate_response(Response0),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		( 	is_htmx_request(Request),
			htmx_response_headers(Options, Headers),
			Headers \== [] ->
			ensure_htmx_response_headers_allowed(Response0),
			merge_response_headers(Response0, Headers, Response)
		; 	Response = Response0
		).

	ensure_htmx_response_headers_allowed(Response) :-
		http::status(Response, Status),
		( 	htmx_response_headers_allowed(Status) ->
			true
		; 	domain_error(htmx_response_headers_status, Status)
		).

	htmx_response_headers_allowed(status(Code, _Reason)) :-
		Code >= 100,
		Code < 300,
		!.
	htmx_response_headers_allowed(status(Code, _Reason)) :-
		Code >= 400.

	valid_option(Option) :-
		ground(Option),
		valid_option_term(Option).

	valid_option_term(status(Status)) :-
		valid_status(Status).
	valid_option_term(headers(Headers)) :-
		valid_headers(Headers).
	valid_option_term(redirect(none)).
	valid_option_term(redirect(URL)) :-
		valid_non_empty_atom(URL).
	valid_option_term(refresh(Boolean)) :-
		valid_boolean(Boolean).
	valid_option_term(location(none)).
	valid_option_term(location(Value)) :-
		valid_location_value(Value).
	valid_option_term(push_url(none)).
	valid_option_term(push_url(Value)) :-
		valid_push_replace_value(Value).
	valid_option_term(replace_url(none)).
	valid_option_term(replace_url(Value)) :-
		valid_push_replace_value(Value).
	valid_option_term(reswap(none)).
	valid_option_term(reswap(Value)) :-
		valid_non_empty_atom(Value).
	valid_option_term(retarget(none)).
	valid_option_term(retarget(Value)) :-
		valid_non_empty_atom(Value).
	valid_option_term(reselect(none)).
	valid_option_term(reselect(Value)) :-
		valid_non_empty_atom(Value).
	valid_option_term(trigger(Value)) :-
		valid_trigger_value(Value).
	valid_option_term(trigger_after_settle(Value)) :-
		valid_trigger_value(Value).
	valid_option_term(trigger_after_swap(Value)) :-
		valid_trigger_value(Value).

	default_option(status(status(200, 'OK'))).
	default_option(headers([])).
	default_option(redirect(none)).
	default_option(refresh(false)).
	default_option(location(none)).
	default_option(push_url(none)).
	default_option(replace_url(none)).
	default_option(reswap(none)).
	default_option(retarget(none)).
	default_option(reselect(none)).
	default_option(trigger([])).
	default_option(trigger_after_settle([])).
	default_option(trigger_after_swap([])).

	render_content(Content, HTML) :-
		atom(Content),
		!,
		HTML = Content.
	render_content(Content, HTML) :-
		catch(
			with_output_to(atom(HTML), (
				current_output(Stream),
				html5::generate(stream(Stream), Content)
			)),
			_,
			fail
		),
		!.
	render_content(Content, _HTML) :-
		domain_error(http_htmx_content, Content).

	request_boolean_header(Request, Name) :-
		request_header_value(Request, Name, Value),
		header_true(Value).

	request_text_header(Request, Name, Atom) :-
		request_header_value(Request, Name, Value),
		text_atom(Value, Atom).

	request_header_value(Request, Name, Value) :-
		http::header(Request, Name, Value),
		!.

	header_true(Value) :-
		text_atom(Value, Atom0),
		lowercase_ascii_atom(Atom0, Atom),
		Atom == 'true'.

	htmx_response_headers(Options, Headers) :-
		redirect_header(Options, RedirectHeaders),
		refresh_header(Options, RefreshHeaders),
		location_header(Options, LocationHeaders),
		push_url_header(Options, PushHeaders),
		replace_url_header(Options, ReplaceHeaders),
		reswap_header(Options, ReswapHeaders),
		retarget_header(Options, RetargetHeaders),
		reselect_header(Options, ReselectHeaders),
		trigger_header(Options, TriggerHeaders),
		trigger_after_settle_header(Options, TriggerAfterSettleHeaders),
		trigger_after_swap_header(Options, TriggerAfterSwapHeaders),
		append([
			RedirectHeaders,
			RefreshHeaders,
			LocationHeaders,
			PushHeaders,
			ReplaceHeaders,
			ReswapHeaders,
			RetargetHeaders,
			ReselectHeaders,
			TriggerHeaders,
			TriggerAfterSettleHeaders,
			TriggerAfterSwapHeaders
		], Headers).

	redirect_header(Options, [hx_redirect-URL]) :-
		^^option(redirect(URL), Options),
		URL \== none,
		!.
	redirect_header(_Options, []).

	refresh_header(Options, [hx_refresh-'true']) :-
		^^option(refresh(true), Options),
		!.
	refresh_header(_Options, []).

	location_header(Options, [hx_location-HeaderValue]) :-
		^^option(location(Value), Options),
		Value \== none,
		!,
		location_header_value(Value, HeaderValue).
	location_header(_Options, []).

	push_url_header(Options, [hx_push_url-HeaderValue]) :-
		^^option(push_url(Value), Options),
		Value \== none,
		!,
		push_replace_header_value(Value, HeaderValue).
	push_url_header(_Options, []).

	replace_url_header(Options, [hx_replace_url-HeaderValue]) :-
		^^option(replace_url(Value), Options),
		Value \== none,
		!,
		push_replace_header_value(Value, HeaderValue).
	replace_url_header(_Options, []).

	reswap_header(Options, [hx_reswap-Value]) :-
		^^option(reswap(Value), Options),
		Value \== none,
		!.
	reswap_header(_Options, []).

	retarget_header(Options, [hx_retarget-Value]) :-
		^^option(retarget(Value), Options),
		Value \== none,
		!.
	retarget_header(_Options, []).

	reselect_header(Options, [hx_reselect-Value]) :-
		^^option(reselect(Value), Options),
		Value \== none,
		!.
	reselect_header(_Options, []).

	trigger_header(Options, [hx_trigger-HeaderValue]) :-
		^^option(trigger(Value), Options),
		Value \== [],
		!,
		trigger_header_value(Value, HeaderValue).
	trigger_header(_Options, []).

	trigger_after_settle_header(Options, [hx_trigger_after_settle-HeaderValue]) :-
		^^option(trigger_after_settle(Value), Options),
		Value \== [],
		!,
		trigger_header_value(Value, HeaderValue).
	trigger_after_settle_header(_Options, []).

	trigger_after_swap_header(Options, [hx_trigger_after_swap-HeaderValue]) :-
		^^option(trigger_after_swap(Value), Options),
		Value \== [],
		!,
		trigger_header_value(Value, HeaderValue).
	trigger_after_swap_header(_Options, []).

	location_header_value(Value, HeaderValue) :-
		atom(Value),
		!,
		HeaderValue = Value.
	location_header_value(Value, HeaderValue) :-
		json_header_value(Value, HeaderValue).

	push_replace_header_value(false, 'false') :-
		!.
	push_replace_header_value(Value, Value).

	trigger_header_value(Value, HeaderValue) :-
		atom(Value),
		!,
		HeaderValue = Value.
	trigger_header_value(Value, HeaderValue) :-
		valid_atom_list(Value),
		!,
		atomic_list_concat(Value, ', ', HeaderValue).
	trigger_header_value(Value, HeaderValue) :-
		json_header_value(Value, HeaderValue).

	json_header_value(Value, HeaderValue) :-
		^^normalize_json_value(Value, JSON),
		http::encode_body('application/json', JSON, [], Body),
		http::generate_body(atom(HeaderValue), Body, []).

	merge_response_headers(response(Version, Status, Headers0, Body, Properties), Headers, Response) :-
		overlay_headers(Headers, Headers0, MergedHeaders),
		http::response(Version, Status, MergedHeaders, Body, Properties, Response).

	overlay_headers(Overrides, Headers0, Headers) :-
		filter_overridden_headers(Headers0, Overrides, FilteredHeaders),
		append(Overrides, FilteredHeaders, Headers).

	filter_overridden_headers([], _Overrides, []).
	filter_overridden_headers([Header| Headers0], Overrides, Headers) :-
		( 	overridden_header(Header, Overrides) ->
			Headers = Tail
		; 	Headers = [Header| Tail]
		),
		filter_overridden_headers(Headers0, Overrides, Tail).

	overridden_header(Name-_, [Override| _]) :-
		Override = Name-_,
		!.
	overridden_header(Header, [_| Overrides]) :-
		overridden_header(Header, Overrides).

	validate_request(Request) :-
		http::is_request(Request),
		!.
	validate_request(Request) :-
		domain_error(http_request, Request).

	validate_response(Response) :-
		http::is_response(Response),
		!.
	validate_response(Response) :-
		domain_error(http_response, Response).

	valid_status(status(Code, Reason)) :-
		integer(Code),
		Code >= 100,
		Code =< 999,
		atom(Reason).

	valid_headers([]).
	valid_headers([Header| Headers]) :-
		valid_header(Header),
		valid_headers(Headers).

	valid_header(Name-Value) :-
		atom(Name),
		ground(Value).

	valid_boolean(true).
	valid_boolean(false).

	valid_non_empty_atom(Value) :-
		atom(Value),
		Value \== ''.

	valid_push_replace_value(false).
	valid_push_replace_value(Value) :-
		valid_non_empty_atom(Value).

	valid_location_value(Value) :-
		valid_non_empty_atom(Value),
		!.
	valid_location_value(Value) :-
		valid_location_object(Value).

	valid_trigger_value([]).
	valid_trigger_value(Value) :-
		valid_non_empty_atom(Value),
		!.
	valid_trigger_value(Value) :-
		valid_atom_list(Value),
		Value \== [],
		!.
	valid_trigger_value(Value) :-
		valid_trigger_object(Value).

	valid_location_object(Value) :-
		^^json_object_pairs(Value, Pairs),
		valid_location_pairs(Pairs, [], no).

	valid_location_pairs([], _SeenKeys, yes).
	valid_location_pairs([], _SeenKeys, no) :-
		fail.
	valid_location_pairs([Pair| Pairs], SeenKeys0, PathSeen0) :-
		^^pair_key_value(Pair, Key, PairValue),
		valid_unique_json_key(Key, SeenKeys0, SeenKeys),
		valid_location_pair(Key, PairValue, PathSeen0, PathSeen),
		valid_location_pairs(Pairs, SeenKeys, PathSeen).

	valid_location_pair(path, Value, _PathSeen0, yes) :-
		valid_non_empty_atom(Value).
	valid_location_pair(target, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(swap, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(select, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(push, Value, PathSeen, PathSeen) :-
		valid_push_replace_value(Value).
	valid_location_pair(replace, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(source, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(event, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(handler, Value, PathSeen, PathSeen) :-
		valid_non_empty_atom(Value).
	valid_location_pair(values, Value, PathSeen, PathSeen) :-
		valid_json_object_value(Value).
	valid_location_pair(headers, Value, PathSeen, PathSeen) :-
		valid_json_object_value(Value).

	valid_trigger_object(Value) :-
		^^json_object_pairs(Value, Pairs),
		Pairs \== [],
		valid_trigger_pairs(Pairs, []).

	valid_trigger_pairs([], _SeenKeys).
	valid_trigger_pairs([Pair| Pairs], SeenKeys0) :-
		^^pair_key_value(Pair, Key, PairValue),
		valid_unique_json_key(Key, SeenKeys0, SeenKeys),
		valid_json_value(PairValue),
		valid_trigger_pairs(Pairs, SeenKeys).

	valid_unique_json_key(Key, SeenKeys0, [Key| SeenKeys0]) :-
		valid_non_empty_atom(Key),
		\+ member(Key, SeenKeys0).

	valid_json_object_value(Value) :-
		^^json_object_pairs(Value, Pairs),
		valid_json_object_pairs(Pairs).

	valid_json_object_pairs([]).
	valid_json_object_pairs([Pair| Pairs]) :-
		^^pair_key_value(Pair, Key, PairValue),
		valid_non_empty_atom(Key),
		valid_json_value(PairValue),
		valid_json_object_pairs(Pairs).

	valid_json_value(Value) :-
		ground(Value),
		catch(^^normalize_json_value(Value, _), _, fail).

	valid_atom_list([]).
	valid_atom_list([Atom| Atoms]) :-
		atom(Atom),
		valid_atom_list(Atoms).

	text_atom(Value, Atom) :-
		atom(Value),
		!,
		Atom = Value.
	text_atom([Head| Tail], Atom) :-
		integer(Head),
		!,
		atom_codes(Atom, [Head| Tail]).
	text_atom([Head| Tail], Atom) :-
		atom(Head),
		atom_chars(Atom, [Head| Tail]).

	lowercase_ascii_atom(Atom0, Atom) :-
		atom_codes(Atom0, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes(Atom, Codes).

	lowercase_ascii_codes([], []).
	lowercase_ascii_codes([Code0| Codes0], [Code| Codes]) :-
		lowercase_ascii_code(Code0, Code),
		lowercase_ascii_codes(Codes0, Codes).

	lowercase_ascii_code(Code0, Code) :-
		Code0 >= 0'A,
		Code0 =< 0'Z,
		!,
		Code is Code0 + 32.
	lowercase_ascii_code(Code, Code).

:- end_object.
