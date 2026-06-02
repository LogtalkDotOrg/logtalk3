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


% This handler keeps the example small while still demonstrating the main
% moving parts of the new library: ordinary HTML replies, page-versus-fragment
% replies, optional router middleware that derives request properties and adds
% HTMX response headers, and visible browser-side behavior driven by HTMX
% attributes plus server-triggered events.

:- object(htmx_panel_http_handler,
	implements(http_handler_protocol),
	imports([http_router, http_router_htmx])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP handler for the HTMX panel example.'
	]).

	:- protected(home/2).
	:- mode(home(+compound, -compound), one_or_error).
	:- info(home/2, [
		comment is 'Route handler that returns the ordinary example home page.',
		argnames is ['Request', 'Response']
	]).

	:- protected(panel/2).
	:- mode(panel(+compound, -compound), one_or_error).
	:- info(panel/2, [
		comment is 'Route handler that returns a full page for ordinary or boosted navigation and a fragment for non-boosted HTMX requests.',
		argnames is ['Request', 'Response']
	]).

	:- protected(panel_overview/2).
	:- mode(panel_overview(+compound, -compound), one_or_error).
	:- info(panel_overview/2, [
		comment is 'Route handler for the overview panel variant.',
		argnames is ['Request', 'Response']
	]).

	:- protected(panel_metrics/2).
	:- mode(panel_metrics(+compound, -compound), one_or_error).
	:- info(panel_metrics/2, [
		comment is 'Route handler for the metrics panel variant.',
		argnames is ['Request', 'Response']
	]).

	:- protected(panel_alert/2).
	:- mode(panel_alert(+compound, -compound), one_or_error).
	:- info(panel_alert/2, [
		comment is 'Route handler for the alert panel variant.',
		argnames is ['Request', 'Response']
	]).

	:- protected(activity_overview/2).
	:- mode(activity_overview(+compound, -compound), one_or_error).
	:- info(activity_overview/2, [
		comment is 'Route handler that returns the activity rail fragment for the overview panel.',
		argnames is ['Request', 'Response']
	]).

	:- protected(activity_metrics/2).
	:- mode(activity_metrics(+compound, -compound), one_or_error).
	:- info(activity_metrics/2, [
		comment is 'Route handler that returns the activity rail fragment for the metrics panel.',
		argnames is ['Request', 'Response']
	]).

	:- protected(activity_alert/2).
	:- mode(activity_alert(+compound, -compound), one_or_error).
	:- info(activity_alert/2, [
		comment is 'Route handler that returns the activity rail fragment for the alert panel.',
		argnames is ['Request', 'Response']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	middleware(htmx_request, annotate_htmx_request).
	response_middleware(htmx_response_headers, add_htmx_response_headers).

	route(home, get, '/', home).
	route(panel, get, '/panel', panel).
	route(panel_overview, get, '/panel/overview', panel_overview).
	route(panel_metrics, get, '/panel/metrics', panel_metrics).
	route(panel_alert, get, '/panel/alert', panel_alert).
	route(activity_overview, get, '/activity/overview', activity_overview).
	route(activity_metrics, get, '/activity/metrics', activity_metrics).
	route(activity_alert, get, '/activity/alert', activity_alert).

	% The panel routes use the companion middleware to attach trigger headers to
	% HTMX responses so the example can refresh the visible activity rail through
	% event-driven relays instead of inline JavaScript.
	route_metadata(panel, [htmx_response_options([trigger(overview_loaded)])]).
	route_metadata(panel_overview, [htmx_response_options([trigger(overview_loaded)])]).
	route_metadata(panel_metrics, [htmx_response_options([trigger(metrics_loaded)])]).
	route_metadata(panel_alert, [htmx_response_options([trigger(alert_loaded)])]).

	home(Request, Response) :-
		home_page_content(Content),
		http_htmx::reply(Request, Content, Response).

	panel(Request, Response) :-
		panel_variant_response(overview, Request, Response).

	panel_overview(Request, Response) :-
		panel_variant_response(overview, Request, Response).

	panel_metrics(Request, Response) :-
		panel_variant_response(metrics, Request, Response).

	panel_alert(Request, Response) :-
		panel_variant_response(alert, Request, Response).

	activity_overview(Request, Response) :-
		activity_response(overview, Request, Response).

	activity_metrics(Request, Response) :-
		activity_response(metrics, Request, Response).

	activity_alert(Request, Response) :-
		activity_response(alert, Request, Response).

	home_page_content(
		html([lang=en], [
			head([
				meta([charset='utf-8']),
				meta([name=viewport, content='width=device-width, initial-scale=1']),
				title('HTMX panel example'),
				script([
					src='https://unpkg.com/htmx.org@2.0.4',
					integrity='sha384-HGfztofotfshcF7+8n44JQL2oJmowVChPTg48S+jvZoztPfvwD79OC/LTtG6dMp+',
					crossorigin='anonymous'
				], []),
				style(Styles)
			]),
			body([
				main([class='page-shell'], [
					header([class='page-header'], [
						p([class='eyebrow'], 'http_htmx example'),
						h1('HTMX panel studio'),
						p('Explore ordinary navigation, boosted navigation, pushed URLs, fragment swaps, loading indicators, and server-triggered refreshes in one page.')
					]),
					section([class='demo-grid'], [
						article([class='card nav-card'], [
							h2('Full-page navigation'),
							p('Use these links to open the default panel page either as an ordinary full request or as boosted HTMX navigation.'),
							ul([class='link-list'], [
								li(a([href='/panel'], 'Open the default panel page as an ordinary request')),
								li(a([href='/panel', 'hx-boost'='true'], 'Open the default panel page with boosted HTMX navigation'))
							]),
							p('The studio controls in the middle card use HTMX fragment requests instead. They target the workspace, show a loading indicator, and push the selected panel URL into browser history.')
						]),
						StudioCard,
						ActivityCard
					])
				])
			])
		])
	) :-
		page_styles(Styles),
		placeholder_panel(PlaceholderPanel),
		studio_card(PlaceholderPanel, StudioCard),
		waiting_activity(WaitingActivity),
		activity_card(WaitingActivity, ActivityCard).

	panel_page_content(Request, Variant,
		html([lang=en], [
			head([
				meta([charset='utf-8']),
				meta([name=viewport, content='width=device-width, initial-scale=1']),
				title(PageTitle),
				script([
					src='https://unpkg.com/htmx.org@2.0.4',
					integrity='sha384-HGfztofotfshcF7+8n44JQL2oJmowVChPTg48S+jvZoztPfvwD79OC/LTtG6dMp+',
					crossorigin='anonymous'
				], []),
				style(Styles)
			]),
			body([
				main([class='page-shell'], [
					header([class='page-header'], [
						p([class='eyebrow'], 'http_htmx example'),
						h1('HTMX panel studio'),
						p('This full-page wrapper is shared by the panel routes. Each panel URL uses http_htmx::page_fragment_reply/4 to return a full document for ordinary or boosted navigation and a fragment for in-place HTMX updates.')
					]),
					section([class='demo-grid'], [
						article([class='card nav-card'], [
							h2('Route context'),
							p(['This page was served for a ', strong(ModeLabel), '.']),
							p(['Current full-page panel: ', strong(VariantLabel), '.']),
							p('Use the studio controls to swap other panel variants without leaving the page. Each HTMX fragment response emits a server-triggered event that refreshes the activity rail.'),
							p(a([href='/'], 'Back to the landing page'))
						]),
						StudioCard,
						ActivityCard
					])
				])
			])
		])
	) :-
		page_styles(Styles),
		request_mode_label(Request, ModeLabel),
		variant_label(Variant, VariantLabel),
		page_title(Variant, PageTitle),
		panel_card(Variant, ModeLabel, PanelCard),
		initial_activity_content(Variant, ModeLabel, ActivityContent),
		studio_card(PanelCard, StudioCard),
		activity_card(ActivityContent, ActivityCard).

	panel_fragment_content(Request, Variant, FragmentContent) :-
		request_mode_label(Request, ModeLabel),
		panel_card(Variant, ModeLabel, FragmentContent).

	panel_variant_response(Variant, Request, Response) :-
		panel_page_content(Request, Variant, PageContent),
		panel_fragment_content(Request, Variant, FragmentContent),
		http_htmx::page_fragment_reply(Request, PageContent, FragmentContent, Response).

	activity_response(Variant, Request, Response) :-
		activity_update_content(Variant, ActivityContent),
		http_htmx::reply(Request, ActivityContent, Response).

	studio_card(InitialPanelContent,
		article([class='card studio-card'], [
			h2('Live studio'),
			p('Load one of three panel variants into the workspace. The buttons use hx-get, hx-target, hx-indicator, hx-swap, and hx-push-url.'),
			div([class='button-row'], Buttons),
			div([class='workspace-shell'], [
				div([id='panel-loading', class='htmx-indicator loading-pill'], 'Loading panel...'),
				div([id='panel-slot', class='panel-slot'], [InitialPanelContent])
			])
		])
	) :-
		fragment_buttons(Buttons).

	activity_card(InitialActivityContent,
		article([class='card activity-card'], [
			h2('Activity rail'),
			p('These hidden relays listen for HX-Trigger events emitted by the panel routes and refresh this rail from the server.'),
			div([id='activity-slot', class='activity-slot'], [InitialActivityContent]),
			div([class='relay-bank'], Relays)
		])
	) :-
		relay_elements(Relays).

	fragment_buttons([OverviewButton, MetricsButton, AlertButton]) :-
		fragment_button(overview, OverviewButton),
		fragment_button(metrics, MetricsButton),
		fragment_button(alert, AlertButton).

	fragment_button(Variant,
		button([
			class-Class,
			type=button,
			'hx-get'=Path,
			'hx-target'='#panel-slot',
			'hx-indicator'='#panel-loading',
			'hx-swap'='innerHTML transition:true',
			'hx-push-url'='true'
		], Label)
	) :-
		variant_path(Variant, Path),
		button_label(Variant, Label),
		button_class(Variant, Class).

	relay_elements([OverviewRelay, MetricsRelay, AlertRelay]) :-
		relay_element(overview, OverviewRelay),
		relay_element(metrics, MetricsRelay),
		relay_element(alert, AlertRelay).

	relay_element(Variant,
		div([
			'hx-get'=Path,
			'hx-trigger'=Trigger,
			'hx-target'='#activity-slot',
			'hx-swap'='innerHTML'
		], [])
	) :-
		activity_path(Variant, Path),
		variant_event(Variant, Event),
		atom_concat(Event, ' from:body', Trigger).

	placeholder_panel(
		div([class='panel-placeholder'], [
			h2('Fragment workspace'),
			p('Use the studio buttons to load overview, metrics, and alert fragments here.'),
			p('Each fragment swap also triggers a server-side event relay that refreshes the activity rail.')
		])
	).

	waiting_activity(
		div([class='activity-entry activity-waiting'], [
			strong('Waiting for activity'),
			p('Load a panel fragment to refresh this rail from an HX-Trigger response header.')
		])
	).

	initial_activity_content(Variant, ModeLabel,
		div([class='activity-entry activity-current'], [
			strong('Current full-page state'),
			p(['The ', strong(VariantLabel), ' was opened as a ', strong(ModeLabel), '.']),
			p('Switch panels with the studio buttons to watch the rail refresh through hidden HTMX relays.')
		])
	) :-
		variant_label(Variant, VariantLabel).

	activity_update_content(Variant,
		div([class=Class], [
			strong('Activity rail updated'),
			p(['The ', strong(VariantLabel), ' finished loading and refreshed this rail from a server-triggered HTMX event.'])
		])
	) :-
		variant_label(Variant, VariantLabel),
		activity_class(Variant, Class).

	panel_card(overview, ModeLabel,
		div([class='panel-card panel-overview'], [
			div([class='panel-topline'], [
				h2('Overview panel'),
				span([class='mode-pill'], ModeLabel)
			]),
			p('This default card summarizes what the example is covering and gives the fragment area a clean landing state.'),
			div([class='stat-grid'], [
				div([class='stat-tile'], [strong('reply/3'), span('Ordinary HTML responses')]),
				div([class='stat-tile'], [strong('page_fragment_reply/4'), span('Full page or fragment output')]),
				div([class='stat-tile'], [strong('http_router_htmx'), span('Derived request properties and response headers')])
			]),
			p('Open the full route with the navigation links, or keep swapping cards in place with the studio buttons.')
		])
	).

	panel_card(metrics, ModeLabel,
		div([class='panel-card panel-metrics'], [
			div([class='panel-topline'], [
				h2('Metrics panel'),
				span([class='mode-pill'], ModeLabel)
			]),
			p('This variant is more visual: it renders a compact dashboard fragment that is easy to distinguish when swapped in place.'),
			div([class='stat-grid'], [
				div([class='stat-tile'], [strong('3'), span('panel variants')]),
				div([class='stat-tile'], [strong('1'), span('activity rail')]),
				div([class='stat-tile'], [strong('2'), span('navigation modes')])
			]),
			ul([class='metric-list'], [
				li('hx-indicator makes the loading state visible.'),
				li('hx-push-url keeps the chosen panel shareable.'),
				li('HX-Trigger relays refresh the side rail without inline scripting.')
			])
		])
	).

	panel_card(alert, ModeLabel,
		div([class='panel-card panel-alert'], [
			div([class='panel-topline'], [
				h2('Alert panel'),
				span([class='mode-pill'], ModeLabel)
			]),
			p('This variant uses a stronger visual treatment so swaps are obvious even without reading the surrounding text.'),
			ul([class='alert-list'], [
				li('The fragment buttons target only the workspace, not the whole page.'),
				li('The activity rail is refreshed by event relays that listen for HX-Trigger headers.'),
				li('Direct navigation still returns a complete document for ordinary and boosted requests.')
			])
		])
	).

	variant_label(overview, 'Overview panel').
	variant_label(metrics, 'Metrics panel').
	variant_label(alert, 'Alert panel').

	variant_path(overview, '/panel/overview').
	variant_path(metrics, '/panel/metrics').
	variant_path(alert, '/panel/alert').

	activity_path(overview, '/activity/overview').
	activity_path(metrics, '/activity/metrics').
	activity_path(alert, '/activity/alert').

	variant_event(overview, overview_loaded).
	variant_event(metrics, metrics_loaded).
	variant_event(alert, alert_loaded).

	button_label(overview, 'Show overview panel').
	button_label(metrics, 'Show metrics panel').
	button_label(alert, 'Show alert panel').

	button_class(overview, 'variant-button variant-overview').
	button_class(metrics, 'variant-button variant-metrics').
	button_class(alert, 'variant-button variant-alert').

	activity_class(overview, 'activity-entry activity-overview').
	activity_class(metrics, 'activity-entry activity-metrics').
	activity_class(alert, 'activity-entry activity-alert').

	page_title(overview, 'HTMX panel example - overview').
	page_title(metrics, 'HTMX panel example - metrics').
	page_title(alert, 'HTMX panel example - alert').

	page_styles('body {\n\tmargin: 0;\n\tbackground: linear-gradient(180deg, #f7f4ee 0%, #efe6d8 100%);\n\tcolor: #20303a;\n\tfont-family: Georgia, "Iowan Old Style", "Palatino Linotype", serif;\n}\n\n* {\n\tbox-sizing: border-box;\n}\n\na {\n\tcolor: #0f766e;\n}\n\n.page-shell {\n\tmax-width: 1100px;\n\tmargin: 0 auto;\n\tpadding: 32px 20px 48px;\n}\n\n.page-header {\n\tmargin-bottom: 24px;\n}\n\n.page-header h1 {\n\tmargin: 0 0 12px;\n\tfont-size: 2.6rem;\n\tline-height: 1.05;\n}\n\n.page-header p {\n\tmax-width: 760px;\n\tfont-size: 1.05rem;\n\tline-height: 1.6;\n\tmargin: 0;\n}\n\n.eyebrow {\n\tmargin: 0 0 12px;\n\ttext-transform: uppercase;\n\tletter-spacing: 0.18em;\n\tfont-size: 0.75rem;\n\tfont-weight: bold;\n\tcolor: #8a4b08;\n}\n\n.demo-grid {\n\tdisplay: grid;\n\tgrid-template-columns: minmax(220px, 280px) minmax(0, 1fr) minmax(220px, 280px);\n\tgap: 20px;\n\talign-items: start;\n}\n\n.card {\n\tbackground: rgba(255, 250, 242, 0.92);\n\tborder: 1px solid #d9c9b0;\n\tborder-radius: 22px;\n\tpadding: 20px;\n\tbox-shadow: 0 18px 40px rgba(60, 42, 18, 0.08);\n}\n\n.card h2 {\n\tmargin-top: 0;\n\tfont-size: 1.2rem;\n\tmargin-bottom: 12px;\n}\n\n.link-list {\n\tmargin: 0 0 16px;\n\tpadding-left: 20px;\n\tline-height: 1.7;\n}\n\n.button-row {\n\tdisplay: flex;\n\tflex-wrap: wrap;\n\tgap: 10px;\n\tmargin-bottom: 16px;\n}\n\n.variant-button {\n\tborder: 0;\n\tborder-radius: 999px;\n\tpadding: 10px 16px;\n\tfont: inherit;\n\tfont-weight: bold;\n\tcursor: pointer;\n\tcolor: #ffffff;\n\tbackground: #20303a;\n\tbox-shadow: 0 10px 20px rgba(32, 48, 58, 0.16);\n}\n\n.variant-overview {\n\tbackground: #0f766e;\n}\n\n.variant-metrics {\n\tbackground: #0b7285;\n}\n\n.variant-alert {\n\tbackground: #b42318;\n}\n\n.workspace-shell {\n\tbackground: rgba(255, 255, 255, 0.7);\n\tborder: 1px solid #e5d8c2;\n\tborder-radius: 18px;\n\tpadding: 14px;\n\tmin-height: 320px;\n}\n\n.loading-pill {\n\tdisplay: inline-block;\n\tmargin-bottom: 12px;\n\tpadding: 6px 10px;\n\tborder-radius: 999px;\n\tfont-size: 0.9rem;\n\tbackground: #d9f3ef;\n\tcolor: #0f766e;\n}\n\n.panel-slot {\n\tmin-height: 240px;\n}\n\n.panel-placeholder,\n.panel-card,\n.activity-slot {\n\tborder-radius: 18px;\n\tpadding: 18px;\n\tbackground: #ffffff;\n\tborder: 1px solid #eadfcd;\n}\n\n.panel-topline {\n\tdisplay: flex;\n\tjustify-content: space-between;\n\talign-items: center;\n\tgap: 12px;\n\tmargin-bottom: 12px;\n}\n\n.panel-topline h2 {\n\tmargin-bottom: 0;\n\tfont-size: 1.35rem;\n}\n\n.mode-pill {\n\tdisplay: inline-flex;\n\talign-items: center;\n\tpadding: 6px 10px;\n\tborder-radius: 999px;\n\tfont-size: 0.85rem;\n\tbackground: rgba(32, 48, 58, 0.08);\n\tcolor: #20303a;\n}\n\n.panel-overview {\n\tbackground: linear-gradient(135deg, #f9fffd 0%, #e6faf5 100%);\n}\n\n.panel-metrics {\n\tbackground: linear-gradient(135deg, #f1fbff 0%, #dff4fa 100%);\n}\n\n.panel-alert {\n\tbackground: linear-gradient(135deg, #fff4f2 0%, #fde8e6 100%);\n}\n\n.stat-grid {\n\tdisplay: grid;\n\tgrid-template-columns: repeat(3, minmax(0, 1fr));\n\tgap: 12px;\n\tmargin: 16px 0;\n}\n\n.stat-tile {\n\tpadding: 12px;\n\tborder-radius: 14px;\n\tbackground: rgba(255, 255, 255, 0.86);\n\tborder: 1px solid rgba(32, 48, 58, 0.08);\n\ttext-align: center;\n}\n\n.stat-tile strong,\n.activity-entry strong {\n\tdisplay: block;\n\tmargin-bottom: 6px;\n}\n\n.metric-list,\n.alert-list {\n\tmargin: 0;\n\tpadding-left: 20px;\n\tline-height: 1.7;\n}\n\n.activity-slot {\n\tmin-height: 170px;\n\tbackground: linear-gradient(180deg, #fffdf9 0%, #fff7ea 100%);\n}\n\n.activity-overview {\n\tborder-left: 4px solid #0f766e;\n}\n\n.activity-metrics {\n\tborder-left: 4px solid #0b7285;\n}\n\n.activity-alert {\n\tborder-left: 4px solid #b42318;\n}\n\n.relay-bank {\n\tdisplay: none;\n}\n\n@media (max-width: 920px) {\n\t.demo-grid {\n\t\tgrid-template-columns: 1fr;\n\t}\n\n\t.page-header h1 {\n\t\tfont-size: 2.1rem;\n\t}\n\n\t.stat-grid {\n\t\tgrid-template-columns: 1fr;\n\t}\n}\n').

	request_mode_label(Request, Label) :-
		http_htmx::request_kind(Request, Kind),
		request_kind_label(Kind, Label).

	request_kind_label(boosted, 'boosted HTMX request').
	request_kind_label(fragment, 'non-boosted HTMX request').
	request_kind_label(history_restore, 'history restore HTMX request').
	request_kind_label(ordinary, 'ordinary request').

:- end_object.


% This bounded server mirrors the other recent HTTP examples: it accepts a
% known number of client connections and then shuts itself down.

:- object(htmx_panel_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Small local HTTP server used by the HTMX panel example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	:- public(serve_listener/2).
	:- mode(serve_listener(+compound, +integer), one_or_error).
	:- info(serve_listener/2, [
		comment is 'Serves the requested number of client connections on an already opened listener.',
		argnames is ['Listener', 'Count']
	]).

	serve(Port, Count) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			serve_listener(Listener, Count),
			Error,
			(  catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket::close_listener(Listener).

	serve_listener(Listener, Count) :-
		http_socket::serve_listener(Listener, htmx_panel_http_handler, Count, _ClientInfos, [shutdown(close)]).

:- end_object.


% The direct client uses ordinary GET requests plus additional HTMX request
% headers so the example can exercise the same route in ordinary, fragment,
% and boosted-navigation modes through the wire.

:- object(htmx_panel_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP client used by the HTMX panel example.'
	]).

	:- public(fetch_home/2).
	:- mode(fetch_home(+integer, -compound), one_or_error).
	:- info(fetch_home/2, [
		comment is 'Fetches the example home page.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_panel_page/2).
	:- mode(fetch_panel_page(+integer, -compound), one_or_error).
	:- info(fetch_panel_page/2, [
		comment is 'Fetches the panel route as an ordinary page request.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_panel_fragment/2).
	:- mode(fetch_panel_fragment(+integer, -compound), one_or_error).
	:- info(fetch_panel_fragment/2, [
		comment is 'Fetches the panel route as a non-boosted HTMX request by supplying the HX-Request header.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_panel_boosted/2).
	:- mode(fetch_panel_boosted(+integer, -compound), one_or_error).
	:- info(fetch_panel_boosted/2, [
		comment is 'Fetches the panel route as a boosted HTMX request by supplying the HX-Request and HX-Boosted headers.',
		argnames is ['Port', 'Response']
	]).

	:- public(run/2).
	:- mode(run(+integer, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Fetches the home page, the ordinary panel page, the HTMX fragment response, and the boosted panel page from the example server.',
		argnames is ['Port', 'Result']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	fetch_home(Port, Response) :-
		example_url(Port, '/', URL),
		http_client::get(URL, Response, []).

	fetch_panel_page(Port, Response) :-
		example_url(Port, '/panel', URL),
		http_client::get(URL, Response, []).

	fetch_panel_fragment(Port, Response) :-
		example_url(Port, '/panel', URL),
		http_client::get(URL, Response, [headers([hx_request-'true'])]).

	fetch_panel_boosted(Port, Response) :-
		example_url(Port, '/panel', URL),
		http_client::get(URL, Response, [headers([hx_request-'true', hx_boosted-'true'])]).

	run(Port, result(HomeResponse, PanelPageResponse, PanelFragmentResponse, PanelBoostedResponse)) :-
		fetch_home(Port, HomeResponse),
		fetch_panel_page(Port, PanelPageResponse),
		fetch_panel_fragment(Port, PanelFragmentResponse),
		fetch_panel_boosted(Port, PanelBoostedResponse).

	example_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

:- end_object.


% The self-contained demo mirrors the style used by the other recent HTTP
% examples: one worker thread runs the bounded server while the main thread
% runs the direct client workflow.

:- object(http_htmx_panel_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Self-contained demo object for the HTMX panel example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the fetched responses when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(htmx_panel_server::serve_listener(Listener, 4), Tag),
			catch(
				htmx_panel_client::run(Port, Result),
				Error,
				(  cleanup_demo(Listener, Tag),
					throw(Error)
				)
			),
			once(threaded_exit(htmx_panel_server::serve_listener(Listener, 4), Tag)),
			catch(http_socket::close_listener(Listener), _, true).

		cleanup_demo(Listener, Tag) :-
			catch(http_socket::close_listener(Listener), _, true),
			catch(once(threaded_exit(htmx_panel_server::serve_listener(Listener, 4), Tag)), _, true).

		print_result(result(HomeResponse, PanelPageResponse, PanelFragmentResponse, PanelBoostedResponse)) :-
			http_core::status(HomeResponse, HomeStatus),
			http_core::status(PanelPageResponse, PanelPageStatus),
			http_core::status(PanelFragmentResponse, PanelFragmentStatus),
			http_core::status(PanelBoostedResponse, PanelBoostedStatus),
			write('Home response: '),
			write(HomeStatus),
			nl,
			write('Ordinary panel response: '),
			write(PanelPageStatus),
			nl,
			write('HTMX fragment response: '),
			write(PanelFragmentStatus),
			nl,
			write('Boosted panel response: '),
			write(PanelBoostedStatus),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run htmx_panel_server::serve/2 and htmx_panel_client::run/2 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_htmx_panel_demo::run/1)).

	:- endif.

:- end_object.
