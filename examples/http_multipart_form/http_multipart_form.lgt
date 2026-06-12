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


:- object(multipart_form_http_handler,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP handler for the multipart form example.'
	]).

	:- protected(home/2).
	:- mode(home(+compound, -compound), one_or_error).
	:- info(home/2, [
		comment is 'Returns the example form page.',
		argnames is ['Request', 'Response']
	]).

	:- protected(submit/2).
	:- mode(submit(+compound, -compound), one_or_error).
	:- info(submit/2, [
		comment is 'Accepts the multipart form submission and returns a confirmation page when both fields are present.',
		argnames is ['Request', 'Response']
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(term_io, [
		with_output_to/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	route(home, get, '/', home).
	route(submit, post, '/submit', submit).

	home(Request, Response) :-
		form_page_content(Content),
		html_response(Request, status(200, 'OK'), Content, Response).

	submit(Request, Response) :-
		( 	submitted_form_data(Request, Name, Email) ->
			confirmation_page_content(Name, Email, Content),
			html_response(Request, status(200, 'OK'), Content, Response)
		; 	invalid_submission_page_content(Content),
			html_response(Request, status(400, 'Bad Request'), Content, Response)
		).

	submitted_form_data(Request, Name, Email) :-
		http_core::body(Request, Body),
		catch(http_multipart::fields(Body, Fields), _, fail),
		extract_field_value(name, Fields, Name),
		extract_field_value(email, Fields, Email).

	extract_field_value(FieldName, Fields, Value) :-
		memberchk(field(FieldName, Value, _Parameters), Fields).

	html_response(Request, Status, Content, Response) :-
		render_html(Content, HTML),
		http_core::version(Request, Version),
		http_core::response(Version, Status, [], content('text/html', text(HTML)), [], Response).

	render_html(Content, HTML) :-
		with_output_to(atom(HTML), (
			current_output(Stream),
			html5::generate(stream(Stream), Content)
		)).

	form_page_content(
		html([lang=en], [
			head([
				meta([charset='utf-8']),
				meta([name=viewport, content='width=device-width, initial-scale=1']),
				title('Multipart form example'),
				style(Styles)
			]),
			body([
				main([class='page-shell'], [
					section([class='card hero-card'], [
						p([class='eyebrow'], 'http_multipart example'),
						h1('Simple multipart form handling'),
						p('This example serves a plain HTML form that posts multipart/form-data to the server. The handler reads the normalized request body and extracts the submitted name and email fields using http_multipart::fields/2.')
					]),
					section([class='card form-card'], [
						h2('Contact form'),
						p('Enter your name and email address, then submit the form to send the multipart fields to the server.'),
						form([method=post, action='/submit', enctype='multipart/form-data', class='contact-form'], [
							label([for=name], 'Name'),
							input([type=text, id=name, name=name, required=required, placeholder='Ada Lovelace']),
							label([for=email], 'Email'),
							input([type=email, id=email, name=email, required=required, placeholder='ada@example.com']),
							button([type=submit], 'Send form data')
						]),
						p([class='hint'], 'The form uses enctype="multipart/form-data" so the server can inspect the normalized multipart request body directly.')
					]),
					EnvironmentCard
				])
			])
		])
	) :-
		page_styles(Styles),
		runtime_environment_card(EnvironmentCard).

	confirmation_page_content(Name, Email,
		html([lang=en], [
			head([
				meta([charset='utf-8']),
				meta([name=viewport, content='width=device-width, initial-scale=1']),
				title('Multipart form example - submitted'),
				style(Styles)
			]),
			body([
				main([class='page-shell'], [
					section([class='card success-card'], [
						p([class='eyebrow'], 'Submission received'),
						h1('Multipart form data accepted'),
						p('The server extracted the submitted fields from the normalized multipart request body.'),
						dl([class='summary-list'], [
							dt('Name'),
							dd(Name),
							dt('Email'),
							dd(Email)
						]),
						p(a([href='/'], 'Submit another form'))
					]),
					EnvironmentCard
				])
			])
		])
	) :-
		page_styles(Styles),
		runtime_environment_card(EnvironmentCard).

	invalid_submission_page_content(
		html([lang=en], [
			head([
				meta([charset='utf-8']),
				meta([name=viewport, content='width=device-width, initial-scale=1']),
				title('Multipart form example - invalid submission'),
				style(Styles)
			]),
			body([
				main([class='page-shell'], [
					section([class='card error-card'], [
						p([class='eyebrow'], 'Bad request'),
						h1('Missing multipart form fields'),
						p('The server expected a multipart/form-data request containing both the name and email fields.'),
						p(a([href='/'], 'Return to the form'))
					]),
					EnvironmentCard
				])
			])
		])
	) :-
		page_styles(Styles),
		runtime_environment_card(EnvironmentCard).

	runtime_environment_card(
		section([class='card environment-card'], [
			h2('Runtime environment'),
			dl([class='summary-list'], [
				dt('Logtalk'),
				dd(LogtalkVersion),
				dt(BackendName),
				dd(BackendVersion)
			])
		])
	) :-
		runtime_environment(LogtalkVersion, BackendName, BackendVersion).

	runtime_environment(LogtalkVersion, BackendName, BackendVersion) :-
		logtalk_version(LogtalkVersion),
		current_logtalk_flag(prolog_dialect, Backend),
		backend(Backend, BackendName),
		current_logtalk_flag(prolog_version, v(Major, Minor, Patch)),
		atomic_list_concat([Major, Minor, Patch], '.', BackendVersion).

	logtalk_version(Version) :-
		current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status)),
		atomic_list_concat([Major, Minor, Patch], '.', Prefix),
		( 	Status == stable ->
			Version = Prefix
		; 	atomic_list_concat([Prefix, Status], '-', Version)
		).

	backend(logtalk, 'Logtalk').
	backend(b, 'B-Prolog').
	backend(ciao, 'Ciao Prolog').
	backend(cx, 'CxProlog').
	backend(eclipse, 'ECLiPSe').
	backend(gnu, 'GNU Prolog').
	backend(ji, 'JIProlog').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(swi, 'SWI-Prolog').
	backend(tau, 'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb, 'XSB').
	backend(xvm, 'XVM').
	backend(yap, 'YAP').

	page_styles('body {\n\tmargin: 0;\n\tbackground: linear-gradient(180deg, #f4efe8 0%, #e5ddd1 100%);\n\tcolor: #1d2935;\n\tfont-family: Georgia, "Iowan Old Style", "Palatino Linotype", serif;\n}\n\n* {\n\tbox-sizing: border-box;\n}\n\n.page-shell {\n\tmax-width: 760px;\n\tmargin: 0 auto;\n\tpadding: 40px 20px 56px;\n}\n\n.card {\n\tbackground: rgba(255, 251, 246, 0.94);\n\tborder: 1px solid #d7c8b2;\n\tborder-radius: 24px;\n\tpadding: 26px;\n\tbox-shadow: 0 20px 45px rgba(52, 39, 24, 0.09);\n}\n\n.hero-card,\n.form-card,\n.success-card,\n.error-card,\n.environment-card {\n\tmargin-bottom: 18px;\n}\n\n.eyebrow {\n\tmargin: 0 0 12px;\n\ttext-transform: uppercase;\n\tletter-spacing: 0.18em;\n\tfont-size: 0.75rem;\n\tfont-weight: bold;\n\tcolor: #8a4b08;\n}\n\n.card h1,\n.card h2 {\n\tmargin-top: 0;\n}\n\n.card p {\n\tline-height: 1.65;\n}\n\n.contact-form {\n\tdisplay: grid;\n\tgap: 12px;\n\tmargin-top: 18px;\n}\n\nlabel {\n\tfont-weight: bold;\n}\n\ninput {\n\twidth: 100%;\n\tpadding: 12px 14px;\n\tborder: 1px solid #c8b79d;\n\tborder-radius: 14px;\n\tfont: inherit;\n\tbackground: #fffdf8;\n}\n\nbutton {\n\tmargin-top: 6px;\n\tborder: 0;\n\tborder-radius: 999px;\n\tpadding: 12px 18px;\n\tfont: inherit;\n\tfont-weight: bold;\n\tcursor: pointer;\n\tcolor: #ffffff;\n\tbackground: #0f766e;\n\tbox-shadow: 0 12px 24px rgba(15, 118, 110, 0.18);\n}\n\n.hint {\n\tmargin-bottom: 0;\n\tfont-size: 0.95rem;\n\tcolor: #5d4c3c;\n}\n\n.summary-list {\n\tdisplay: grid;\n\tgrid-template-columns: max-content 1fr;\n\tgap: 10px 18px;\n\tmargin: 20px 0;\n}\n\n.summary-list dt {\n\tfont-weight: bold;\n}\n\n.summary-list dd {\n\tmargin: 0;\n}\n\n.error-card {\n\tborder-color: #d5a6a3;\n}\n\n@media (max-width: 640px) {\n\t.page-shell {\n\t\tpadding: 28px 16px 40px;\n\t}\n\n\t.card {\n\t\tpadding: 20px;\n\t}\n}\n').

:- end_object.


:- object(multipart_form_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Small local HTTP server used by the multipart form example.'
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
			( 	catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket::close_listener(Listener).

	serve_listener(Listener, Count) :-
		http_socket::serve_listener(Listener, multipart_form_http_handler, Count, _ClientInfos, [shutdown(close)]).

:- end_object.


:- object(multipart_form_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP client used by the multipart form example.'
	]).

	:- public(fetch_form/2).
	:- mode(fetch_form(+integer, -compound), one_or_error).
	:- info(fetch_form/2, [
		comment is 'Fetches the form page.',
		argnames is ['Port', 'Response']
	]).

	:- public(submit_form/4).
	:- mode(submit_form(+integer, +atom, +atom, -compound), one_or_error).
	:- info(submit_form/4, [
		comment is 'Submits the multipart form using the supplied name and email values.',
		argnames is ['Port', 'Name', 'Email', 'Response']
	]).

	:- public(run/4).
	:- mode(run(+integer, +atom, +atom, -compound), one_or_error).
	:- info(run/4, [
		comment is 'Fetches the form page and then submits the multipart form.',
		argnames is ['Port', 'Name', 'Email', 'Result']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	fetch_form(Port, Response) :-
		example_url(Port, '/', URL),
		http_client::get(URL, Response, []).

	submit_form(Port, Name, Email, Response) :-
		example_url(Port, '/submit', URL),
		http_client::post(URL, form_data([field(name, Name, []), field(email, Email, [])]), Response, []).

	run(Port, Name, Email, result(FormResponse, SubmitResponse)) :-
		fetch_form(Port, FormResponse),
		submit_form(Port, Name, Email, SubmitResponse).

	example_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

:- end_object.


:- object(http_multipart_form_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Self-contained demo object for the multipart form example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the fetched form page and submitted form response when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(multipart_form_server::serve_listener(Listener, 2), Tag),
			catch(
				multipart_form_client::run(Port, 'Ada Lovelace', 'ada@example.com', Result),
				Error,
				( 	cleanup_demo(Listener, Tag),
					throw(Error)
				)
			),
			http_socket::request_listener_shutdown(Listener),
			threaded_exit(multipart_form_server::serve_listener(Listener, 2), Tag),
			catch(http_socket::close_listener(Listener), _, true).

		cleanup_demo(Listener, Tag) :-
			http_socket::request_listener_shutdown(Listener),
			catch(threaded_exit(multipart_form_server::serve_listener(Listener, 2), Tag), _, true),
			catch(http_socket::close_listener(Listener), _, true).

		print_result(result(FormResponse, SubmitResponse)) :-
			http_core::status(FormResponse, FormStatus),
			http_core::status(SubmitResponse, SubmitStatus),
			write('Form page response: '),
			write(FormStatus),
			nl,
			write('Submission response: '),
			write(SubmitStatus),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run multipart_form_server::serve/2 and multipart_form_client::run/4 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_multipart_form_demo::run/1)).

	:- endif.

:- end_object.
