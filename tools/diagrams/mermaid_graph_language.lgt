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


:- object(mermaid_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 0:7:0,
		author is 'Paulo Moura',
		date is 2026-11-11,
		comment is 'Predicates for generating graph files using Mermaid. Requires Mermaid 12 or later.'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(term_io, [
		write_term_to_chars/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- private(edge_index_/1).
	:- dynamic(edge_index_/1).
	:- mode(edge_index_(?integer), zero_or_one).
	:- info(edge_index_/1, [
		comment is 'Current edge index counter.',
		argnames is ['Index']
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(mermaid, mermaid_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.html', File).

	file_header(Stream, _Identifier, Options) :-
		retractall(edge_index_(_)),
		asserta(edge_index_(0)),
		% Mermaid creates the tooltip element with an inline max-width of 200px, which
		% truncates long paths; the stylesheet overrides it so that the tooltip box
		% fits the tooltip text
		write(Stream, '<html>\n<head>\n<style>\ndiv.mermaidTooltip { max-width: none !important; white-space: nowrap; }\n</style>\n</head>\n<body>\n'),
		write(Stream, '<script type="module">\nimport mermaid from \'https://cdn.jsdelivr.net/npm/mermaid@12/dist/mermaid.esm.min.mjs\';\nwindow.noop = function() {};\n// keep tooltips, which Mermaid anchors at the node center, inside the window\ndocument.addEventListener(\'mouseover\', function() { var t = document.querySelector(\'.mermaidTooltip\'); if (t) { var m = window.scrollX + document.documentElement.clientWidth - t.offsetWidth - 4, l = parseFloat(t.style.left); if (l > m) { t.style.left = Math.max(window.scrollX, m) + \'px\'; } } });\nmermaid.initialize({ securityLevel: \'loose\', startOnLoad: true, htmlLabels: true });\n</script>\n'),
		write(Stream, '<pre class="mermaid">\n'),
		write(Stream, 'graph '),
		^^option(layout(Layout), Options),
		convert_layout(Layout, RankDir),
		write(Stream, RankDir),
		nl(Stream),
		write(Stream, 'classDef default stroke:#000,font-size:11pt\n'),
		write(Stream, 'classDef filled stroke:#000\n'),
		write(Stream, 'classDef dashed stroke:#000,stroke-dasharray: 5 5\n').

	convert_layout(top_to_bottom, 'TB').
	convert_layout(bottom_to_top, 'BT').
	convert_layout(left_to_right, 'LR').
	convert_layout(right_to_left, 'RL').

	diagram_label(Options, Label) :-
		^^option(title(Title), Options),
		^^option(description(Description), Options),
		(	Title \== '' ->
			atomic_list_concat([Title, ' - ', Description], Label0)
		;	Label0 = Description
		),
		(	^^option(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atomic_list_concat([Label0, '<br/ >Generated on ', Year, '-', PaddedMonth, '-', PaddedDay, ', ', PaddedHours, ':', PaddedMinutes, '<br/ >'], Label1)
		;	Label1 = Label0
		),
		(	^^option(versions(true), Options) ->
			current_logtalk_flag(version_data, logtalk(LogtalkMajor, LogtalkMinor, LogtalkPatch, LogtalkStatus)),
			current_logtalk_flag(prolog_dialect, BackendId),
			backend(BackendId, BackendName),
			current_logtalk_flag(prolog_version, v(BackendMajor, BackendMinor, BackendPatch)),
			atomic_list_concat([
				Label1,
				'Generated with Logtalk ', LogtalkMajor, '.', LogtalkMinor, '.', LogtalkPatch, '-', LogtalkStatus,
				' running on ', BackendName, ' ', BackendMajor, '.', BackendMinor, '.', BackendPatch
			], Label)
		;	Label = Label1
		).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			atom_codes(Atom, [0'0| Codes])
		;	atom_codes(Atom, Codes)
		).

	% Prolog backend identifier table
	backend(b,       'B-Prolog').
	backend(ciao,    'Ciao Prolog').
	backend(cx,      'CxProlog').
	backend(eclipse, 'ECLiPSe').
	backend(gnu,     'GNU Prolog').
	backend(ji,      'JIProlog').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(shumway, 'Shumway Prolog').
	backend(swi,     'SWI-Prolog').
	backend(tau,     'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb,     'XSB').
	backend(xvm,     'XVM').
	backend(yap,     'YAP').

	file_footer(Stream, _Identifier, Options) :-
		write(Stream, '</pre>\n<p>\n'),
		diagram_label(Options, Label),
		write(Stream, Label),
		write(Stream, '</p></body>\n</html>\n').

	graph_header(Stream, Identifier, Label, Kind, _Options) :-
		graph_style_margin_color(Kind, _Style, _Margin, Color),
		write(Stream, 'subgraph '),
		write_safe_identifier(Stream, Identifier),
		write(Stream, ' ["'),
		write_html_escaped(Stream, Label, false),
		write(Stream, '"]\nstyle '),
		write_safe_identifier(Stream, Identifier),
		write(Stream, ' fill:'),
		write(Stream, Color),
		nl(Stream).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, 'end\n\n').

	graph_style_margin_color(rlibrary, rounded, 10, '#808080').
	graph_style_margin_color(libraries, rounded, 10, '#808080').
	graph_style_margin_color(library, rounded, 10, '#f5f5f5').
	graph_style_margin_color(rdirectory, rounded, 10, '#808080').
	graph_style_margin_color(directories, rounded, 10, '#808080').
	graph_style_margin_color(directory, rounded, 10, '#f5f5f5').
	graph_style_margin_color(files, rounded, 10, '#f5f5f5').
	graph_style_margin_color(file, rounded, 10, '#fffafa').
	graph_style_margin_color(external, rounded, 10, '#ffffff').
	graph_style_margin_color(entity, rounded, 10, '#fffafa').

	% nodes use the Mermaid 12 "id@{ shape: Shape, label: Label }" syntax with an
	% HTML label (bold title, optional italic caption, one line per content item);
	% label text is escaped using Mermaid entity codes; Mermaid lays out the diagram
	% using its default ELK layout engine
	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_shape_style_color(Kind, Shape, Style, Color),
		write_escaped_identifier(Stream, Identifier),
		write(Stream, '@{ shape: '),
		write(Stream, Shape),
		write(Stream, ', label: "<b>'),
		write_html_escaped(Stream, Label, false),
		write(Stream, '</b>'),
		(	^^option(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '<br/><i>'),
			write_html_escaped(Stream, Caption, false),
			write(Stream, '</i>')
		;	true
		),
		write_node_lines(Contents, Stream),
		(	member(metrics_overlay(Ce,Ca,I,A,D), Options) ->
			write(Stream, '<br/><i>Ce: '),
			write_html_escaped(Stream, Ce, false),
			write(Stream, ' Ca: '),
			write_html_escaped(Stream, Ca, false),
			write(Stream, '</i><br/><i>I: '),
			write_html_escaped(Stream, I, false),
			write(Stream, ' A: '),
			write_html_escaped(Stream, A, false),
			write(Stream, ' D: '),
			write_html_escaped(Stream, D, false),
			write(Stream, '</i>')
		;	true
		),
		write(Stream, '" }\nclass '),
		write_escaped_identifier(Stream, Identifier),
		write(Stream, ' '),
		write(Stream, Style),
		nl(Stream),
		node_click(Stream, Identifier, Options),
		write(Stream, 'style '),
		write_escaped_identifier(Stream, Identifier),
		write(Stream, ' fill:'),
		write(Stream, Color),
		nl(Stream).

	% clickable nodes: a URL, with an optional tooltip, becomes a link; a tooltip
	% alone must use the callback form as a single quoted string is always read
	% by Mermaid as a link; the "noop" callback is defined in the page header
	node_click(Stream, Identifier, Options) :-
		(	^^option(url(URL), Options),
			URL \== '' ->
			write(Stream, 'click '),
			write_escaped_identifier(Stream, Identifier),
			write(Stream, ' "'),
			write(Stream, URL),
			write(Stream, '"'),
			(	member(tooltip(Tooltip), Options),
				Tooltip \== '' ->
				write(Stream, ' "'),
				write(Stream, Tooltip),
				write(Stream, '"')
			;	true
			),
			nl(Stream)
		;	member(tooltip(Tooltip), Options),
			Tooltip \== '' ->
			write(Stream, 'click '),
			write_escaped_identifier(Stream, Identifier),
			write(Stream, ' noop "'),
			write(Stream, Tooltip),
			write(Stream, '"'),
			nl(Stream)
		;	true
		).

	% entities belonging to the file or library being documented
	node_shape_style_color(prototype, rect, filled, '#fff8dc').
	node_shape_style_color(class, rect, filled, '#ffff00').
	node_shape_style_color(instance, rect, filled, '#ffff00').
	node_shape_style_color(instance_and_class, rect, filled, '#ffff00').
	node_shape_style_color(protocol, 'notch-rect', filled, '#7fffd4').
	node_shape_style_color(category, 'lin-rect', filled, '#00ffff').
	node_shape_style_color(module, 'tag-rect', filled, '#dda0dd').
	node_shape_style_color(file, rect, filled, '#30d5c8').
	node_shape_style_color(directory, 'tag-rect', filled, '#ff8c69').
	node_shape_style_color(library, 'tag-rect', filled, '#ff8c69').
	% external entities to the file or library being documented
	node_shape_style_color(external_prototype, rect, dashed, '#f5f5dc').
	node_shape_style_color(external_class, rect, dashed, '#daa520').
	node_shape_style_color(external_instance, rect, dashed, '#daa520').
	node_shape_style_color(external_instance_and_class, rect, dashed, '#daa520').
	node_shape_style_color(external_protocol, 'notch-rect', dashed, '#7fffd4').
	node_shape_style_color(external_category, 'lin-rect', dashed, '#00ffff').
	node_shape_style_color(external_module, 'tag-rect', dashed, '#d8bfd8').
	node_shape_style_color(external_file, rect, dashed, '#87ceeb').
	node_shape_style_color(external_directory, 'tag-rect', dashed, '#fa8072').
	node_shape_style_color(external_library, 'tag-rect', dashed, '#fa8072').
	% predicates of the entities being documented
	node_shape_style_color(directive, rect, filled, '#ffe4c4').
	node_shape_style_color(predicate, rect, filled, '#fff8dc').
	node_shape_style_color(public_predicate, rect, filled, '#00ff00').
	node_shape_style_color(protected_predicate, rect, filled, '#ffff00').
	node_shape_style_color(private_predicate, rect, filled, '#ff0000').
	node_shape_style_color(local_predicate, rect, filled, '#fff8dc').
	node_shape_style_color(multifile_predicate, rect, filled, '#87ceeb').
	node_shape_style_color(exported_predicate, rect, filled, '#00ff00').
	% external predicates to the entities being documented
	node_shape_style_color(external_predicate, rect, dashed, '#f5f5dc').

	edge(Stream, Start, End, Labels, Kind, Options) :-
		retract(edge_index_(N)),
		N1 is N + 1,
		asserta(edge_index_(N1)),
		write_vertex(Start, Stream),
		write(Stream, ' '),
		edge_arrow(Kind, ArrowHead),
		write(Stream, ArrowHead),
		write(Stream, ' |'),
		write_edge_lines(Labels, Stream),
		write(Stream, '| '),
		write_vertex(End, Stream),
		nl(Stream),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write(Stream, 'click '),
			write_vertex(Start, Stream),
			write(Stream, '-->'),
			write_vertex(End, Stream),
			write(Stream, ' "'),
			write(Stream, URL),
			write(Stream, '"'),
			(	member(tooltip(Tooltip), Options) ->
				write(Stream, ' "'),
				write(Stream, Tooltip),
				write(Stream, '"')
			;	true
			),
			nl(Stream)
		;	true
		),
		(	member(color(Color), Options) ->
			write(Stream, 'linkStyle '),
			write(Stream, N),
			write(Stream, ' stroke:'),
			write(Stream, Color),
			write(Stream, ',stroke-width:2px'),
			nl(Stream)
		;	true
		).

	write_vertex(_-Node, Stream) :-
		!,
		write_escaped_identifier(Stream, Node).
	write_vertex(Node, Stream) :-
		write_escaped_identifier(Stream, Node).

	% entity relations
	edge_arrow(extends_object, '-->').
	edge_arrow(extends_protocol, '-->').
	edge_arrow(extends_category, '-->').
	edge_arrow(instantiates_class, '-->').
	edge_arrow(specializes_class, '-->').
	edge_arrow(implements_protocol, '--o').
	edge_arrow(imports_category, '--x').
	edge_arrow(complements_object, '--x').
	% multifile predicates
	edge_arrow(provides_clauses, '-->').
	% cross-referencing predicate calls
	edge_arrow(calls_predicate, '-->').
	edge_arrow(calls_super_predicate, '-->').
	edge_arrow(calls_self_predicate, '-->').
	% dynamic predicate updates
	edge_arrow(updates_predicate, '--o').
	edge_arrow(updates_this_predicate, '--o').
	edge_arrow(updates_self_predicate, '--o').
	% file relations
	edge_arrow(depends_on_file, '-->').
	edge_arrow(loads_file, '-->').
	edge_arrow(includes_file, '-->').
	% directory relations
	edge_arrow(depends_on_directory, '-->').
	edge_arrow(loads_directory, '-->').
	% library relations
	edge_arrow(depends_on_library, '-->').
	edge_arrow(loads_library, '-->').

	% node and edge identifiers are written between single quotes; subgraph
	% identifiers are written as is; in both cases, only characters that are safe
	% in a Mermaid identifier are kept (any other character, e.g. ^ ~ | = < > @ ; [
	% ] { } or a space, would be a syntax error or would silently split the
	% identifier) and all other characters are replaced by their character code
	% between underscores, which keeps distinct identifiers distinct
	write_escaped_identifier(Stream, Identifier) :-
		put_char(Stream, '\''),
		write_safe_identifier(Stream, Identifier),
		put_char(Stream, '\'').

	write_safe_identifier(Stream, Identifier) :-
		write_term_to_chars(Identifier, Chars, [quoted(false)]),
		write_safe_chars(Chars, Stream).

	write_safe_chars([], _).
	write_safe_chars([Char| Chars], Stream) :-
		char_code(Char, Code),
		(	safe_identifier_code(Code) ->
			put_char(Stream, Char)
		;	write(Stream, '_'),
			write(Stream, Code),
			write(Stream, '_')
		),
		write_safe_chars(Chars, Stream).

	% letters, digits, underscore, slash, and period
	safe_identifier_code(Code) :-
		Code >= 0'a, Code =< 0'z, !.
	safe_identifier_code(Code) :-
		Code >= 0'A, Code =< 0'Z, !.
	safe_identifier_code(Code) :-
		Code >= 0'0, Code =< 0'9, !.
	safe_identifier_code(0'_).
	safe_identifier_code(0'/).
	safe_identifier_code(0'.).

	write_node_lines([], _).
	write_node_lines([Line| Lines], Stream) :-
		write(Stream, '<br/>'),
		write_html_escaped(Stream, Line, true),
		write_node_lines(Lines, Stream).

	% writes a term as text safe for use inside a Mermaid double-quoted HTML label
	% (or URL/tooltip string) by using Mermaid entity codes for the characters
	% that would otherwise be interpreted as markup, as string delimiters, or as
	% escape sequences (the label is a double-quoted string in the @{ ... } syntax)
	write_html_escaped(Stream, Term, Quoted) :-
		write_term_to_chars(Term, Chars, [quoted(Quoted)]),
		write_html_escaped_chars(Chars, Stream).

	write_html_escaped_chars([], _).
	write_html_escaped_chars([Char| Chars], Stream) :-
		(	html_entity(Char, Entity) ->
			write(Stream, Entity)
		;	put_char(Stream, Char)
		),
		write_html_escaped_chars(Chars, Stream).

	html_entity('#',  '#35;').
	html_entity('<',  '#lt;').
	html_entity('>',  '#gt;').
	html_entity('&',  '#amp;').
	html_entity('"',  '#quot;').
	html_entity('\\',  '#92;').

	write_edge_lines([], _).
	write_edge_lines([Line| Lines], Stream) :-
		write_edge_lines(Lines, Line, Stream).

	write_edge_lines([], Line, Stream) :-
		write(Stream, Line).
	write_edge_lines([Next| Lines], Line, Stream) :-
		write(Stream, Line),
		nl(Stream),
		write_edge_lines(Lines, Next, Stream).

:- end_object.
