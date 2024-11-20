%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(d2_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-11-20,
		comment is 'Predicates for generating graph files in the DOT language (version 2.36.0 or later).'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(term_io, [
		write_to_chars/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(d2, d2_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.d2', File).

	file_header(Stream, Identifier, Options) :-
		^^option(layout(Layout), Options),
		convert_layout(Layout, Direction),
		write_key_value(Stream, direction, Direction),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '": {\n'),
		diagram_label(Options, Label),
		write_key_value(Stream, label, Label),
		write_key_value(Stream, 'label.near', 'outside-bottom-left'),
		nl(Stream).

	convert_layout(top_to_bottom, down).
	convert_layout(bottom_to_top, up).
	convert_layout(left_to_right, right).
	convert_layout(right_to_left, left).

	diagram_label(Options, Label) :-
		^^option(title(Title), Options),
		^^option(description(Description), Options),
		(	Title \== '' ->
			atomic_list_concat([Title, '\\n', Description, '\\n'], Label0)
		;	atomic_list_concat([Description, '\\n'], Label0)
		),
		(	^^option(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atomic_list_concat([Label0, 'Generated on ', Year, '-', PaddedMonth, '-', PaddedDay, ', ', PaddedHours, ':', PaddedMinutes, '\\n'], Label1)
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
	backend(swi,     'SWI-Prolog').
	backend(tau,     'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb,     'XSB').
	backend(xvm,     'XVM').
	backend(yap,     'YAP').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '}\n').

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		graph_style_color(Kind, Style, Color),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '_": {\n'),
		write_key_value(Stream, label, Label),
		write_key_value(Stream, 'label.near', 'bottom-center'),
		write_key_value(Stream, 'style.border-radius', Style),
		write_key_value(Stream, 'style.fill', Color),
		(	member(url(URL), Options) ->
			(	URL \== '' ->
				write_key_value_quoted(Stream, link, URL)
			;	member(tooltip(Tooltip), Options) ->
				write_key_value(Stream, tooltip, Tooltip)
			;	write_key_value(Stream, tooltip, Label)
			)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value(Stream, tooltip, Tooltip)
		;	write_key_value(Stream, tooltip, Label)
		).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}\n\n').

	graph_style_color(rlibrary,    8, lightgray).
	graph_style_color(libraries,   8, lightgray).
	graph_style_color(library,     8, whitesmoke).
	graph_style_color(rdirectory,  8, lightgray).
	graph_style_color(directories, 8, lightgray).
	graph_style_color(directory,   8, whitesmoke).
	graph_style_color(files,       8, whitesmoke).
	graph_style_color(file,        8, snow).
	graph_style_color(external,    8, white).
	graph_style_color(entity,      8, snow).

	node(Stream, _-Identifier, Label, Caption, Contents, Kind, Options) :-
		!,
		node(Stream, Identifier, Label, Caption, Contents, Kind, Options).
	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_shape_dash_color(Kind, Shape, Dash, Color),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '_": {\n'),
		write_key_value(Stream, shape, Shape),
		write_key_value(Stream, 'style.fill', Color),
		write_key_value(Stream, 'style.stroke-dash', Dash),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write_key_value_quoted(Stream, link, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value(Stream, tooltip, Tooltip)
		;	true
		),
		write(Stream, 'label: ""\n'),
		write(Stream, 'text: |md\n'),
		(	member(zoom_url(Diagram), Options) ->
			write(Stream, '[&#128269;]('),
			write(Stream, Diagram),
			write(Stream, ')\n\n')
		;	true
		),
		write(Stream, '## '),
		write(Stream, Label),
		write(Stream, '\n'),
		(	^^option(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '#### '),
			write(Stream, Caption),
			write(Stream, '\n')
		;	true
		),
		(	Contents == [] ->
			true
		;	write_node_lines(Contents, Stream)
		),
		write(Stream, '|\n'),
		write(Stream, '}\n').

	% entities belonging to the file or library being documented
	node_shape_dash_color(prototype,                   rectangle, 0, cornsilk).
	node_shape_dash_color(class,                       rectangle, 0, yellow).
	node_shape_dash_color(instance,                    rectangle, 0, yellow).
	node_shape_dash_color(instance_and_class,          rectangle, 0, yellow).
	node_shape_dash_color(protocol,                    page,      0, aquamarine).
	node_shape_dash_color(category,                    rectangle, 0, lightcyan).
	node_shape_dash_color(module,                      package,   0, plum).
	node_shape_dash_color(file,                        rectangle, 0, paleturquoise).
	node_shape_dash_color(directory,                   package,   0, lightsalmon).
	node_shape_dash_color(library,                     package,   0, lightsalmon).
	% external entities to the file or library being documented
	node_shape_dash_color(external_prototype,          rectangle, 2, beige).
	node_shape_dash_color(external_class,              rectangle, 2, lightgoldenrodyellow).
	node_shape_dash_color(external_instance,           rectangle, 2, lightgoldenrodyellow).
	node_shape_dash_color(external_instance_and_class, rectangle, 2, lightgoldenrodyellow).
	node_shape_dash_color(external_protocol,           page,      2, mediumaquamarine).
	node_shape_dash_color(external_category,           rectangle, 2, cyan).
	node_shape_dash_color(external_module,             package,   2, thistle).
	node_shape_dash_color(external_file,               rectangle, 2, powderblue).
	node_shape_dash_color(external_directory,          package,   2, salmon).
	node_shape_dash_color(external_library,            package,   2, salmon).
	% predicates of the entities being documented
	node_shape_dash_color(directive,                   rectangle, 0, bisque).
	node_shape_dash_color(predicate,                   rectangle, 0, cornsilk).
	node_shape_dash_color(public_predicate,            rectangle, 0, springgreen).
	node_shape_dash_color(protected_predicate,         rectangle, 0, yellow).
	node_shape_dash_color(private_predicate,           rectangle, 0, indianred).
	node_shape_dash_color(local_predicate,             rectangle, 0, cornsilk).
	node_shape_dash_color(multifile_predicate,         rectangle, 0, skyblue).
	node_shape_dash_color(exported_predicate,          rectangle, 0, springgreen).
	% external predicates to the entities being documented
	node_shape_dash_color(external_predicate,          rectangle, 2, beige).

	edge(Stream, Source, Destination, Labels, Kind, Options) :-
		edge_arrow_filled(Kind, ArrowHead, Filled),
		write_vertex(Source, Stream),
		write(Stream, ' -> '),
		write_vertex(Destination, Stream),
		write(Stream, ': "'),
		write_edge_lines(Labels, Stream),
		write(Stream, '" {\n'),
		write_key_value(Stream, 'target-arrowhead.shape', ArrowHead),
		write_key_value(Stream, 'target-arrowhead.style.filled', Filled),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write_key_value_quoted(Stream, link, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value(Stream, tooltip, Tooltip)
		;	true
		),
		write(Stream, '}\n').

	write_vertex(Container-Node, Stream) :-
		!,
		write(Stream, '"'),
		write(Stream, Container),
		write(Stream, '_"."'),
		write(Stream, Node),
		write(Stream, '_"').
	write_vertex(Node, Stream) :-
		write(Stream, '"'),
		write(Stream, Node),
		write(Stream, '_"').

	% entity relations
	edge_arrow_filled(extends_object,         arrow,    true).
	edge_arrow_filled(extends_protocol,       arrow,    true).
	edge_arrow_filled(extends_category,       arrow,    true).
	edge_arrow_filled(instantiates_class,     triangle, true).
	edge_arrow_filled(specializes_class,      triangle, false).
	edge_arrow_filled(implements_protocol,    circle,   true).
	edge_arrow_filled(imports_category,       arrow,    true).
	edge_arrow_filled(complements_object,     arrow,    true).
	% multifile predicates
	edge_arrow_filled(provides_clauses,       circle,   true).
	% cross-referencing predicate calls
	edge_arrow_filled(calls_predicate,        triangle, true).
	edge_arrow_filled(calls_super_predicate,  triangle, true).
	edge_arrow_filled(calls_self_predicate,   triangle, true).
	% dynamic predicate updates
	edge_arrow_filled(updates_predicate,      diamond,  true).
	edge_arrow_filled(updates_this_predicate, diamond,  true).
	edge_arrow_filled(updates_self_predicate, diamond,  true).
	% file relations
	edge_arrow_filled(depends_on_file,        triangle, true).
	edge_arrow_filled(loads_file,             triangle, true).
	edge_arrow_filled(includes_file,          triangle, true).
	% directory relations
	edge_arrow_filled(depends_on_directory,   triangle, true).
	edge_arrow_filled(loads_directory,        triangle, true).
	% library relations
	edge_arrow_filled(depends_on_library,     triangle, true).
	edge_arrow_filled(loads_library,          triangle, true).

	write_key_value_quoted(Stream, Key, Value) :-
		write(Stream, Key),
		write(Stream, ': "'),
		write(Stream, Value),
		write(Stream, '"'),
		nl(Stream).

	write_key_value(Stream, Key, Value) :-
		write(Stream, Key),
		write(Stream, ': '),
		write(Stream, Value),
		nl(Stream).

	write_node_lines([], _).
	write_node_lines([Line| Lines], Stream) :-
		(	atom(Line) ->
			atom_chars(Line, Chars)
		;	write_to_chars(Line, Chars)
		),
		write_escaped_chars(Chars, Stream),
		write(Stream, '  \n'),
		write_node_lines(Lines, Stream).

	write_escaped_chars([], _).
	write_escaped_chars([Char| Chars], Stream) :-
		(	escaped_char(Char) ->
			put_char(Stream, '\\')
		;	true
		),
		put_char(Stream, Char),
		write_escaped_chars(Chars, Stream).

	escaped_char('\\').
	escaped_char('`').
	escaped_char('*').
	escaped_char('_').
	escaped_char('{').
	escaped_char('}').
	escaped_char('[').
	escaped_char(']').
	escaped_char('<').
	escaped_char('>').
	escaped_char('(').
	escaped_char(')').
	escaped_char('#').
	escaped_char('+').
	escaped_char('-').
	escaped_char('.').
	escaped_char('!').
	escaped_char('|').
	escaped_char('~').

	write_edge_lines([], _).
	write_edge_lines([Line| Lines], Stream) :-
		write_edge_lines(Lines, Line, Stream).

	write_edge_lines([], Line, Stream) :-
		write(Stream, Line).
	write_edge_lines([Next| Lines], Line, Stream) :-
		write(Stream, Line),
		write(Stream, '\\n'),
		write_edge_lines(Lines, Next, Stream).

:- end_object.
