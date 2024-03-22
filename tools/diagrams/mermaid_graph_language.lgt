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


:- object(mermaid_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2024-08-22,
		comment is 'Predicates for generating graph files in the mermaid language (version 2.36.0 or later).'
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
	graph_language_registry::language_object(mermaid, mermaid_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.html', File).

	file_header(Stream, _Identifier, Options) :-
		write(Stream, '<html>\n<body>\n'),
		write(Stream, '<script type="module">\nimport mermaid from \'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs\';\nmermaid.initialize({ startOnLoad: true });\n</script>\n'),
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
			atomic_list_concat([Label0, 'Generated on ', Year, '-', PaddedMonth, '-', PaddedDay, ', ', PaddedHours, ':', PaddedMinutes, '\\l'], Label1)
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
	backend(lvm,     'LVM').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(swi,     'SWI-Prolog').
	backend(tau,     'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb,     'XSB').
	backend(yap,     'YAP').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '</pre>\n</body>\n</html>\n').

	graph_header(Stream, Identifier, Label, Kind, _Options) :-
		graph_style_margin_color(Kind, _Style, _Margin, Color),
		write(Stream, 'subgraph '),
		write(Stream, Identifier),
		write(Stream, ' ["'),
		write(Stream, Label),
		write(Stream, '"]\nstyle '),
		write(Stream, Identifier),
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

	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_shape_style_color(Kind, _Shape, Style, Color),
		write(Stream, Identifier),
		write(Stream, '["`\n**'),
		write(Stream, Label),
		write(Stream, '**\n'),
		(	^^option(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '_'),
			write(Stream, Caption),
			write(Stream, '_'),
			nl(Stream)
		;	true
		),
		write_node_lines(Contents, Stream),
		write(Stream, '`"]:::'),
		write(Stream, Style),
		nl(Stream),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write(Stream, 'click '),
			write(Stream, Identifier),
			write(Stream, ' "'),
			write(Stream, URL),
			write(Stream, '"')
		;	member(tooltip(Tooltip), Options) ->
			write(Stream, 'click '),
			write(Stream, Identifier),
			write(Stream, ' "'),
			write(Stream, Tooltip),
			write(Stream, '"')
		;	true
		),
		write(Stream, '\nstyle '),
		write(Stream, Identifier),
		write(Stream, ' fill:'),
		write(Stream, Color),
		write(Stream, '\n').

	% entities belonging to the file or library being documented
	node_shape_style_color(prototype, box, filled, '#fff8dc').
	node_shape_style_color(class, box, filled, '#ffff00').
	node_shape_style_color(instance, box, filled, '#ffff00').
	node_shape_style_color(instance_and_class, box, filled, '#ffff00').
	node_shape_style_color(protocol, note, filled, '#7fffd4').
	node_shape_style_color(category, component, filled, '#00ffff').
	node_shape_style_color(module, tab, filled, '#dda0dd').
	node_shape_style_color(file, box, filled, '#30d5c8').
	node_shape_style_color(directory, tab, filled, '#ff8c69').
	node_shape_style_color(library, tab, filled, '#ff8c69').
	% external entities to the file or library being documented
	node_shape_style_color(external_prototype, box, dashed, '#f5f5dc').
	node_shape_style_color(external_class, box, dashed, '#daa520').
	node_shape_style_color(external_instance, box, dashed, '#daa520').
	node_shape_style_color(external_instance_and_class, box, dashed, '#daa520').
	node_shape_style_color(external_protocol, note, dashed, '#7fffd4').
	node_shape_style_color(external_category, component, dashed, '#00ffff').
	node_shape_style_color(external_module, tab, dashed, '#d8bfd8').
	node_shape_style_color(external_file, box, dashed, '#0000ff').
	node_shape_style_color(external_directory, tab, dashed, '#fa8072').
	node_shape_style_color(external_library, tab, dashed, '#fa8072').
	% predicates of the entities being documented
	node_shape_style_color(directive, box, filled, '#ffe4c4').
	node_shape_style_color(predicate, box, filled, '#fff8dc').
	node_shape_style_color(public_predicate, box, filled, '#00ff00').
	node_shape_style_color(protected_predicate, box, filled, '#ffff00').
	node_shape_style_color(private_predicate, box, filled, '#ff0000').
	node_shape_style_color(local_predicate, box, filled, '#fff8dc').
	node_shape_style_color(multifile_predicate, box, filled, '#0000ff').
	node_shape_style_color(exported_predicate, box, filled, '#00ff00').
	% external predicates to the entities being documented
	node_shape_style_color(external_predicate, box, dashed, '#f5f5dc').

	edge(Stream, Start, End, Labels, Kind, _Options) :-
		write(Stream, Start),
		write(Stream, ' '),
		edge_arrow(Kind, ArrowHead),
		write(Stream, ArrowHead),
		write(Stream, ' |'),
		write_edge_lines(Labels, Stream),
		write(Stream, '| '),
		write(Stream, End),
		nl(Stream).

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

	write_node_lines([], _).
	write_node_lines([Line| Lines], Stream) :-
		write(Stream, Line),
		nl(Stream),
		write_node_lines(Lines, Stream).

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
