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


:- object(dot_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 3:10:3,
		author is 'Paulo Moura',
		date is 2024-12-04,
		comment is 'Predicates for generating graph files in the DOT language (version 2.36.0 or later).'
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

	:- multifile(graph_language_registry::language_object/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(dot, dot_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.dot', File).

	file_header(Stream, Identifier, Options) :-
		write(Stream, 'digraph "'),
		write(Stream, Identifier),
		write(Stream, '" {\n'),
		^^option(layout(Layout), Options),
		convert_layout(Layout, RankDir),
		write_key_value_nl(Stream, rankdir, RankDir),
		write_key_value_nl(Stream, ranksep, '1.0'),
		write_key_value_nl(Stream, compound, true),
		write_key_value_nl(Stream, splines, true),
		write_key_value_nl(Stream, clusterrank, local),
		write_key_value_nl(Stream, labeljust, l),
		write_key_value_nl(Stream, margin, '1.0'),
		write_key_value_nl(Stream, fontname, 'Monospace'),
		write_key_value_nl(Stream, fontsize, 10),
		write_key_value_nl(Stream, fontcolor, dimgray),
		write_key_value_nl(Stream, pencolor, dimgray),
		write_key_value_nl(Stream, stylesheet, 'diagrams.css'),
		write(Stream, 'node [shape="ellipse",style="filled",fillcolor="white",fontname="Monospace",fontsize="9"]\n'),
		write(Stream, 'edge [fontname="Monospace",fontsize="9"]\n'),
		diagram_label(Options, Label),
		write_key_value_nl(Stream, label, Label),
		nl(Stream).

	convert_layout(top_to_bottom, 'TB').
	convert_layout(bottom_to_top, 'BT').
	convert_layout(left_to_right, 'LR').
	convert_layout(right_to_left, 'RL').

	diagram_label(Options, Label) :-
		^^option(title(Title), Options),
		^^option(description(Description), Options),
		(	Title \== '' ->
			atomic_list_concat([Title, '\\l', Description, '\\l'], Label0)
		;	atomic_list_concat([Description, '\\l'], Label0)
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
	backend(xvm,     'XVM').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(swi,     'SWI-Prolog').
	backend(tau,     'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb,     'XSB').
	backend(yap,     'YAP').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '}\n').

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		graph_style_margin_color(Kind, Style, Margin, Color),
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Identifier),
		write(Stream, '" {\n'),
		write_key_value_nl(Stream, bgcolor, Color),
		write_key_value_nl(Stream, style, Style),
		write_key_value_nl(Stream, margin, Margin),
		(	member(url(URL), Options) ->
			(	URL \== '' ->
				write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
				write(Stream, URL),
				write(Stream, '" href="'),
				write(Stream, URL),
				write(Stream, '">'),
				write(Stream, Label),
				write(Stream, '</TD></TR></TABLE>>\n'),
				write_key_value_nl(Stream, tooltip, Label)
			;	member(tooltip(Tooltip), Options) ->
				write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
				write(Stream, Tooltip),
				write(Stream, '">'),
				write(Stream, Label),
				write(Stream, '</TD></TR></TABLE>>\n'),
				write_key_value_nl(Stream, tooltip, Tooltip)
			;	write_key_value_nl(Stream, tooltip, Label)
			)
		;	member(tooltip(Tooltip), Options) ->
			write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
			write(Stream, Tooltip),
			write(Stream, '">'),
			write(Stream, Label),
			write(Stream, '</TD></TR></TABLE>>\n'),
			write_key_value_nl(Stream, tooltip, Tooltip)
		;	write_key_value_nl(Stream, tooltip, Label)
		).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}\n\n').

	graph_style_margin_color(rlibrary,    rounded, 10, lightgray).
	graph_style_margin_color(libraries,   rounded, 10, lightgray).
	graph_style_margin_color(library,     rounded, 10, whitesmoke).
	graph_style_margin_color(rdirectory,  rounded, 10, lightgray).
	graph_style_margin_color(directories, rounded, 10, lightgray).
	graph_style_margin_color(directory,   rounded, 10, whitesmoke).
	graph_style_margin_color(files,       rounded, 10, whitesmoke).
	graph_style_margin_color(file,        rounded, 10, snow).
	graph_style_margin_color(external,    rounded, 10, white).
	graph_style_margin_color(entity,      rounded, 10, snow).

	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_shape_style_color_quoted(Kind, Shape, Style, Color, Quoted),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '" ['),
		write_key_value_comma(Stream, shape, Shape),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write_key_value_comma(Stream, 'URL', URL),
			write_key_value_comma(Stream, tooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, tooltip, Tooltip)
		;	true
		),
		write_key_value_comma(Stream, style, Style),
		write_key_value_comma(Stream, fillcolor, Color),
		write(Stream, 'label=<<TABLE border="0" cellborder="0" cellspacing="0" cellpadding="0">'),
		(	member(zoom_url(Diagram), Options) ->
			write(Stream, '<TR><TD width="11" height="11" fixedsize="true" align="left" tooltip="Zoom" href="'),
			write(Stream, Diagram),
			write(Stream, '">&#128269;</TD></TR>')
		;	true
		),
		write(Stream, '<TR><TD> </TD><TD><FONT POINT-SIZE="11">'),
		write_escaped_term(Stream, Label, [quoted(Quoted)]),
		write(Stream, '</FONT></TD><TD> </TD></TR>'),
		(	^^option(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '<TR><TD> </TD><TD><FONT POINT-SIZE="7">'),
			write_escaped_term(Stream, Caption, []),
			write(Stream, '</FONT></TD><TD> </TD></TR>')
		;	true
		),
		(	Contents == [] ->
			true
		;	write(Stream, '<TR><TD> </TD></TR>'),
			write_node_lines(Contents, Stream, [quoted(Quoted)])
		),
		write(Stream, '</TABLE>>]\n').

	% entities belonging to the file or library being documented
	node_shape_style_color_quoted(prototype,                   box,       filled,          cornsilk,             false).
	node_shape_style_color_quoted(class,                       box,       filled,          yellow,               false).
	node_shape_style_color_quoted(instance,                    box,       filled,          yellow,               false).
	node_shape_style_color_quoted(instance_and_class,          box,       filled,          yellow,               false).
	node_shape_style_color_quoted(protocol,                    note,      filled,          aquamarine,           false).
	node_shape_style_color_quoted(category,                    component, filled,          lightcyan,            false).
	node_shape_style_color_quoted(module,                      tab,       filled,          plum,                 false).
	node_shape_style_color_quoted(file,                        box,       filled,          paleturquoise,        false).
	node_shape_style_color_quoted(directory,                   tab,       filled,          lightsalmon,          false).
	node_shape_style_color_quoted(library,                     tab,       filled,          lightsalmon,          false).
	% external entities to the file or library being documented
	node_shape_style_color_quoted(external_prototype,          box,       'filled,dashed', beige,                false).
	node_shape_style_color_quoted(external_class,              box,       'filled,dashed', lightgoldenrodyellow, false).
	node_shape_style_color_quoted(external_instance,           box,       'filled,dashed', lightgoldenrodyellow, false).
	node_shape_style_color_quoted(external_instance_and_class, box,       'filled,dashed', lightgoldenrodyellow, false).
	node_shape_style_color_quoted(external_protocol,           note,      'filled,dashed', mediumaquamarine,     false).
	node_shape_style_color_quoted(external_category,           component, 'filled,dashed', cyan,                 false).
	node_shape_style_color_quoted(external_module,             tab,       'filled,dashed', thistle,              false).
	node_shape_style_color_quoted(external_file,               box,       'filled,dashed', powderblue,           false).
	node_shape_style_color_quoted(external_directory,          tab,       'filled,dashed', salmon,               false).
	node_shape_style_color_quoted(external_library,            tab,       'filled,dashed', salmon,               false).
	% predicates of the entities being documented
	node_shape_style_color_quoted(directive,                   box,       filled,          bisque,               true).
	node_shape_style_color_quoted(predicate,                   box,       filled,          cornsilk,             true).
	node_shape_style_color_quoted(public_predicate,            box,       filled,          springgreen,          true).
	node_shape_style_color_quoted(protected_predicate,         box,       filled,          yellow,               true).
	node_shape_style_color_quoted(private_predicate,           box,       filled,          indianred1,           true).
	node_shape_style_color_quoted(local_predicate,             box,       filled,          cornsilk,             true).
	node_shape_style_color_quoted(multifile_predicate,         box,       filled,          skyblue,              true).
	node_shape_style_color_quoted(exported_predicate,          box,       filled,          springgreen,          true).
	% external predicates to the entities being documented
	node_shape_style_color_quoted(external_predicate,          box,       'filled,dashed', beige,                true).

	edge(Stream, _-Start, _-End, Labels, Kind, Options) :-
		!,
		edge(Stream, Start, End, Labels, Kind, Options).
	edge(Stream, Start, End, Labels, Kind, Options) :-
		edge_arrow(Kind, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" ['),
		write_key_value_comma(Stream, arrowhead, ArrowHead),
		(	^^option(url(URL), Options),
			URL \== '' ->
			write_key_value_comma(Stream, 'URL', URL),
			write_key_value_comma(Stream, labeltooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, labeltooltip, Tooltip)
		;	true
		),
		write(Stream, 'label=<'),
		write_edge_lines(Labels, Stream),
		write(Stream, '>]\n').

	% entity relations
	edge_arrow(extends_object, vee).
	edge_arrow(extends_protocol, vee).
	edge_arrow(extends_category, vee).
	edge_arrow(instantiates_class, normal).
	edge_arrow(specializes_class, onormal).
	edge_arrow(implements_protocol, dot).
	edge_arrow(imports_category, box).
	edge_arrow(complements_object, obox).
	% multifile predicates
	edge_arrow(provides_clauses, inv).
	% cross-referencing predicate calls
	edge_arrow(calls_predicate, normal).
	edge_arrow(calls_super_predicate, normal).
	edge_arrow(calls_self_predicate, normal).
	% dynamic predicate updates
	edge_arrow(updates_predicate, diamond).
	edge_arrow(updates_this_predicate, diamond).
	edge_arrow(updates_self_predicate, diamond).
	% file relations
	edge_arrow(depends_on_file, normal).
	edge_arrow(loads_file, normal).
	edge_arrow(includes_file, normal).
	% directory relations
	edge_arrow(depends_on_directory, normal).
	edge_arrow(loads_directory, normal).
	% library relations
	edge_arrow(depends_on_library, normal).
	edge_arrow(loads_library, normal).

	write_key_value_nl(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		nl(Stream).

	write_key_value_comma(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		write(Stream, ',').

	write_key_value(Stream, Key, Value) :-
		write(Stream, Key),
		write(Stream, '="'),
		write_escaped_term(Stream, Value, []),
		write(Stream, '"').

	write_node_lines([], _, _).
	write_node_lines([Line| Lines], Stream, Options) :-
		write(Stream, '<TR><TD> </TD><TD>'),
		write_escaped_term(Stream, Line, Options),
		write(Stream, '</TD><TD> </TD></TR>'),
		write_node_lines(Lines, Stream, Options).

	write_edge_lines([], _).
	write_edge_lines([Line| Lines], Stream) :-
		write_edge_lines(Lines, Line, Stream).

	write_edge_lines([], Line, Stream) :-
		write_escaped_term(Stream, Line, []).
	write_edge_lines([Next| Lines], Line, Stream) :-
		write_escaped_term(Stream, Line, []),
		write(Stream, '<BR/>'),
		write_edge_lines(Lines, Next, Stream).

	% CDATA tags are not officially supported in dot as of version 2.38 and
	% are broken as they don't escape problematic characters; we try to
	% workaround the problem by manually escaping characters but the chosen
	% solution requires writing a non-atomic term to a list of characters,
	% which uses a slow but portable implementation as this is a non-standard
	% functionality that only some backend systems provide in a usable form
	write_escaped_term(Stream, Term, Options) :-
		write_term_to_chars(Term, Chars, Options),
		write_escaped_chars(Chars, Stream).

	write_escaped_chars([], _).
	write_escaped_chars([Char| Chars], Stream) :-
		(	Char == ('>') ->
			write(Stream, '&gt;')
		;	Char == ('<') ->
			write(Stream, '&lt;')
		;	Char == ('&') ->
			write(Stream, '&amp;')
		;	Char == ('"') ->
			write(Stream, '&quot;')
		;	put_char(Stream, Char)
		),
		write_escaped_chars(Chars, Stream).

:- end_object.
