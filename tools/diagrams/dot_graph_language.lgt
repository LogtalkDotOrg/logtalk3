%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(dot_graph_language,
	implements(graph_language_protocol)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/12/30,
		comment is 'Predicates for generating graph files in the DOT language (version 2.36.0 or later).'
	]).

	:- uses(list, [member/2, memberchk/2]).

	:- multifile(graph_language_registry::language_object/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(dot, dot_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.dot', File).

	file_header(Stream, Identifier, Options) :-
		write(Stream, 'digraph "'),
		write(Stream, Identifier),
		write(Stream, '" {'), nl(Stream),
		memberchk(layout(Layout), Options),
		convert_layout(Layout, RankDir),
		write_key_value_nl(Stream, rankdir, RankDir),
		write_key_value_nl(Stream, ranksep, '1.25'),
		write_key_value_nl(Stream, compound, true),
		write_key_value_nl(Stream, splines, true),
		write_key_value_nl(Stream, pack, true),
		write_key_value_nl(Stream, clusterrank, local),
		write_key_value_nl(Stream, labeljust, l),
		write_key_value_nl(Stream, margin, '1.0'),
		write_key_value_nl(Stream, fontname, 'Courier'),
		write_key_value_nl(Stream, fontsize, 10),
		write_key_value_nl(Stream, fontcolor, snow4),
		write_key_value_nl(Stream, pencolor, snow4),
		write(Stream, 'node [shape="ellipse",style="filled",fillcolor="white",fontname="Courier",fontsize="9"]'), nl(Stream),
		write(Stream, 'edge [fontname="Courier",fontsize="9"]'), nl(Stream),
		diagram_label(Options, Label),
		write_key_value_nl(Stream, label, Label),
		nl(Stream).

	convert_layout(top_to_bottom, 'TB').
	convert_layout(bottom_to_top, 'BT').
	convert_layout(left_to_right, 'LR').
	convert_layout(right_to_left, 'RL').

	diagram_label(Options, Label) :-
		memberchk(title(Title), Options),
		(	Title \== '' ->
			atom_concat(Title, '\\l', Label0)
		;	Label0 = ''
		),
		(	memberchk(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			number_codes(Year, YearCodes),
			atom_codes(YearAtom, YearCodes),
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atom_concat(Label0, 'Generated on ', Label1),
			atom_concat(Label1, YearAtom, Label2),
			atom_concat(Label2, '/', Label3),
			atom_concat(Label3, PaddedMonth, Label4),
			atom_concat(Label4, '/', Label5),
			atom_concat(Label5, PaddedDay, Label6),
			atom_concat(Label6, ', ', Label7),
			atom_concat(Label7, PaddedHours, Label8),
			atom_concat(Label8, ':', Label9),
			atom_concat(Label9, PaddedMinutes, Label10),
			atom_concat(Label10, '\\l', Label)
		;	Label = Label0
		).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '}'), nl(Stream).

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		graph_style_margin_color(Kind, Style, Margin, Color),
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Identifier),
		write(Stream, '" {'), nl(Stream),
		(	memberchk(urls(URL, _), Options), URL \== '' ->
			write_key_value_nl(Stream, 'URL', URL),
			write_key_value_nl(Stream, tooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_nl(Stream, tooltip, Tooltip)
		;	true
		),
		write_key_value_nl(Stream, bgcolor, Color),
		write_key_value_nl(Stream, style, Style),
		write_key_value_nl(Stream, margin, Margin),
		write_key_value_nl(Stream, label, Label).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}'), nl(Stream), nl(Stream).

	graph_style_margin_color(rlibrary, rounded, 10, snow3).
	graph_style_margin_color(libraries, rounded, 10, snow3).
	graph_style_margin_color(library, rounded, 10, snow2).
	graph_style_margin_color(rdirectory, rounded, 10, snow3).
	graph_style_margin_color(directories, rounded, 10, snow3).
	graph_style_margin_color(directory, rounded, 10, snow2).
	graph_style_margin_color(files, rounded, 10, snow2).
	graph_style_margin_color(file, rounded, 10, snow).
	graph_style_margin_color(external, rounded, 10, white).
	graph_style_margin_color(entity, rounded, 10, snow).

	node(Stream, Identifier, Label, Contents, Kind, Options) :-
		node_caption_shape_style_color(Kind, Caption, Shape, Style, Color),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '" ['),
		write_key_value_comma(Stream, shape, Shape),
		(	(	Kind == file, member(urls(URL, _), Options)
			;	Kind == external_file, member(urls(URL, _), Options)
			;	member(urls(_, URL), Options)	% entities or predicates
			),
			URL \== '' ->
			write_key_value_comma(Stream, 'URL', URL),
			write_key_value_comma(Stream, tooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, tooltip, Tooltip)
		;	true
		),
		write_key_value_comma(Stream, style, Style),
		write_key_value_comma(Stream, fillcolor, Color),
		write(Stream, 'label=<<FONT POINT-SIZE="11"><![CDATA['),
		write(Stream, Label),
		write(Stream, ']]></FONT>'),
		(	member(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '<BR /><FONT POINT-SIZE="7"><![CDATA['),
			write(Stream, Caption),
			write(Stream, ']]></FONT>')
		;	true
		),
		(	Contents == [] ->
			true
		;	write(Stream, '<BR/> <BR/>'),
			write_lines(Contents, Stream)
		),
		write(Stream, '>]'), nl(Stream).

	% entities belonging to the file or library being documented
	node_caption_shape_style_color(prototype, prototype, box, filled, beige).
	node_caption_shape_style_color(class, class, box, filled, yellow).
	node_caption_shape_style_color(instance, instance, box, filled, yellow).
	node_caption_shape_style_color(instance_and_class, 'instance/class', box, filled, yellow).
	node_caption_shape_style_color(protocol, protocol, note, filled, aquamarine).
	node_caption_shape_style_color(category, category, component, filled, lightcyan).
	node_caption_shape_style_color(module, module, tab, filled, gainsboro).
	node_caption_shape_style_color(file, file, box, filled, paleturquoise).
	% external entities to the file or library being documented
	node_caption_shape_style_color(external_prototype, prototype, box, 'filled,dashed', beige).
	node_caption_shape_style_color(external_class, class, box, 'filled,dashed', yellow).
	node_caption_shape_style_color(external_instance, instance, box, 'filled,dashed', yellow).
	node_caption_shape_style_color(external_instance_and_class, 'instance/class', box, 'filled,dashed', yellow).
	node_caption_shape_style_color(external_protocol, protocol, note, 'filled,dashed', aquamarine).
	node_caption_shape_style_color(external_category, category, component, 'filled,dashed', lightcyan).
	node_caption_shape_style_color(external_module, module, tab, 'filled,dashed', gainsboro).
	node_caption_shape_style_color(external_file, file, box, 'filled,dashed', paleturquoise).
	% predicates
	node_caption_shape_style_color(predicate, '', ellipse, filled, beige).
	node_caption_shape_style_color(public_predicate, (public), ellipse, filled, springgreen).
	node_caption_shape_style_color(protected_predicate, protected, ellipse, filled, yellow).
	node_caption_shape_style_color(private_predicate, (private), ellipse, filled, indianred1).
	node_caption_shape_style_color(local_predicate, (local), ellipse, filled, grey).
	node_caption_shape_style_color(multifile_predicate, 'public, multifile', ellipse, filled, skyblue).
	node_caption_shape_style_color(module_multifile_predicate, (multifile), ellipse, filled, skyblue).
	node_caption_shape_style_color(external_predicate, external, ellipse, 'filled,dashed', lightgrey).
	node_caption_shape_style_color(exported_predicate, exported, ellipse, filled, springgreen).

	edge(Stream, Start, End, Labels, Kind, Options) :-
		edge_arrow(Kind, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" ['),
		write_key_value_comma(Stream, arrowhead, ArrowHead),
		(	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, tooltip, Tooltip)
		;	true
		),
		write(Stream, 'label=<'),
		write_lines(Labels, Stream),
		write(Stream, '>]'), nl(Stream).

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
	edge_arrow(calls_predicate, rdiamond).
	% file relations
	edge_arrow(depends_on_file, normal).
	edge_arrow(loads_file, normal).

	write_key_value_nl(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		nl(Stream).

	write_key_value_comma(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		write(Stream, ',').

	write_key_value(Stream, Key, Value) :-
		write(Stream, Key),
		write(Stream, '="'),
		write(Stream, Value),
		write(Stream, '"').

	write_lines([], _).
	write_lines([Line| Lines], Stream) :-
		write(Stream, '<![CDATA['),
		write(Stream, Line),
		write(Stream, ']]><BR/>'),
		write_lines(Lines, Stream).

:- end_object.
