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


:- object(dot_graph,
	implements(graphp)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/06,
		comment is 'Predicates for generating graph files in the DOT language.'
	]).

 	:- multifile(diagram(_)::format_object/2).
	diagram(_)::format_object(dot, dot_graph).

	output_file_name(Name, File) :-
		atom_concat(Name, '.dot', File).

	file_header(Stream, Identifier, Options) :-
		write(Stream, 'digraph '),
		writeq(Stream, Identifier),
		write(Stream, ' {\n'),
		write(Stream, 'rankdir=BT\n'),
		write(Stream, 'ranksep=1.25\n'),
		write(Stream, 'compound=true\n'),
		write(Stream, 'splines=true\n'),
		write(Stream, 'pack=true\n'),
		write(Stream, 'clusterrank=local\n'),
		write(Stream, 'labeljust=l\n'),
		write(Stream, 'margin=1.0\n'),
		write(Stream, 'fontname="Courier"\n'),
		write(Stream, 'fontsize=10\n'),
		write(Stream, 'fontcolor=snow4\n'),
		write(Stream, 'pencolor=snow4\n'),
		write(Stream, 'node [shape=ellipse,style=filled,fillcolor=white,fontname="Courier",fontsize=9]\n'),
		write(Stream, 'edge [fontname="Courier",fontsize=9]\n'),
		output_date(Stream, Options),
		nl(Stream).

	output_date(Stream, Options) :-
		(	member(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),			
			write(Stream, '\nlabel="Generated on '),
			write(Stream, Year), write(Stream, '/'),
			write(Stream, PaddedMonth), write(Stream, '/'),
			write(Stream, PaddedDay),
			write(Stream, ', '),
			write(Stream, PaddedHours), write(Stream, ':'),
			write(Stream, PaddedMinutes),
			write(Stream, '"')
		;	true
		).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			atom_codes(Atom, [0'0| Codes])
		;	atom_codes(Atom, Codes)
		).

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '}\n').

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		graph_style_margin_color(Kind, Style, Margin, Color),
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Identifier),
		write(Stream, '" {\n'),
		(	member(url(URL), Options) ->
			write(Stream, 'URL="'),
			write(Stream, URL),
			write(Stream, '"\n'),
			write(Stream, 'tooltip="'),
			write(Stream, URL),
			write(Stream, '"\n')
		;	member(tooltip(Tooltip), Options) ->
			write(Stream, 'tooltip="'),
			write(Stream, Tooltip),
			write(Stream, '"\n')
		;	true
		),
		write(Stream, 'bgcolor="'),
		write(Stream, Color),
		write(Stream, '"\nstyle="'),
		write(Stream, Style),
		write(Stream, '"\nmargin="'),
		write(Stream, Margin),
		write(Stream, '"\nlabel="'),
		write(Stream, Label),
		write(Stream, '"\n').

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}\n\n').

	graph_style_margin_color(rlibrary, rounded, 10, snow3).
	graph_style_margin_color(libraries, rounded, 10, snow3).
	graph_style_margin_color(library, rounded, 10, snow2).
	graph_style_margin_color(files, rounded, 10, snow2).
	graph_style_margin_color(file, rounded, 10, snow).
	graph_style_margin_color(external, rounded, 10, white).

	node(Stream, Identifier, Label, Contents, Kind, Options) :-
		node_shape_style_color(Kind, Shape, Style, Color),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '" [shape='),
		write(Stream, Shape),
		(	member(url(URL), Options) ->
			write(Stream, ',URL="'),
			write(Stream, URL),
			write(Stream, '",tooltip="'),
			write(Stream, URL),
			write(Stream, '"')
		;	member(tooltip(Tooltip), Options) ->
			write(Stream, ',tooltip="'),
			write(Stream, Tooltip),
			write(Stream, '"')
		;	true
		),
		write(Stream, ',style='),
		write(Stream, Style),
		write(Stream, ',fillcolor="'),
		write(Stream, Color),
		write(Stream, '",label=<<B>'),
		write(Stream, Label),
		(	Contents == [] ->
			write(Stream, '</B>')
		;	write(Stream, '</B><BR/> <BR/>'),
			write_lines(Contents, Stream)
		),
		write(Stream, '>]\n').

	node_shape_style_color(prototype, box, filled, beige).
	node_shape_style_color(instance_or_class, box, filled, yellow).
	node_shape_style_color(protocol, note, filled, aquamarine).
	node_shape_style_color(category, component, filled, cyan).
	node_shape_style_color(module, tab, filled, gainsboro).
	node_shape_style_color(file, box, filled, turquoise).

	node_shape_style_color(external_prototype, box, '"filled,dashed"', beige).
	node_shape_style_color(external_instance_or_class, box, '"filled,dashed"', yellow).
	node_shape_style_color(external_protocol, note, '"filled,dashed"', aquamarine).
	node_shape_style_color(external_category, component, '"filled,dashed"', cyan).
	node_shape_style_color(external_module, tab, '"filled,dashed"', gainsboro).
	node_shape_style_color(external_file, box, '"filled,dashed"', turquoise).

	edge(Stream, Start, End, Labels, Kind, Options) :-
		edge_arrow(Kind, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" [arrowhead='),
		write(Stream, ArrowHead),
		(	member(tooltip(Tooltip), Options) ->
			write(Stream, ',tooltip="'),
			write(Stream, Tooltip),
			write(Stream, '"')
		;	true
		),
		write(Stream, ',label=<'),
		write_lines(Labels, Stream),
		write(Stream, '>]\n').

	edge_arrow(extends_object, vee).
	edge_arrow(extends_protocol, vee).
	edge_arrow(extends_category, vee).
	edge_arrow(instantiates_class, normal).
	edge_arrow(specializes_class, onormal).
	edge_arrow(implements_protocol, dot).
	edge_arrow(imports_category, box).
	edge_arrow(complements_object, obox).
	edge_arrow(depends_on_file, rdiamond).
	edge_arrow(loads_file, normal).

	write_lines([], _).
	write_lines([Line| Lines], Stream) :-
		write(Stream, '<![CDATA['),
		write(Stream, Line),
		write(Stream, ']]><BR/>'),
		write_lines(Lines, Stream).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
