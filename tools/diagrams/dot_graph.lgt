%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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
		date is 2013/12/28,
		comment is 'Generates entity diagram DOT files for source files and libraries.'
	]).

 	:- multifile(diagram(_)::format_object/2).
	diagram(_)::format_object(dot, dot_graph).

	output_file_name(Name, File) :-
		atom_concat(Name, '.dot', File).

	output_file_header(Stream, Options) :-
		write(Stream, 'digraph G {\n'),
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
		write(Stream, 'node [shape=ellipse,style=dashed,fillcolor=white,fontname="Courier",fontsize=9]\n'),
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

	output_file_footer(Stream, _Options) :-
		write(Stream, '}\n').

	graph_header(Stream, Identifier, Label, Options) :-
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Identifier),
		write(Stream, '" {\n'),
		write(Stream, 'bgcolor="'),
		(	member(bgcolor(BGColor), Options) ->
			true
		;	BGColor = white
		),
		write(Stream, BGColor),
		write(Stream, '"\nlabel="'),
		write(Stream, Label),
		write(Stream, '"\n').

	graph_footer(Stream, _Identifier, _Label, _Options) :-
		write(Stream, '}\n\n').

	node(Stream, Identifier, Label, Lines, Kind, _Options) :-
		lines_to_contents(Lines, Contents),
		entity_shape(Kind, Shape, Style),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '" [shape='),
		write(Stream, Shape),
		write(Stream, ',style='),
		write(Stream, Style),
		write(Stream, ',label=<<B>'),
		write(Stream, Label),
		write(Stream, '</B><BR/>'),
		write(Stream, Contents),
		write(Stream, '>]\n').

	entity_shape(prototype, box, solid).
	entity_shape(instance_or_class, box, solid).
	entity_shape(protocol, note, solid).
	entity_shape(category, component, solid).
	entity_shape(module, tab, solid).

	entity_shape(external_prototype, box, dashed).
	entity_shape(external_instance_or_class, box, dashed).
	entity_shape(external_protocol, note, dashed).
	entity_shape(external_category, component, dashed).
	entity_shape(external_module, box, solid).

	entity_shape(file, box, solid).

	edge(Stream, Start, End, Labels, Kind, _Options) :-
		labels_to_lines(Labels, Lines),
		kind_edge(Kind, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" [arrowhead='),
		write(Stream, ArrowHead),
		write(Stream, ',label="'),
		write(Stream, Lines),
		write(Stream, '"]\n').

	kind_edge(extends_object, vee).
	kind_edge(extends_protocol, vee).
	kind_edge(extends_category, vee).
	kind_edge(instantiates_class, normal).
	kind_edge(specializes_class, onormal).
	kind_edge(implements_protocol, dot).
	kind_edge(imports_category, box).
	kind_edge(complements_object, obox).
	kind_edge(calls_predicate, halfopen).
	kind_edge(loads_file, normal).

	lines_to_contents([], '').
	lines_to_contents([Line| Lines], Contents) :-
		lines_to_contents([Line| Lines], ' <BR/>', Contents).

	lines_to_contents([], Contents, Contents).
	lines_to_contents([Line| Lines], Contents0, Contents) :-
		atom_concat('<![CDATA[', Line, WrappedLine0),
		atom_concat(WrappedLine0, ']]>', WrappedLine),
		atom_concat(Contents0, '<BR/>', Contents1),
		atom_concat(Contents1, WrappedLine, Contents2),
		lines_to_contents(Lines, Contents2, Contents).

	labels_to_lines([], '').
	labels_to_lines([Line| Lines], Contents) :-
		labels_to_lines(Lines, Line, Contents).

	labels_to_lines([], Contents, Contents).
	labels_to_lines([Line| Lines], Contents0, Contents) :-
		atom_concat(Contents0, '\n', Contents1),
		atom_concat(Contents1, Line, Contents2),
		labels_to_lines(Lines, Contents2, Contents).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
