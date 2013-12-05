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


:- object(dot_diagram,
	imports(diagram)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/05,
		comment is 'Generates entity diagram DOT files for source files and libraries.'
	]).

	:- multifile(diagram(_)::format_object/2).
	diagram(_)::format_object(dot, dot_diagram).

	output_file_name(Name, OutputFile) :-
		atom_concat(Name, '.dot', OutputFile).

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

	graph_header(Stream, Id, Label, _Options) :-
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Id),
		write(Stream, '" {\n'),
		write(Stream, 'bgcolor=snow3\nlabel="'),
		write(Stream, Label),
		write(Stream, '"\n').

	graph_footer(Stream, _Id, _Label, _Options) :-
		write(Stream, '}\n\n').

	subgraph_header(Stream, Id, Label, _Options) :-
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Id),
		write(Stream, '" {\n'),
		write(Stream, 'bgcolor=snow2\nlabel="'),
		write(Stream, Label),
		write(Stream, '"\n').

	subgraph_footer(Stream, _Id, _Label, _Options) :-
		write(Stream, '}\n\n').

	externals_subgraph_header(Stream, _Options) :-
		write(Stream, 'subgraph "cluster_others" {\n'),
		write(Stream, 'bgcolor=white\nlabel="(other referenced entities)"\n').

	externals_subgraph_footer(Stream, _Options) :-
		write(Stream, '}\n').

	node(Stream, Label, Predicates, Kind) :-
		predicate_list_to_atom(Predicates, Text),
		entity_shape(Kind, Shape, Style),
		write(Stream, '"'),
		write(Stream, Label),
		write(Stream, '" [shape='),
		write(Stream, Shape),
		write(Stream, ',style='),
		write(Stream, Style),
		write(Stream, ',label=<<B>'),
		write(Stream, Label),
		write(Stream, '</B><BR/>'),
		write(Stream, Text),
		write(Stream, '>]\n').

	entity_shape(prototype, box, solid).
	entity_shape(instance_or_class, box, solid).
	entity_shape(protocol, note, solid).
	entity_shape(category, component, solid).

	entity_shape(external_prototype, box, dashed).
	entity_shape(external_instance_or_class, box, dashed).
	entity_shape(external_protocol, note, dashed).
	entity_shape(external_category, component, dashed).

	arrow(Stream, Start, End, Label, Options) :-
		label_arrowhead(Label, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" [arrowhead='),
		write(Stream, ArrowHead),
		(	member(relation_labels(true), Options) ->
			write(Stream, ',label="'),
			write(Stream, Label),
			write(Stream, '"]\n')
		;	write(Stream, ',label=""]\n')
		).

	label_arrowhead(extends, vee).
	label_arrowhead(instantiates, normal).
	label_arrowhead(specializes, onormal).
	label_arrowhead(implements, dot).
	label_arrowhead(imports, box).
	label_arrowhead(complements, obox).
	label_arrowhead(uses, none).
	label_arrowhead(use_module, none).

	predicate_list_to_atom([], '').
	predicate_list_to_atom([Predicate| Predicates], Atom) :-
		predicate_list_to_atom([Predicate| Predicates], ' <BR/>', Atom).

	predicate_list_to_atom([], Atom, Atom).
	predicate_list_to_atom([Functor/Arity| Predicates], Atom0, Atom) :-
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Atom0, '<BR/>', Atom1),
		atom_concat(Atom1, Functor, Atom2),
		atom_concat(Atom2, '/', Atom3),
		atom_concat(Atom3, ArityAtom, Atom4),
		predicate_list_to_atom(Predicates, Atom4, Atom).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
