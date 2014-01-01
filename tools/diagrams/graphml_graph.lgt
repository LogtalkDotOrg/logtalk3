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


:- object(graphml_graph,
	implements(graphp)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/18,
		comment is 'Generates entity diagram GraphML files for source files and libraries.'
	]).

 	:- multifile(diagram(_)::format_object/2).
	diagram(_)::format_object(graphml, graphml_graph).

	output_file_name(Name, File) :-
		atom_concat(Name, '.graphml', File).

	output_file_header(Stream, _Options) :-
		write(Stream, '<?xml version="1.0" encoding="UTF-8"?>\n'),
		write(Stream, '<graphml xmlns="http://graphml.graphdrawing.org/xmlns"\n'),
		write(Stream, '    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n'),
		write(Stream, '    xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns\n'),
		write(Stream, '     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">\n'),
		write(Stream, '<key id="kind" for="node" attr.name="kind" attr.type="string"/>\n'),
		write(Stream, '<key id="label" for="node" attr.name="label" attr.type="string"/>\n'),
		write(Stream, '<key id="lines" for="node" attr.name="lines" attr.type="string"/>\n'),
		write(Stream, '<key id="kind" for="edge" attr.name="kind" attr.type="string"/>\n'),
		write(Stream, '<key id="label" for="edge" attr.name="label" attr.type="string"/>\n').

	output_file_footer(Stream, _Options) :-
		write(Stream, '</graphml>\n').

	graph_header(Stream, Identifier, _Label, _Options) :-
		write(Stream, '<graph id="'),
		write(Stream, Identifier),
		write(Stream, '" edgedefault="undirected">\n').

	graph_footer(Stream, _Identifier, _Label, _Options) :-
		write(Stream, '</graph>\n\n').

	node(Stream, Identifier, Label, Lines, Kind, _Options) :-
		write(Stream, '    <node id="'),
		write(Stream, Identifier),
		write(Stream, '">\n'),
		write(Stream, '        <data key="kind">'),
		write(Stream, Kind),
		write(Stream, '</data>\n'),
		write(Stream, '        <data key="label">'),
		write(Stream, Label),
		write(Stream, '</data>\n'),
		write(Stream, '        <data key="lines">'),
		write(Stream, Lines),
		write(Stream, '</data>\n'),
		write(Stream, '    </node>\n').

	edge(Stream, Start, End, Label, Kind, _Options) :-
		write(Stream, '    <edge source="'),
		write(Stream, Start),
		write(Stream, '" target="'),
		write(Stream, End),
		write(Stream, '"/>\n'),
		write(Stream, '        <data key="kind">'),
		write(Stream, Kind),
		write(Stream, '</data>\n'),
		write(Stream, '        <data key="label">'),
		write(Stream, Label),
		write(Stream, '</data>\n'),
		write(Stream, '    </edge>\n').

:- end_object.
