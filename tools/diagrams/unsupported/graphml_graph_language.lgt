%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(graphml_graph_language,
	implements(graph_language_protocol)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2014/08/18,
		comment is 'Predicates for generating graph files in the GraphML language.'
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(graphml, graphml_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.graphml', File).

	file_header(Stream, _Identifier, _Options) :-
		write(Stream, '<?xml version="1.0" encoding="UTF-8"?>\n'),
		write(Stream, '<graphml xmlns="http://graphml.graphdrawing.org/xmlns"\n'),
		write(Stream, '    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n'),
		write(Stream, '    xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns\n'),
		write(Stream, '     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">\n'),
		write(Stream, '<key id="kind" for="node" attr.name="kind" attr.type="string"/>\n'),
		write(Stream, '<key id="label" for="node" attr.name="label" attr.type="string"/>\n'),
		write(Stream, '<key id="lines" for="node" attr.name="lines" attr.type="string"/>\n'),
		write(Stream, '<key id="kind" for="edge" attr.name="kind" attr.type="string"/>\n'),
		write(Stream, '<key id="labels" for="edge" attr.name="labels" attr.type="string"/>\n').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '</graphml>\n').

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		write(Stream, '<graph id="'),
		write(Stream, Identifier),
		write(Stream, '" edgedefault="undirected">\n').

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '</graph>\n\n').

	node(Stream, Identifier, Label, Contents, Kind, Options) :-
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
		write(Stream, Contents),
		write(Stream, '</data>\n'),
		write(Stream, '    </node>\n').

	edge(Stream, Start, End, Labels, Kind, Options) :-
		write(Stream, '    <edge source="'),
		write(Stream, Start),
		write(Stream, '" target="'),
		write(Stream, End),
		write(Stream, '"/>\n'),
		write(Stream, '        <data key="kind">'),
		write(Stream, Kind),
		write(Stream, '</data>\n'),
		write(Stream, '        <data key="label">'),
		write(Stream, Labels),
		write(Stream, '</data>\n'),
		write(Stream, '    </edge>\n').

:- end_object.
