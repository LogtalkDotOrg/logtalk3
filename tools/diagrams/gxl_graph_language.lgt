%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(gxl_graph_language,
	implements(graph_language_protocol)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2014/08/18,
		comment is 'Predicates for generating graph files in the GXL language.'
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(gxl, gxl_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.gxl', File).

	file_header(Stream, _Identifier, _Options) :-
		write(Stream, '<?xml version="1.0" encoding="UTF-8"?>\n'),
		write(Stream, '<gxl xmlns:xlink=" http://www.w3.org/1999/xlink">\n'),
		write(Stream, '  <graph id="diagram" edgeids="true" edgemode="defaultdirected" hypergraph="false">\n').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '  </graph>\n'),
		write(Stream, '</gxl>\n').

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		fail.

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		fail.

	node(Stream, Identifier, Label, Contents, Kind, Options) :-
		fail.

	edge(Stream, Start, End, Labels, Kind, Options) :-
		fail.

:- end_object.
