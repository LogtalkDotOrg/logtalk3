%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(cytoscapejs_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Predicates for generating diagram files in the Cytoscape Exchange (CX2) JSON format.'
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
	graph_language_registry::language_object(cx2, cytoscapejs_graph_language).

	:- private(section_started_/1).
	:- dynamic(section_started_/1).
	:- mode(section_started_(?atom), zero_or_more).
	:- info(section_started_/1, [
		comment is 'Tracks whether a JSON array section (``nodes`` or ``edges``) has already emitted at least one element.',
		argnames is ['Section']
	]).

	:- private(in_nodes_section_/0).
	:- dynamic(in_nodes_section_/0).
	:- mode(in_nodes_section_, zero_or_one).
	:- info(in_nodes_section_/0, [
		comment is 'Flag indicating if the exporter is currently writing the nodes section.'
	]).

	:- private(parent_stack_/1).
	:- dynamic(parent_stack_/1).
	:- mode(parent_stack_(?list(nonvar)), zero_or_one).
	:- info(parent_stack_/1, [
		comment is 'Current stack of open graph container identifiers.',
		argnames is ['Stack']
	]).

	:- private(edge_counter_/1).
	:- dynamic(edge_counter_/1).
	:- mode(edge_counter_(?integer), zero_or_one).
	:- info(edge_counter_/1, [
		comment is 'Current counter used to generate unique edge identifiers.',
		argnames is ['Counter']
	]).

	:- private(node_counter_/1).
	:- dynamic(node_counter_/1).
	:- mode(node_counter_(?integer), zero_or_one).
	:- info(node_counter_/1, [
		comment is 'Current counter used to generate unique node identifiers.',
		argnames is ['Counter']
	]).

	:- private(node_id_/2).
	:- dynamic(node_id_/2).
	:- mode(node_id_(?nonvar, ?integer), zero_or_more).
	:- info(node_id_/2, [
		comment is 'Maps source node identifiers to generated CX2 numeric identifiers.',
		argnames is ['Identifier', 'NumericId']
	]).

	:- private(containment_edge_/2).
	:- dynamic(containment_edge_/2).
	:- mode(containment_edge_(?integer, ?integer), zero_or_more).
	:- info(containment_edge_/2, [
		comment is 'Stores pending containment edges between parent and child nodes.',
		argnames is ['ParentId', 'ChildId']
	]).

	output_file_name(Name, File) :-
		atom_concat(Name, '.cx2', File).

	file_header(Stream, _Identifier, Options) :-
		retractall(section_started_(_)),
		retractall(in_nodes_section_),
		retractall(parent_stack_(_)),
		retractall(edge_counter_(_)),
		retractall(node_counter_(_)),
		retractall(node_id_(_, _)),
		retractall(containment_edge_(_, _)),
		assertz(in_nodes_section_),
		assertz(parent_stack_([])),
		assertz(edge_counter_(0)),
		assertz(node_counter_(0)),
		write(Stream, '[\n'),
		write(Stream, '  {"CXVersion": "2.0", "hasFragments": false},\n'),
		write_attribute_declarations(Stream),
		write(Stream, ',\n'),
		write_network_attributes(Stream, Options),
		write(Stream, ',\n'),
		write(Stream, '  {"nodes": [').

	file_footer(Stream, _Identifier, _Options) :-
		(	in_nodes_section_ ->
			retract(in_nodes_section_),
			write(Stream, '\n  ]},\n'),
			write(Stream, '  {"edges": ['),
			emit_containment_edges(Stream),
			write(Stream, '\n  ]}')
		;	emit_containment_edges(Stream),
			write(Stream, '\n  ]}')
		),
		write(Stream, ',\n'),
		write_visual_properties(Stream),
		write(Stream, ',\n'),
		write(Stream, '  {"status": [{"success": true}]}\n'),
		write(Stream, ']\n'),
		retractall(section_started_(_)),
		retractall(parent_stack_(_)),
		retractall(edge_counter_(_)),
		retractall(node_counter_(_)),
		retractall(node_id_(_, _)),
		retractall(containment_edge_(_, _)).

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		container_node_style(Kind, Shape, Color, Border),
		ensure_node_id(Identifier, NodeId),
		(	parent_stack_([Parent| _]) ->
			ensure_node_id(Parent, ParentId)
		;	ParentId = 0
		),
		before_node(Stream),
		write(Stream, '\n    {"id": '),
		write(Stream, NodeId),
		write(Stream, ', "v": {"name": '),
		write_json_id(Stream, Label),
		write(Stream, ', "kind": "'),
		write(Stream, Kind),
		write(Stream, '", "shape": "'),
		write(Stream, Shape),
		write(Stream, '", "color": "'),
		write(Stream, Color),
		write(Stream, '", "border": "'),
		write(Stream, Border),
		write(Stream, '"'),
		(	ParentId =\= 0 ->
			write(Stream, ', "parent": '),
			write(Stream, ParentId)
		;	true
		),
		remember_containment_edge(ParentId, NodeId),
		(	member(url(URL), Options),
			URL \== '' ->
			write(Stream, ', "url": '),
			write_json_id(Stream, URL)
		;	true
		),
		(	member(urls(CodeURL, _), Options),
			CodeURL \== '' ->
			write(Stream, ', "code_url": '),
			write_json_id(Stream, CodeURL)
		;	true
		),
		(	member(zoom_url(ZoomURL), Options),
			ZoomURL \== '' ->
			normalize_zoom_url(ZoomURL, NormalizedZoomURL),
			write(Stream, ', "zoom_url": '),
			write_json_id(Stream, NormalizedZoomURL)
		;	true
		),
		write(Stream, '}}'),
		retract(parent_stack_(Stack)),
		assertz(parent_stack_([Identifier| Stack])).

	graph_footer(_Stream, _Identifier, _Label, _Kind, _Options) :-
		(	retract(parent_stack_([_| Rest])) ->
			assertz(parent_stack_(Rest))
		;	true
		).

	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_style(Kind, Shape, Color, Border),
		ensure_node_id(Identifier, NodeId),
		(	parent_stack_([Parent| _]) ->
			ensure_node_id(Parent, ParentId)
		;	ParentId = 0
		),
		before_node(Stream),
		write(Stream, '\n    {"id": '),
		write(Stream, NodeId),
		write(Stream, ', "v": {"name": '),
		write_json_id(Stream, Label),
		write(Stream, ', "kind": "'),
		write(Stream, Kind),
		write(Stream, '", "shape": "'),
		write(Stream, Shape),
		write(Stream, '", "color": "'),
		write(Stream, Color),
		write(Stream, '", "border": "'),
		write(Stream, Border),
		write(Stream, '"'),
		(	ParentId =\= 0 ->
			write(Stream, ', "parent": '),
			write(Stream, ParentId)
		;	true
		),
		remember_containment_edge(ParentId, NodeId),
		(	^^option(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, ', "caption": '),
			write_json_id(Stream, Caption)
		;	true
		),
		(	Contents \== [] ->
			write(Stream, ', "lines": ['),
			write_json_array(Contents, Stream),
			write(Stream, ']')
		;	true
		),
		(	member(url(URL), Options),
			URL \== '' ->
			write(Stream, ', "url": '),
			write_json_id(Stream, URL)
		;	true
		),
		(	member(urls(CodeURL, _), Options),
			CodeURL \== '' ->
			write(Stream, ', "code_url": '),
			write_json_id(Stream, CodeURL)
		;	true
		),
		(	member(zoom_url(ZoomURL), Options),
			ZoomURL \== '' ->
			normalize_zoom_url(ZoomURL, NormalizedZoomURL),
			write(Stream, ', "zoom_url": '),
			write_json_id(Stream, NormalizedZoomURL)
		;	true
		),
		(	member(tooltip(Tooltip), Options) ->
			write(Stream, ', "tooltip": '),
			write_json_id(Stream, Tooltip)
		;	true
		),
		(	member(metrics_overlay(Ce,Ca,I,A), Options) ->
			write(Stream, ', "metrics": "Ca:'),
			write(Stream, Ca),
			write(Stream, ' Ce:'),
			write(Stream, Ce),
			write(Stream, ' I:'),
			write(Stream, I),
			write(Stream, ' A:'),
			write(Stream, A),
			write(Stream, '"')
		;	true
		),
		write(Stream, '}}').

	remember_containment_edge(ParentId, ChildId) :-
		(	ParentId =:= 0 ->
			true
		;	assertz(containment_edge_(ParentId, ChildId))
		).

	emit_containment_edges(Stream) :-
		(	retract(containment_edge_(ParentId, ChildId)) ->
			before_edge(Stream),
			retract(edge_counter_(N)),
			N1 is N + 1,
			assertz(edge_counter_(N1)),
			EdgeId is -N1,
			write(Stream, '\n    {"id": '),
			write(Stream, EdgeId),
			write(Stream, ', "s": '),
			write(Stream, ParentId),
			write(Stream, ', "t": '),
			write(Stream, ChildId),
			write(Stream, ', "v": {"kind": "contains", "color": "#bbbbbb", "line_style": "dotted"}}'),
			emit_containment_edges(Stream)
		;	true
		).

	edge(Stream, _-Start, _-End, Labels, Kind, Options) :-
		!,
		edge(Stream, Start, End, Labels, Kind, Options).
	edge(Stream, Start, End, Labels, Kind, Options) :-
		(	in_nodes_section_ ->
			retract(in_nodes_section_),
			write(Stream, '\n  ]},\n'),
			write(Stream, '  {"edges": ['),
			emit_containment_edges(Stream)
		;	true
		),
		before_edge(Stream),
		ensure_node_id(Start, StartId),
		ensure_node_id(End, EndId),
		once(edge_style(Kind, EdgeColor0, LineStyle)),
		(	member(color(Color), Options) ->
			EdgeColor = Color
		;	EdgeColor = EdgeColor0
		),
		retract(edge_counter_(N)),
		N1 is N + 1,
		assertz(edge_counter_(N1)),
		EdgeId is -N1,
		write(Stream, '\n    {"id": '),
		write(Stream, EdgeId),
		write(Stream, ', "s": '),
		write(Stream, StartId),
		write(Stream, ', "t": '),
		write(Stream, EndId),
		write(Stream, ', "v": {"kind": "'),
		write(Stream, Kind),
		write(Stream, '", "color": "'),
		write(Stream, EdgeColor),
		write(Stream, '", "line_style": "'),
		write(Stream, LineStyle),
		write(Stream, '"'),
		(	Labels \== [] ->
			write(Stream, ', "label": '),
			write_json_edge_label(Labels, Stream)
		;	true
		),
		(	member(url(URL), Options),
			URL \== '' ->
			write(Stream, ', "url": '),
			write_json_id(Stream, URL)
		;	true
		),
		(	member(urls(CodeURL, _), Options),
			CodeURL \== '' ->
			write(Stream, ', "code_url": '),
			write_json_id(Stream, CodeURL)
		;	true
		),
		(	member(tooltip(Tooltip), Options) ->
			write(Stream, ', "tooltip": '),
			write_json_id(Stream, Tooltip)
		;	true
		),
		write(Stream, '}}').

	before_node(Stream) :-
		(	section_started_(nodes) ->
			write(Stream, ',')
		;	assertz(section_started_(nodes))
		).

	before_edge(Stream) :-
		(	section_started_(edges) ->
			write(Stream, ',')
		;	assertz(section_started_(edges))
		).

	ensure_node_id(Identifier, NodeId) :-
		(	node_id_(Identifier, NodeId) ->
			true
		;	retract(node_counter_(N)),
			NodeId is N + 1,
			assertz(node_counter_(NodeId)),
			assertz(node_id_(Identifier, NodeId))
		).

	write_attribute_declarations(Stream) :-
		write(Stream, '  {"attributeDeclarations": [\n'),
		write(Stream, '    {\n'),
		write(Stream, '      "networkAttributes": {\n'),
		write(Stream, '        "name": {"d": "string"},\n'),
		write(Stream, '        "description": {"d": "string"},\n'),
		write(Stream, '        "generated_at": {"d": "string"},\n'),
		write(Stream, '        "logtalk_version": {"d": "string"},\n'),
		write(Stream, '        "prolog_dialect": {"d": "string"},\n'),
		write(Stream, '        "prolog_version": {"d": "string"},\n'),
		write(Stream, '        "format": {"d": "string"}\n'),
		write(Stream, '      },\n'),
		write(Stream, '      "nodes": {\n'),
		write(Stream, '        "name": {"d": "string"},\n'),
		write(Stream, '        "kind": {"d": "string"},\n'),
		write(Stream, '        "shape": {"d": "string"},\n'),
		write(Stream, '        "color": {"d": "string"},\n'),
		write(Stream, '        "border": {"d": "string"},\n'),
		write(Stream, '        "parent": {"d": "long"},\n'),
		write(Stream, '        "caption": {"d": "string"},\n'),
		write(Stream, '        "lines": {"d": "list_of_string"},\n'),
		write(Stream, '        "url": {"d": "string"},\n'),
		write(Stream, '        "code_url": {"d": "string"},\n'),
		write(Stream, '        "zoom_url": {"d": "string"},\n'),
		write(Stream, '        "tooltip": {"d": "string"}\n'),
		write(Stream, '      },\n'),
		write(Stream, '      "edges": {\n'),
		write(Stream, '        "kind": {"d": "string"},\n'),
		write(Stream, '        "label": {"d": "string"},\n'),
		write(Stream, '        "color": {"d": "string"},\n'),
		write(Stream, '        "line_style": {"d": "string"},\n'),
		write(Stream, '        "url": {"d": "string"},\n'),
		write(Stream, '        "code_url": {"d": "string"},\n'),
		write(Stream, '        "tooltip": {"d": "string"}\n'),
		write(Stream, '      }\n'),
		write(Stream, '    }\n'),
		write(Stream, '  ]}').

	write_network_attributes(Stream, Options) :-
		^^option(title(Title), Options),
		^^option(description(Description), Options),
		write(Stream, '  {"networkAttributes": [\n'),
		write(Stream, '    {\n'),
		(	Title \== '' ->
			write(Stream, '      "name": '),
			write_json_id(Stream, Title),
			write(Stream, ',\n')
		;	true
		),
		(	Description \== '' ->
			write(Stream, '      "description": '),
			write_json_id(Stream, Description),
			write(Stream, ',\n')
		;	true
		),
		(	^^option(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atomic_list_concat([Year, '-', PaddedMonth, '-', PaddedDay, ' ', PaddedHours, ':', PaddedMinutes], Timestamp),
			write(Stream, '      "generated_at": '),
			write_json_id(Stream, Timestamp),
			write(Stream, ',\n')
		;	true
		),
		(	^^option(versions(true), Options) ->
			current_logtalk_flag(version_data, logtalk(LgtMajor, LgtMinor, LgtPatch, LgtStatus)),
			atomic_list_concat([LgtMajor, '.', LgtMinor, '.', LgtPatch, '-', LgtStatus], LgtVersion),
			write(Stream, '      "logtalk_version": '),
			write_json_id(Stream, LgtVersion),
			write(Stream, ',\n'),
			current_logtalk_flag(prolog_dialect, BackendId),
			backend(BackendId, BackendName),
			write(Stream, '      "prolog_dialect": '),
			write_json_id(Stream, BackendName),
			write(Stream, ',\n'),
			current_logtalk_flag(prolog_version, v(BMajor, BMinor, BPatch)),
			atomic_list_concat([BMajor, '.', BMinor, '.', BPatch], BVersion),
			write(Stream, '      "prolog_version": '),
			write_json_id(Stream, BVersion),
			write(Stream, ',\n')
		;	true
		),
		write(Stream, '      "format": "cx2"\n'),
		write(Stream, '    }\n'),
		write(Stream, '  ]}').

	write_visual_properties(Stream) :-
		write(Stream, '  {"visualProperties": [\n'),
		write(Stream, '    {\n'),
		write(Stream, '      "default": {\n'),
		write(Stream, '        "network": {"NETWORK_BACKGROUND_COLOR": "#FFFFFF"},\n'),
		write(Stream, '        "node": {"NODE_SHAPE": "rectangle", "NODE_BACKGROUND_COLOR": "#F5F5F5", "NODE_BORDER_STYLE": "solid", "NODE_BORDER_COLOR": "#666666", "NODE_BORDER_WIDTH": 1.2, "NODE_LABEL": "", "NODE_LABEL_COLOR": "#202020"},\n'),
		write(Stream, '        "edge": {"EDGE_LINE_STYLE": "solid", "EDGE_LINE_COLOR": "#666666", "EDGE_WIDTH": 1.4, "EDGE_TARGET_ARROW_SHAPE": "triangle", "EDGE_LABEL_COLOR": "#202020"}\n'),
		write(Stream, '      },\n'),
		write(Stream, '      "nodeMapping": {\n'),
		write(Stream, '        "NODE_LABEL": {"type": "PASSTHROUGH", "definition": {"attribute": "name"}},\n'),
		write(Stream, '        "NODE_SHAPE": {"type": "PASSTHROUGH", "definition": {"attribute": "shape"}},\n'),
		write(Stream, '        "NODE_BACKGROUND_COLOR": {"type": "PASSTHROUGH", "definition": {"attribute": "color"}},\n'),
		write(Stream, '        "NODE_BORDER_STYLE": {"type": "PASSTHROUGH", "definition": {"attribute": "border"}}\n'),
		write(Stream, '      },\n'),
		write(Stream, '      "edgeMapping": {\n'),
		write(Stream, '        "EDGE_LABEL": {"type": "PASSTHROUGH", "definition": {"attribute": "label"}},\n'),
		write(Stream, '        "EDGE_LINE_COLOR": {"type": "PASSTHROUGH", "definition": {"attribute": "color"}},\n'),
		write(Stream, '        "EDGE_LINE_STYLE": {"type": "PASSTHROUGH", "definition": {"attribute": "line_style"}}\n'),
		write(Stream, '      }\n'),
		write(Stream, '    }\n'),
		write(Stream, '  ]}').

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

	% graph container colors (used for nodes created from graph_header/5)
	node_color(rlibrary,    lightgray).
	node_color(libraries,   lightgray).
	node_color(library,     whitesmoke).
	node_color(rdirectory,  lightgray).
	node_color(directories, lightgray).
	node_color(directory,   whitesmoke).
	node_color(files,       whitesmoke).
	node_color(file,        snow).
	node_color(external,    white).
	node_color(entity,      snow).

	container_node_style(Kind, 'round-rectangle', Color, solid) :-
		node_color(Kind, Color).

	% node style: shape, color, and border for node/7 kinds
	% entities belonging to the file or library being documented
	node_style(prototype,                   rectangle,         cornsilk,             solid).
	node_style(class,                       rectangle,         yellow,               solid).
	node_style(instance,                    rectangle,         yellow,               solid).
	node_style(instance_and_class,          rectangle,         yellow,               solid).
	node_style(protocol,                    rectangle,         aquamarine,           solid).
	node_style(category,                    rectangle,         lightcyan,            solid).
	node_style(module,                      rectangle,         plum,                 solid).
	node_style(file,                        rectangle,         paleturquoise,        solid).
	node_style(directory,                   'round-rectangle', lightsalmon,          solid).
	node_style(library,                     'round-rectangle', lightsalmon,          solid).
	% external entities to the file or library being documented
	node_style(external_prototype,          rectangle,         beige,                dashed).
	node_style(external_class,              rectangle,         lightgoldenrodyellow, dashed).
	node_style(external_instance,           rectangle,         lightgoldenrodyellow, dashed).
	node_style(external_instance_and_class, rectangle,         lightgoldenrodyellow, dashed).
	node_style(external_protocol,           rectangle,         mediumaquamarine,     dashed).
	node_style(external_category,           rectangle,         cyan,                 dashed).
	node_style(external_module,             rectangle,         thistle,              dashed).
	node_style(external_file,               rectangle,         powderblue,           dashed).
	node_style(external_directory,          'round-rectangle', salmon,               dashed).
	node_style(external_library,            'round-rectangle', salmon,               dashed).
	% predicates of the entities being documented
	node_style(directive,                   rectangle,         bisque,               solid).
	node_style(predicate,                   rectangle,         cornsilk,             solid).
	node_style(public_predicate,            rectangle,         springgreen,          solid).
	node_style(protected_predicate,         rectangle,         yellow,               solid).
	node_style(private_predicate,           rectangle,         indianred,            solid).
	node_style(local_predicate,             rectangle,         cornsilk,             solid).
	node_style(multifile_predicate,         rectangle,         skyblue,              solid).
	node_style(exported_predicate,          rectangle,         springgreen,          solid).
	% external predicates to the entities being documented
	node_style(external_predicate,          rectangle,         beige,                dashed).

	% edge style: color and line style by relation kind
	edge_style(extends_object,         '#556b2f', solid).
	edge_style(extends_protocol,       '#556b2f', solid).
	edge_style(extends_category,       '#556b2f', solid).
	edge_style(instantiates_class,     '#6a5acd', solid).
	edge_style(specializes_class,      '#6a5acd', dashed).
	edge_style(implements_protocol,    '#008b8b', dotted).
	edge_style(imports_category,       '#8b4513', dotted).
	edge_style(complements_object,     '#708090', dashed).
	edge_style(provides_clauses,       '#1e90ff', dashed).
	edge_style(calls_predicate,        '#696969', solid).
	edge_style(calls_super_predicate,  '#696969', solid).
	edge_style(calls_self_predicate,   '#696969', solid).
	edge_style(updates_predicate,      '#b22222', dashed).
	edge_style(updates_this_predicate, '#b22222', dashed).
	edge_style(updates_self_predicate, '#b22222', dashed).
	edge_style(depends_on_file,        '#2f4f4f', solid).
	edge_style(loads_file,             '#228b22', solid).
	edge_style(includes_file,          '#2e8b57', dotted).
	edge_style(depends_on_directory,   '#2f4f4f', solid).
	edge_style(loads_directory,        '#228b22', solid).
	edge_style(depends_on_library,     '#2f4f4f', solid).
	edge_style(loads_library,          '#228b22', solid).
	edge_style(contains,               '#bbbbbb', dotted).
	edge_style(_,                      '#666666', solid).

	normalize_zoom_url(ZoomURL, NormalizedZoomURL) :-
		(	atom(ZoomURL),
			sub_atom(ZoomURL, _, 4, 0, '.svg') ->
			sub_atom(ZoomURL, 0, _, 4, Base),
			atom_concat(Base, '.cx2', NormalizedZoomURL)
		;	NormalizedZoomURL = ZoomURL
		).

	write_json_id(Stream, Term) :-
		write_term_to_chars(Term, Chars, [quoted(false)]),
		put_char(Stream, '"'),
		write_json_escaped_chars(Chars, Stream),
		put_char(Stream, '"').

	write_json_escaped_chars([], _).
	write_json_escaped_chars([Char| Chars], Stream) :-
		(	json_escaped_char(Char, EscChar) ->
			put_char(Stream, '\\'),
			put_char(Stream, EscChar)
		;	put_char(Stream, Char)
		),
		write_json_escaped_chars(Chars, Stream).

	json_escaped_char('"',  '"').
	json_escaped_char('\\', '\\').
	json_escaped_char('\n', 'n').
	json_escaped_char('\t', 't').
	json_escaped_char('\r', 'r').

	write_json_array([], _).
	write_json_array([Item], Stream) :-
		!,
		write_json_id(Stream, Item).
	write_json_array([Item| Items], Stream) :-
		write_json_id(Stream, Item),
		write(Stream, ', '),
		write_json_array(Items, Stream).

	write_json_edge_label([Label| Labels], Stream) :-
		put_char(Stream, '"'),
		write_json_label_chars(Label, Stream),
		write_json_label_rest(Labels, Stream),
		put_char(Stream, '"').

	write_json_label_rest([], _).
	write_json_label_rest([Label| Labels], Stream) :-
		put_char(Stream, '\\'),
		put_char(Stream, 'n'),
		write_json_label_chars(Label, Stream),
		write_json_label_rest(Labels, Stream).

	write_json_label_chars(Label, Stream) :-
		write_term_to_chars(Label, Chars, [quoted(false)]),
		write_json_escaped_chars(Chars, Stream).

:- end_object.
