

:- object(json_graph).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2019-02-18,
		comment is 'Predicates for exporting graphs in the JSON graph format.'
	]).

/*
	graph(Directed, Type, Label, Metadata).
	node(Id, Type, Label, Metadata).
	edge(Source, Relation, Target, Directed, Label, Metadata).

	graph(GraphId, Directed, Type, Label, Metadata).
	node(GraphId, Id, Type, Label, Metadata).
	edge(GraphId, Source, Relation, Target, Directed, Label, Metadata).
*/

	:- public(graph_to_string/2).
	:- mode(graph_to_string(+object_identifier, -list(character_code)), one).
	:- info(graph_to_string/2, [
		comment is 'Returns a JSON graph representation, as a string, of the graph hold by the data object',
		arguments is [
			'DataObject' - 'Object defining the graph predicates graph/4, node/4, and edge/6.',
			'String' - 'String representatino of the JSON graph.'
		]
	]).

	:- public(write_graph_to_stream/2).
	:- mode(write_graph_to_stream(+object_identifier, +stream_or_alias), one).
	:- info(write_graph_to_stream/2, [
		comment is 'Writes to the given stream the JSON graph representation of the graph hold by the data object',
		arguments is [
			'DataObject' - 'Object defining the graph predicates graph/4, node/4, and edge/6.',
			'Stream' - 'Output stream.'
		]
	]).

	:- public(graphs_to_string/2).
	:- mode(graphs_to_string(+object_identifier, -list(character_code)), one).
	:- info(graphs_to_string/2, [
		comment is 'Returns a JSON graph representation, as a string, of the graphs hold by the data object',
		arguments is [
			'DataObject' - 'Object defining the graphs predicates graph/5, node/5, and edge/7.',
			'String' - 'String representatino of the JSON graph.'
		]
	]).

	:- public(write_graphs_to_stream/2).
	:- mode(write_graphs_to_stream(+object_identifier, +stream_or_alias), one).
	:- info(write_graphs_to_stream/2, [
		comment is 'Description',
		arguments is [
			'DataObject' - 'Object defining the graphs predicates graph/5, node/5, and edge/7.',
			'Stream' - 'Output stream.'
		]
	]).

	graph_to_string(DataObject, String) :-
		output_graph(DataObject, Graph),
		json::encode(Graph, String).

	write_graph_to_stream(DataObject, Stream) :-
		graph_to_string(DataObject, String),
		write_codes(String, Stream).

	output_graph(DataObject, {graph:{Graph}}) :-
		DataObject::graph(Directed, Type, Label, Metadata),
		Graph = (
			directed:Directed,
			type:Type,
			label:Label,
			metadata:{Metadata},
			nodes:Nodes,
			edges:Edges
		),
		output_graph_nodes(DataObject, Nodes),
		output_graph_edges(DataObject, Edges).

	output_graph_nodes(DataObject, Nodes) :-
		findall(
			{id:Id, type:Type, label:Label, metadata:{Metadata}},
			DataObject::node(Id, Type, Label, Metadata),
			Nodes
		).

	output_graph_edges(DataObject, Edges) :-
		findall(
			{source:Source, relation:Relation, target:Target, directed:Directed, label:Label, metadata:{Metadata}},
			DataObject::edge(Source, Relation, Target, Directed, Label, Metadata),
			Edges
		).

	graphs_to_string(DataObject, String) :-
		output_graphs(DataObject, Graphs),
		json::encode(Graphs, String).

	write_graphs_to_stream(DataObject, Stream) :-
		graphs_to_string(DataObject, String),
		write_codes(String, Stream).

	output_graphs(DataObject, {graphs:Graphs}) :-
		findall(
			Graph,
			output_graphs_graph(DataObject, Graph),
			Graphs
		).

	output_graphs_graph(DataObject, {Graph}) :-
		DataObject::graph(GraphId, Directed, Type, Label, Metadata),
		Graph = (
			directed:Directed,
			type:Type,
			label:Label,
			metadata:{Metadata},
			nodes:Nodes,
			edges:Edges
		),
		output_graph_nodes(DataObject, GraphId, Nodes),
		output_graph_edges(DataObject, GraphId, Edges).

	output_graph_nodes(DataObject, GraphId, Nodes) :-
		findall(
			{id:Id, type:Type, label:Label, metadata:{Metadata}},
			DataObject::node(GraphId, Id, Type, Label, Metadata),
			Nodes
		).

	output_graph_edges(DataObject, GraphId, Edges) :-
		findall(
			{source:Source, relation:Relation, target:Target, directed:Directed, label:Label, metadata:{Metadata}},
			DataObject::edge(GraphId, Source, Relation, Target, Directed, Label, Metadata),
			Edges
		).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

:- end_object.
