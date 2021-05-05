
:- protocol(graph_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2020/04/03,
		comment is 'Description'
	]).

	:- public(add_edge/5),
	    wundgraph_add_edge/5,
	    wundgraph_add_edges/3,
	    wundgraph_del_edge/5,
	    wundgraph_del_edges/3,
	    wundgraph_del_vertex/3,
	    wundgraph_edges/2,
	    wundgraph_neighbours/3,
	    wundgraph_neighbors/3,
          	    wundgraph_wneighbours/3,
	    wundgraph_wneighbors/3,
	    wundgraph_min_tree/3,
	    wundgraph_max_tree/3]).


:- end_protocol.
