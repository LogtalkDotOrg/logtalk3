/**
 * @file   wgraphs.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   2006
 *
 *
*/

/**************************************

SICStus compatible wgraphs.yap

**************************************/

:- module( wgraphs,
	   [vertices_edges_to_wgraph/3]
	 ).

/**
 * @defgroup wgraphs Weighted Graphs
 * @ingroup library
 * @brief  Weighted  Graph Processing Utilities.
 *
 * @{
*/


:- reexport(wdgraphs,
	    [wdgraph_vertices/2 as vertices,
	     wdgraph_edges/2 as edges,
	     wdgraph_to_dgraph/2 as wgraph_to_ugraph,
	     dgraph_to_wdgraph/2 as ugraph_to_wgraph,
	     wdgraph_add_vertices/3 as add_vertices,
	     wdgraph_del_vertices/3 as del_vertices,
	     wdgraph_add_edges/3 as add_edges,
	     wdgraph_del_edges/3 as del_edges,
	     wdgraph_transpose/2 as transpose,
	     wdgraph_neighbors/3 as neighbors,
	     wdgraph_neighbours/3 as neighbours,
	     wdgraph_transitive_closure/2 as transitive_closure,
	     wdgraph_symmetric_closure/2 as symmetric_closure,
	     wdgraph_top_sort/2 as top_sort,
	     wdgraph_max_path/5 as max_path,
	     wdgraph_min_path/5 as min_path,
	     wdgraph_min_paths/3 as min_paths,
	     wdgraph_path/3 as path]).

:- reexport(wundgraphs,
	    [wundgraph_min_tree/3 as min_tree]).

:- use_module(wdgraphs,
	      [wdgraph_new/1,
	       wdgraph_add_vertices_and_edges/4]).

vertices_edges_to_wgraph(Vertices, Edges, Graph) :-
	wdgraph_new(G0),
	wdgraph_add_vertices_and_edges(G0, Vertices, Edges, Graph).


%% @}

