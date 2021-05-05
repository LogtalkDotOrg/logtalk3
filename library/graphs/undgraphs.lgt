/**
 * @file   undgraphs.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   2006
 *
 * @brief  Undirected Graph Processing Utilities.
 *
 *
*/

:- module( undgraphs,
	   [
	    undgraph_add_edge/4,
	    undgraph_add_edges/3,
	    undgraph_add_vertices/3,
	    undgraph_del_edge/4,
	    undgraph_del_edges/3,
	    undgraph_del_vertex/3,
	    undgraph_del_vertices/3,
	    undgraph_edges/2,
	    undgraph_neighbors/3,
	    undgraph_neighbours/3,
	    undgraph_components/2,
	    undgraph_min_tree/2]).

/** @defgroup undgraphs Undirected Graphs
@ingroup library
@{

The following graph manipulation routines use the red-black tree graph
library to implement undirected graphs. Mostly, this is done by having
two directed edges per undirected edge.

*/

:- reexport(dgraphs,
	   [
	    dgraph_new/1 as undgraph_new,
	    dgraph_add_vertex/3 as undgraph_add_vertex,
	    dgraph_vertices/2 as undgraph_vertices,
	    dgraph_complement/2 as undgraph_complement,
	    dgraph_symmetric_closure/2 as dgraph_to_undgraph,
	    dgraph_edge/3 as undgraph_edge,
	    dgraph_reachable/3 as undgraph_reachable
	    ]).


:- use_module(dgraphs,
	   [
	    dgraph_add_edge/4,
	    dgraph_add_edges/3,
	    dgraph_add_vertices/3,
	    dgraph_del_edge/4,
	    dgraph_del_edges/3,
	    dgraph_del_vertex/3,
	    dgraph_del_vertices/3,
	    dgraph_edges/2,
	    dgraph_neighbors/3,
	    dgraph_neighbours/3]).

:- use_module(wundgraphs, [
            undgraph_to_wundgraph/2,
	    wundgraph_min_tree/3,
	    wundgraph_max_tree/3,
	    wundgraph_to_undgraph/2]).

:- use_module(ordsets,
	[ ord_del_element/3,
	  ord_union/3,
	  ord_subtract/3]).

:- use_module(rbtrees,
	[  rb_delete/4,
	   rb_delete/3,
	   rb_insert/4,
	   rb_in/3,
	   rb_partial_map/4
	]).

/**

 @pred undgraph_new(+ _Graph_)

Create a new directed graph. This operation must be performed before
trying to use the graph.

*/

/** @pred undgraph_complement(+ _Graph_, - _NewGraph_)

Unify  _NewGraph_ with the graph complementary to  _Graph_.

*/

/** @pred undgraph_vertices(+ _Graph_, - _Vertices_)

Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

 */

undgraph_add_edge(Vs0,V1,V2,Vs2) :-
	dgraphs:dgraph_new_edge(V1,V2,Vs0,Vs1),
	dgraphs:dgraph_new_edge(V2,V1,Vs1,Vs2).

/** @pred undgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.


*/
undgraph_add_edges(G0, Edges, GF) :-
	dup_edges(Edges, DupEdges),
	dgraph_add_edges(G0, DupEdges, GF).

dup_edges([],[]).
dup_edges([E1-E2|Edges], [E1-E2,E2-E1|DupEdges]) :-
	dup_edges(Edges, DupEdges).

/** @pred undgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.


*/
undgraph_add_vertices(G, [], G).
undgraph_add_vertices(G0, [V|Vs], GF) :-
	dgraph_add_vertex(G0, V, GI),
	undgraph_add_vertices(GI, Vs, GF).

/** @pred undgraph_edges(+ _Graph_, - _Edges_)


Unify  _Edges_ with all edges appearing in graph
 _Graph_.


*/
undgraph_edges(Vs,Edges) :-
	dgraph_edges(Vs,DupEdges),
	remove_dups(DupEdges,Edges).

remove_dups([],[]).
remove_dups([V1-V2|DupEdges],NEdges) :- V1 @< V2, !,
	NEdges = [V1-V2|Edges],
	remove_dups(DupEdges,Edges).
remove_dups([_|DupEdges],Edges) :-
	remove_dups(DupEdges,Edges).

/** @pred undgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.


*/
undgraph_neighbours(V,Vertices,Children) :-
	dgraph_neighbours(V,Vertices,Children0),
	(
	    ord_del_element(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).
undgraph_neighbors(V,Vertices,Children) :-
	dgraph_neighbors(V,Vertices,Children0),
	(
	    ord_del_element(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).

undgraph_del_edge(Vs0,V1,V2,VsF) :-
	dgraph_del_edge(Vs0,V1,V2,Vs1),
	dgraph_del_edge(Vs1,V2,V1,VsF).

/** @pred undgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.


*/
undgraph_del_edges(G0, Edges, GF) :-
	dup_edges(Edges,DupEdges),
	dgraph_del_edges(G0, DupEdges, GF).

undgraph_del_vertex(Vs0, V, Vsf) :-
	rb_delete(Vs0, V, BackEdges, Vsi),
	(
	    ord_del_element(BackEdges,V,RealBackEdges)
	->
	    true
	;
	    BackEdges = RealBackEdges
	),
	rb_partial_map(Vsi, RealBackEdges, del_edge(V), Vsf).

/** @pred undgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.


*/
undgraph_del_vertices(G0, Vs, GF) :-
	sort(Vs,SortedVs),
	delete_all(SortedVs, [], BackEdges, G0, GI),
	ord_subtract(BackEdges, SortedVs, TrueBackEdges),
	delete_remaining_edges(SortedVs, TrueBackEdges, GI, GF).

% it would be nice to be able to delete a set of elements from an RB tree
% but I don't how to do it yet.
delete_all([], BackEdges, BackEdges) --> [].
delete_all([V|Vs], BackEdges0, BackEdgesF, Vs0,Vsf) :-
	rb_delete(Vs0, V, NewEdges, Vsi),
	ord_union(NewEdges,BackEdges0,BackEdgesI),
	delete_all(Vs, BackEdgesI ,BackEdgesF, Vsi,Vsf).

delete_remaining_edges(SortedVs, TrueBackEdges, Vs0,Vsf) :-
	rb_partial_map(Vs0, TrueBackEdges, del_edges(SortedVs), Vsf).

del_edges(ToRemove,E0,E) :-
	ord_subtract(E0,ToRemove,E).

del_edge(ToRemove,E0,E) :-
	ord_del_element(E0,ToRemove,E).

undgraph_min_tree(G, T) :-
	undgraph_to_wundgraph(G, WG),
	wundgraph_min_tree(WG, WT, _),
	wundgraph_to_undgraph(WT, T).

undgraph_max_tree(G, T) :-
	undgraph_to_wundgraph(G, WG),
	wundgraph_max_tree(WG, WT, _),
	wundgraph_to_undgraph(WT, T).

undgraph_components(Graph,[Map|Gs]) :-
	pick_node(Graph,Node,Children,Graph1), !,
	undgraph_new(Map0),
	rb_insert(Map0, Node, Children, Map1),
	expand_component(Children, Map1, Map, Graph1, NGraph),
	undgraph_components(NGraph,Gs).
undgraph_components(_,[]).

expand_component([], Map, Map, Graph, Graph).
expand_component([C|Children], Map1, Map, Graph1, NGraph) :-
	rb_delete(Graph1, C, Edges, Graph2), !,
	rb_insert(Map1, C, Edges, Map2),
	expand_component(Children, Map2, Map3, Graph2, Graph3),
	expand_component(Edges, Map3, Map, Graph3, NGraph).
expand_component([_|Children], Map1, Map, Graph1, NGraph) :-
	expand_component(Children, Map1, Map, Graph1, NGraph).


pick_node(Graph,Node,Children,Graph1) :-
	rb_in(Node,Children,Graph), !,
	rb_delete(Graph, Node, Graph1).

%% @}
