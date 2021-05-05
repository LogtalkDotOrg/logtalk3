/**
 * @file   dgraphs.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 01:23:20 2015
 *
 * @brief  Directed Graph Processing Utilities.
 *
 *
*/

:- module( dgraphs,
	   [
	    dgraph_vertices/2,
	    dgraph_edge/3,
	    dgraph_edges/2,
	    dgraph_add_vertex/3,
	    dgraph_add_vertices/3,
	    dgraph_del_vertex/3,
	    dgraph_del_vertices/3,
	    dgraph_add_edge/4,
	    dgraph_add_edges/3,
	    dgraph_del_edge/4,
	    dgraph_del_edges/3,
	    dgraph_to_ugraph/2,
	    ugraph_to_dgraph/2,
	    dgraph_neighbors/3,
	    dgraph_neighbours/3,
	    dgraph_complement/2,
	    dgraph_transpose/2,
	    dgraph_compose/3,
	    dgraph_transitive_closure/2,
	    dgraph_symmetric_closure/2,
	    dgraph_top_sort/2,
	    dgraph_top_sort/3,
	    dgraph_min_path/5,
	    dgraph_max_path/5,
	    dgraph_min_paths/3,
	    dgraph_isomorphic/4,
	    dgraph_path/3,
	    dgraph_path/4,
	    dgraph_leaves/2,
	    dgraph_reachable/3
       ]).

/** @defgroup dgraphs Directed Graphs
@ingroup library
@{

The following graph manipulation routines use the red-black tree library
to try to avoid linear-time scans of the graph for all graph
operations. Graphs are represented as a red-black tree, where the key is
the vertex, and the associated value is a list of vertices reachable
from that vertex through an edge (ie, a list of edges).

*/


/** @pred dgraph_new(+ _Graph_)


Create a new directed graph. This operation must be performed before
trying to use the graph.


*/
:- reexport(library(rbtrees),
	[rb_new/1 as dgraph_new]).

:- use_module(rbtrees,
	[%rb_new/1,
	 rb_empty/1,
	 rb_lookup/3,
	 rb_apply/4,
	 rb_insert/4,
	 rb_visit/2,
	 rb_keys/2,
	 rb_delete/3,
	 rb_map/3,
	 rb_clone/3,
	 ord_list_to_rbtree/2]).

:- use_module(ordsets,
	[ord_insert/3,
	 ord_union/3,
	 ord_subtract/3,
	 ord_del_element/3,
	 ord_memberchk/2]).

:- use_module(wdgraphs,
	[dgraph_to_wdgraph/2,
	 wdgraph_min_path/5,
	 wdgraph_max_path/5,
	 wdgraph_min_paths/3]).


/** @pred dgraph_add_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the edge
 _N1_- _N2_ to the graph  _Graph_.


*/
dgraph_add_edge(Vs0,V1,V2,Vs2) :-
	dgraph_new_edge(V1,V2,Vs0,Vs1),
	dgraph_add_vertex(Vs1,V2,Vs2).


/** @pred dgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.


*/
dgraph_add_edges(V0, Edges, VF) :-
	rb_empty(V0), !,
	sort(Edges,SortedEdges),
	all_vertices_in_edges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	edges2graphl(SortedVertices, SortedEdges, GraphL),
	ord_list_to_rbtree(GraphL, VF).
dgraph_add_edges(G0, Edges, GF) :-
	sort(Edges,SortedEdges),
	all_vertices_in_edges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	dgraph_add_edges(SortedVertices,SortedEdges, G0, GF).

all_vertices_in_edges([],[]).
all_vertices_in_edges([V1-V2|Edges],[V1,V2|Vertices]) :-
	all_vertices_in_edges(Edges,Vertices).

edges2graphl([], [], []).
edges2graphl([V|Vertices], [VV-V1|SortedEdges], [V-[V1|Children]|GraphL]) :-
	V == VV, !,
	get_extra_children(SortedEdges,VV,Children,RemEdges),
	edges2graphl(Vertices, RemEdges, GraphL).
edges2graphl([V|Vertices], SortedEdges, [V-[]|GraphL]) :-
	edges2graphl(Vertices, SortedEdges, GraphL).


dgraph_add_edges([],[]) --> [].
dgraph_add_edges([V|Vs],[V0-V1|Es]) --> { V == V0 }, !,
	{ get_extra_children(Es,V,Children,REs) },
	dgraph_update_vertex(V,[V1|Children]),
	dgraph_add_edges(Vs,REs).
dgraph_add_edges([V|Vs],Es) --> !,
	dgraph_update_vertex(V,[]),
	dgraph_add_edges(Vs,Es).

get_extra_children([V-C|Es],VV,[C|Children],REs) :- V == VV, !,
	get_extra_children(Es,VV,Children,REs).
get_extra_children(Es,_,[],Es).

dgraph_update_vertex(V,Children, Vs0, Vs) :-
	rb_apply(Vs0, V, add_edges(Children), Vs), !.
dgraph_update_vertex(V,Children, Vs0, Vs) :-
	rb_insert(Vs0,V,Children,Vs).

add_edges(E0,E1,E) :-
	ord_union(E0,E1,E).

dgraph_new_edge(V1,V2,Vs0,Vs) :-
	rb_apply(Vs0, V1, insert_edge(V2), Vs), !.
dgraph_new_edge(V1,V2,Vs0,Vs) :-
	rb_insert(Vs0,V1,[V2],Vs).

insert_edge(V2, Children0, Children) :-
	ord_insert(Children0,V2,Children).

/** @pred dgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.


*/
dgraph_add_vertices(G, [], G).
dgraph_add_vertices(G0, [V|Vs], GF) :-
	dgraph_add_vertex(G0, V, G1),
	dgraph_add_vertices(G1, Vs, GF).


/** @pred dgraph_add_vertex(+ _Graph_, + _Vertex_, - _NewGraph_)

Unify  _NewGraph_ with a new graph obtained by adding
vertex  _Vertex_ to the graph  _Graph_.


*/
dgraph_add_vertex(Vs0, V, Vs0) :-
	rb_lookup(V,_,Vs0), !.
dgraph_add_vertex(Vs0, V, Vs) :-
	rb_insert(Vs0, V, [], Vs).


/** @pred dgraph_edges(+ _Graph_, - _Edges_)


Unify  _Edges_ with all edges appearing in graph
 _Graph_.


*/
dgraph_edges(Vs,Edges) :-
	rb_visit(Vs,L0),
	cvt2edges(L0,Edges).

/** @pred dgraph_vertices(+ _Graph_, - _Vertices_)


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

*/
dgraph_vertices(Vs,Vertices) :-
	rb_keys(Vs,Vertices).

cvt2edges([],[]).
cvt2edges([V-Children|L0],Edges) :-
	children2edges(Children,V,Edges,Edges0),
	cvt2edges(L0,Edges0).

children2edges([],_,Edges,Edges).
children2edges([Child|L0],V,[V-Child|EdgesF],Edges0) :-
	children2edges(L0,V,EdgesF,Edges0).

/** @pred dgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.


*/
dgraph_neighbours(V,Vertices,Children) :-
	rb_lookup(V,Children,Vertices).

/** @pred dgraph_neighbors(+ _Vertex_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail.


*/
dgraph_neighbors(V,Vertices,Children) :-
	rb_lookup(V,Children,Vertices).

add_vertices(Graph, [], Graph).
add_vertices(Graph, [V|Vertices], NewGraph) :-
	rb_insert(Graph, V, [], IntGraph),
	add_vertices(IntGraph, Vertices, NewGraph).

/** @pred dgraph_complement(+ _Graph_, - _NewGraph_)


Unify  _NewGraph_ with the graph complementary to  _Graph_.


*/
dgraph_complement(Vs0,VsF) :-
	dgraph_vertices(Vs0,Vertices),
	rb_map(Vs0,complement(Vertices),VsF).

complement(Vs,Children,NewChildren) :-
	ord_subtract(Vs,Children,NewChildren).

/** @pred dgraph_del_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_)


Succeeds if  _NewGraph_ unifies with a new graph obtained by
removing the edge  _N1_- _N2_ from the graph  _Graph_. Notice
that no vertices are deleted.


*/
dgraph_del_edge(Vs0,V1,V2,Vs1) :-
	rb_apply(Vs0, V1, delete_edge(V2), Vs1).

/** @pred dgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.


*/
dgraph_del_edges(G0, Edges, Gf) :-
	sort(Edges,SortedEdges),
	continue_del_edges(SortedEdges, G0, Gf).

continue_del_edges([]) --> [].
continue_del_edges([V-V1|Es]) --> !,
	{ get_extra_children(Es,V,Children,REs) },
	contract_vertex(V,[V1|Children]),
	continue_del_edges(REs).

contract_vertex(V,Children, Vs0, Vs) :-
	rb_apply(Vs0, V, del_edges(Children), Vs).

del_edges(ToRemove,E0,E) :-
	ord_subtract(E0,ToRemove,E).

/** @pred dgraph_del_vertex(+ _Graph_, + _Vertex_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by deleting vertex
 _Vertex_ and all the edges that start from or go to  _Vertex_ to
the graph  _Graph_.


*/
dgraph_del_vertex(Vs0, V, Vsf) :-
	rb_delete(Vs0, V, Vs1),
	rb_map(Vs1, delete_edge(V), Vsf).

delete_edge(Edges0, V, Edges) :-
	ord_del_element(Edges0, V, Edges).

/** @pred dgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.


*/
dgraph_del_vertices(G0, Vs, GF) :-
	sort(Vs,SortedVs),
	delete_all(SortedVs, G0, G1),
	delete_remaining_edges(SortedVs, G1, GF).

% it would be nice to be able to delete a set of elements from an RB tree
% but I don't how to do it yet.
delete_all([]) --> [].
delete_all([V|Vs],Vs0,Vsf) :-
	rb_delete(Vs0, V, Vsi),
	delete_all(Vs,Vsi,Vsf).

delete_remaining_edges(SortedVs,Vs0,Vsf) :-
	rb_map(Vs0, del_edges(SortedVs), Vsf).

/** @pred dgraph_transpose(+ _Graph_, - _Transpose_)


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_.


*/
dgraph_transpose(Graph, TGraph) :-
	rb_visit(Graph, Edges),
	transpose(Edges, Nodes, TEdges, []),
	dgraph_new(G0),
	% make sure we have all vertices, even if they are unconnected.
	dgraph_add_vertices(G0, Nodes, G1),
	dgraph_add_edges(G1, TEdges, TGraph).

transpose([], []) --> [].
transpose([V-Edges|MoreVs], [V|Vs]) -->
	transpose_edges(Edges, V),
	transpose(MoreVs, Vs).

transpose_edges([], _V) --> [].
transpose_edges(E.Edges, V) -->
	[E-V],
	transpose_edges(Edges, V).

dgraph_compose(T1,T2,CT) :-
	rb_visit(T1,Nodes),
	compose(Nodes,T2,NewNodes),
	dgraph_new(CT0),
	dgraph_add_edges(CT0,NewNodes,CT).

compose([],_,[]).
compose([V-Children|Nodes],T2,NewNodes) :-
	compose2(Children,V,T2,NewNodes,NewNodes0),
	compose(Nodes,T2,NewNodes0).

compose2([],_,_,NewNodes,NewNodes).
compose2([C|Children],V,T2,NewNodes,NewNodes0) :-
	rb_lookup(C, GrandChildren, T2),
	compose3(GrandChildren, V, NewNodes,NewNodesI),
	compose2(Children,V,T2,NewNodesI,NewNodes0).

compose3([], _, NewNodes, NewNodes).
compose3([GC|GrandChildren], V, [V-GC|NewNodes], NewNodes0) :-
	compose3(GrandChildren, V, NewNodes, NewNodes0).

/** @pred dgraph_transitive_closure(+ _Graph_, - _Closure_)


Unify  _Closure_ with the transitive closure of graph  _Graph_.


*/
dgraph_transitive_closure(G,Closure) :-
	dgraph_edges(G,Edges),
	continue_closure(Edges,G,Closure).

continue_closure([], Closure, Closure) :- !.
continue_closure(Edges, G, Closure) :-
	transit_graph(Edges,G,NewEdges),
	dgraph_add_edges(G, NewEdges, GN),
	continue_closure(NewEdges, GN, Closure).

transit_graph([],_,[]).
transit_graph([V-V1|Edges],G,NewEdges) :-
	rb_lookup(V1, GrandChildren, G),
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges),
	transit_graph(Edges, G, MoreEdges).

transit_graph2([], _, _, NewEdges, NewEdges).
transit_graph2([GC|GrandChildren], V, G, NewEdges, MoreEdges) :-
	is_edge(V,GC,G), !,
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges).
transit_graph2([GC|GrandChildren], V, G, [V-GC|NewEdges], MoreEdges) :-
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges).

is_edge(V1,V2,G) :-
	rb_lookup(V1,Children,G),
	ord_memberchk(V2, Children).

/** @pred dgraph_symmetric_closure(+ _Graph_, - _Closure_)


Unify  _Closure_ with the symmetric closure of graph  _Graph_,
that is,  if  _Closure_ contains an edge  _U-V_ it must also
contain the edge  _V-U_.


*/
dgraph_symmetric_closure(G,S) :-
	dgraph_edges(G, Edges),
	invert_edges(Edges, InvertedEdges),
	dgraph_add_edges(G, InvertedEdges, S).

invert_edges([], []).
invert_edges([V1-V2|Edges], [V2-V1|InvertedEdges]) :-
	invert_edges(Edges, InvertedEdges).

/** @pred dgraph_top_sort(+ _Graph_, - _Vertices_)


Unify  _Vertices_ with the topological sort of graph  _Graph_.


*/
dgraph_top_sort(G, Q) :-
	dgraph_top_sort(G, Q, []).

/** @pred dgraph_top_sort(+ _Graph_, - _Vertices_, ? _Vertices0_)

Unify the difference list  _Vertices_- _Vertices0_ with the
topological sort of graph  _Graph_.


*/
dgraph_top_sort(G, Q, RQ0) :-
	% O(E)
	rb_visit(G, Vs),
	% O(E)
	invert_and_link(Vs, Links, UnsortedInvertedEdges, AllVs, Q),
	% O(V)
	rb_clone(G, LinkedG, Links),
	% O(Elog(E))
	sort(UnsortedInvertedEdges, InvertedEdges),
	% O(E)
	dgraph_vertices(G, AllVs),
	start_queue(AllVs, InvertedEdges, Q, RQ),
	continue_queue(Q, LinkedG, RQ, RQ0).

invert_and_link([], [], [], [], []).
invert_and_link([V-Vs|Edges], [V-NVs|ExtraEdges], UnsortedInvertedEdges, [V|AllVs],[_|Q]) :-
	inv_links(Vs, NVs, V, UnsortedInvertedEdges, UnsortedInvertedEdges0),
	invert_and_link(Edges, ExtraEdges, UnsortedInvertedEdges0, AllVs, Q).

inv_links([],[],_,UnsortedInvertedEdges,UnsortedInvertedEdges).
inv_links([V2|Vs],[l(V2,A,B,S,E)|VLnks],V1,[V2-e(A,B,S,E)|UnsortedInvertedEdges],UnsortedInvertedEdges0) :-
	inv_links(Vs,VLnks,V1,UnsortedInvertedEdges,UnsortedInvertedEdges0).

dup([], []).
dup([_|AllVs], [_|Q]) :-
	dup(AllVs, Q).

start_queue([], [], RQ, RQ).
start_queue([V|AllVs], [VV-e(S,B,S,E)|InvertedEdges], Q, RQ) :- V == VV, !,
	link_edges(InvertedEdges, VV, B, S, E, RemainingEdges),
	start_queue(AllVs, RemainingEdges, Q, RQ).
start_queue([V|AllVs], InvertedEdges, [V|Q], RQ) :-
	start_queue(AllVs, InvertedEdges, Q, RQ).

link_edges([V-e(A,B,S,E)|InvertedEdges], VV, A, S, E, RemEdges) :- V == VV, !,
	link_edges(InvertedEdges, VV, B, S, E, RemEdges).
link_edges(RemEdges, _, A, _, A, RemEdges).

continue_queue([], _, RQ0, RQ0).
continue_queue([V|Q], LinkedG, RQ, RQ0) :-
	rb_lookup(V, Links, LinkedG),
	close_links(Links, RQ, RQI),
	% not clear whether I should deleted V from LinkedG
	continue_queue(Q, LinkedG, RQI, RQ0).

close_links([], RQ, RQ).
close_links([l(V,A,A,S,E)|Links], RQ, RQ0) :-
	( S == E -> RQ = [V| RQ1] ; RQ = RQ1),
	close_links(Links, RQ1, RQ0).

/** @pred ugraph_to_dgraph( + _UGraph_, - _Graph_)


Unify  _Graph_ with the directed graph obtain from  _UGraph_,
represented in the form used in the  _ugraphs_ unweighted graphs
library.

*/
ugraph_to_dgraph(UG, DG) :-
	ord_list_to_rbtree(UG, DG).

/** @pred dgraph_to_ugraph(+ _Graph_, - _UGraph_)


Unify  _UGraph_ with the representation used by the  _ugraphs_
unweighted graphs library, that is, a list of the form
 _V-Neighbors_, where  _V_ is a node and  _Neighbors_ the nodes
children.

*/
dgraph_to_ugraph(DG, UG) :-
	rb_visit(DG, UG).

/** @pred dgraph_edge(+ _N1_, + _N2_, + _Graph_)


Edge  _N1_- _N2_ is an edge in directed graph  _Graph_.


*/
dgraph_edge(N1, N2, G) :-
	rb_lookup(N1, Ns, G),
	ord_memberchk(N2, Ns).

/** @pred dgraph_min_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_)


Unify the list  _Path_ with the minimal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.


*/
dgraph_min_path(V1, V2, Graph, Path, Cost) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_min_path(V1, V2, WGraph, Path, Cost).

/** @pred dgraph_max_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_)


Unify the list  _Path_ with the maximal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.


*/
dgraph_max_path(V1, V2, Graph, Path, Cost) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_max_path(V1, V2, WGraph, Path, Cost).

/** @pred dgraph_min_paths(+ _V1_, + _Graph_, - _Paths_)


Unify the list  _Paths_ with the minimal cost paths from node
 _N1_ to the nodes in graph  _Graph_.


*/
dgraph_min_paths(V1, Graph, Paths) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_min_paths(V1, WGraph, Paths).

/** @pred dgraph_path(+ _Vertex_, + _Vertex1_, + _Graph_, ? _Path_)

The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_ and ending at path  _Vertex2_.


*/
dgraph_path(V1, V2, Graph, Path) :-
	rb_new(E0),
	rb_lookup(V1, Children, Graph),
	dgraph_path_children(Children, V2, E0, Graph, Path).

dgraph_path_children([V1|_], V2, _E1, _Graph, []) :- V1 == V2.
dgraph_path_children([V1|_], V2, E1, Graph, [V1|Path]) :-
	V2 \== V1,
	\+ rb_lookup(V1, _, E0),
	rb_insert(E0, V2, [], E1),
	rb_lookup(V1, Children, Graph),
	dgraph_path_children(Children, V2, E1, Graph, Path).
dgraph_path_children([_|Children], V2, E1, Graph, Path) :-
	dgraph_path_children(Children, V2, E1, Graph, Path).


do_path([], _, _, []).
do_path([C|Children], G, SoFar, Path) :-
	do_children([C|Children], G, SoFar, Path).

do_children([V|_], G, SoFar, [V|Path]) :-
	rb_lookup(V, Children, G),
	ord_subtract(Children, SoFar, Ch),
	ord_insert(SoFar, V, NextSoFar),
	do_path(Ch, G, NextSoFar, Path).
do_children([_|Children], G, SoFar, Path) :-
	do_children(Children, G, SoFar, Path).

/** @pred dgraph_path(+ _Vertex_, + _Graph_, ? _Path_)


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.


*/
dgraph_path(V, G, [V|P]) :-
	rb_lookup(V, Children, G),
	ord_del_element(Children, V, Ch),
	do_path(Ch, G, [V], P).


/** @pred dgraph_isomorphic(+ _Vs_, + _NewVs_, + _G0_, - _GF_)


Unify the list  _GF_ with the graph isomorphic to  _G0_ where
vertices in  _Vs_ map to vertices in  _NewVs_.


*/
dgraph_isomorphic(Vs, Vs2, G1, G2) :-
	rb_new(Map0),
	mapping(Vs,Vs2,Map0,Map),
	dgraph_edges(G1,Edges),
	translate_edges(Edges,Map,TEdges),
	dgraph_new(G20),
	dgraph_add_vertices(Vs2,G20,G21),
	dgraph_add_edges(G21,TEdges,G2).

mapping([],[],Map,Map).
mapping([V1|Vs],[V2|Vs2],Map0,Map) :-
	rb_insert(Map0,V1,V2,MapI),
	mapping(Vs,Vs2,MapI,Map).



translate_edges([],_,[]).
translate_edges([V1-V2|Edges],Map,[NV1-NV2|TEdges]) :-
	rb_lookup(V1,NV1,Map),
	rb_lookup(V2,NV2,Map),
	translate_edges(Edges,Map,TEdges).

/** @pred dgraph_reachable(+ _Vertex_, + _Graph_, ? _Edges_)


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.


*/
dgraph_reachable(V, G, Edges) :-
	rb_lookup(V, Children, G),
	ord_list_to_rbtree([V-[]],Done0),
	reachable(Children, Done0, _, G, Edges, []).

reachable([], Done, Done, _, Edges, Edges).
reachable([V|Vertices], Done0, DoneF, G, EdgesF, Edges0) :-
	rb_lookup(V,_, Done0), !,
	reachable(Vertices, Done0, DoneF, G, EdgesF, Edges0).
reachable([V|Vertices], Done0, DoneF, G, [V|EdgesF], Edges0) :-
	rb_lookup(V, Kids, G),
	rb_insert(Done0, V, [], Done1),
	reachable(Kids, Done1, DoneI, G, EdgesF, EdgesI),
	reachable(Vertices, DoneI, DoneF, G, EdgesI, Edges0).

/** @pred dgraph_leaves(+ _Graph_, ? _Vertices_)


The vertices  _Vertices_ have no outgoing edge in graph
 _Graph_.


 */
dgraph_leaves(Graph, Vertices) :-
	rb_visit(Graph, Pairs),
	vertices_without_children(Pairs, Vertices).

vertices_without_children([], []).
vertices_without_children((V-[]).Pairs, V.Vertices) :-
	vertices_without_children(Pairs, Vertices).
vertices_without_children(_V-[_|_].Pairs, Vertices) :-
	vertices_without_children(Pairs, Vertices).

%% @}/** @} */

