/**
 * @file   wdgraphs.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   2006
 *
 *
*/

:- module( wdgraphs,
	   [
	    wdgraph_new/1,
	    wdgraph_add_edge/5,
	    wdgraph_add_edges/3,
	    wdgraph_add_vertices_and_edges/4,
	    wdgraph_del_edge/5,
	    wdgraph_del_edges/3,
	    wdgraph_del_vertex/3,
	    wdgraph_del_vertices/3,
	    wdgraph_edge/4,
	    wdgraph_to_dgraph/2,
	    dgraph_to_wdgraph/2,
	    wdgraph_neighbors/3,
	    wdgraph_neighbours/3,
	    wdgraph_wneighbors/3,
	    wdgraph_wneighbours/3,
	    wdgraph_transpose/2,
	    wdgraph_transitive_closure/2,
	    wdgraph_symmetric_closure/2,
	    wdgraph_top_sort/2,
	    wdgraph_min_path/5,
	    wdgraph_min_paths/3,
	    wdgraph_max_path/5,
	    wdgraph_path/3,
	    wdgraph_reachable/3]).

/**
 * @defgroup wdgraphs Weighted Directed Graphs
 * @ingroup library
 *
 * @brief  Weighted Directed Graph Processing Utilities.
 *
 * @{
 *
 */


:- reexport(dgraphs,
	    [dgraph_add_vertex/3 as wdgraph_add_vertex,
	     dgraph_add_vertices/3 as wdgraph_add_vertices,
	     dgraph_vertices/2 as wdgraph_vertices,
	     dgraph_edges/2 as wdgraph_edges
	    ]).


:- use_module(dgraphs,
	[
	dgraph_top_sort/2,
	dgraph_path/3
	]
    ).

:- use_module(rbtrees,
	[rb_new/1,
	 rb_empty/1,
	 rb_lookup/3,
	 rb_apply/4,
	 rb_insert/4,
	 rb_visit/2,
	 rb_keys/2,
	 rb_delete/3,
	 rb_map/3,
	 rb_clone/3,
	 rb_clone/4,
	 rb_update/5,
	 ord_list_to_rbtree/2]).

:- use_module(ordsets,
	[ord_insert/3]).

:- use_module(heaps,
	[
         empty_heap/1,
	 add_to_heap/4,
	 get_from_heap/4
     ]).

wdgraph_new(Vertices) :-
	rb_new(Vertices).

wdgraph_add_vertices_and_edges(Vs0,Vertices,Edges,Vs2) :-
	wdgraph_add_vertices(Vs0, Vertices, Vs1),
	wdgraph_add_edges(Vs1, Edges, Vs2).


wdgraph_add_edge(Vs0,V1,V2,Weight,Vs2) :-
	wdgraph_new_edge(V1,V2,Weight,Vs0,Vs1),
	dgraph_add_vertex(Vs1,V2,Vs2).

wdgraph_add_edges(V0, Edges, VF) :-
	rb_empty(V0), !,
	sort(Edges,SortedEdges),
	all_vertices_in_wedges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	edges2wgraphl(SortedVertices, SortedEdges, GraphL),
	ord_list_to_rbtree(GraphL, VF).
wdgraph_add_edges(G0, Edges, GF) :-
	sort(Edges,SortedEdges),
	all_vertices_in_wedges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	add_edges(SortedVertices,SortedEdges, G0, GF).

all_vertices_in_wedges([],[]).
all_vertices_in_wedges([V1-(V2-_)|Edges],[V1,V2|Vertices]) :-
	all_vertices_in_wedges(Edges,Vertices).

edges2wgraphl([], [], []).
edges2wgraphl([V|Vertices], [V-(V1-W)|SortedEdges], [V-[V1-W|Children]|GraphL]) :- !,
	get_extra_children(SortedEdges,V,Children,RemEdges),
	edges2wgraphl(Vertices, RemEdges, GraphL).
edges2wgraphl([V|Vertices], SortedEdges, [V-[]|GraphL]) :-
	edges2wgraphl(Vertices, SortedEdges, GraphL).


add_edges([],[]) --> [].
add_edges([VA|Vs],[VB-(V1-W)|Es]) --> { VA == VB }, !,
	{ get_extra_children(Es,VA,Children,REs) },
	wdgraph_update_vertex(VA,[V1-W|Children]),
	add_edges(Vs,REs).
add_edges([V|Vs],Es) --> !,
	wdgraph_update_vertex(V,[]),
	add_edges(Vs,Es).

get_extra_children([VA-(C-W)|Es],VB,[C-W|Children],REs) :- VA == VB, !,
	get_extra_children(Es,VB,Children,REs).
get_extra_children(Es,_,[],Es).


wdgraph_update_vertex(V,Edges,WG0,WGF) :-
	rb_update(WG0, V, Edges0, EdgesF, WGF), !,
	key_union(Edges, Edges0, EdgesF).
wdgraph_update_vertex(V,Edges,WG0,WGF) :-
	rb_insert(WG0, V, Edges, WGF).

key_union([], [], []) :- !.
key_union([], [C|Children], [C|Children]).
key_union([C|Children], [], [C|Children]) :- !.
key_union([K-W|ToAdd], [K1-W1|Children0], NewUnion) :-
	( K == K1 ->
	    NewUnion = [K-W|NewChildren],
	    key_union(ToAdd, Children0, NewChildren)
	;
	    K1 @< K ->
	    NewUnion = [K1-W1|NewChildren],
	    key_union([K-W|ToAdd], Children0, NewChildren)
	;
	    NewUnion = [K-W|NewChildren],
	    key_union(ToAdd, [K1-W1|Children0], NewChildren)
	).

wdgraph_new_edge(V1,V2,W,Vs0,Vs) :-
	rb_apply(Vs0, V1, insert_edge(V2,W), Vs), !.
wdgraph_new_edge(V1,V2,W,Vs0,Vs) :-
	rb_insert(Vs0,V1,[V2-W],Vs).

insert_edge(V2, W, Children0, Children) :-
	ord_insert(Children0,V2-W,Children).

wdgraph_top_sort(WG,Q) :-
	wdgraph_to_dgraph(WG, G),
	dgraph_top_sort(G, Q).

wgraph_to_wdgraph(UG, DG) :-
	ord_list_to_rbtree(UG, DG).

wdgraph_to_wgraph(DG, UG) :-
	rb_visit(DG, UG).

wdgraph_edge(N1, N2, W, G) :-
	rb_lookup(N1, Ns, G),
	find_edge(N2-W, Ns).

find_edge(N-W,[N1-W|_]) :- N == N1, !.
find_edge(El,[_|Edges]) :-
	find_edge(El,Edges).

wdgraph_del_edge(Vs0, V1, V2, W, Vs) :-
	rb_update(Vs0, V1, Children0, NewChildren, Vs),
	del_edge(Children0, V2, W, NewChildren).

% I assume first argument is subset of second.
del_edge([K-W|Children], K1, W1, NewChildren) :-
	( K == K1 ->
	    W = W1,
	    Children = NewChildren
	;
	    % K1 @< K
	    NewChildren = [K-W|ChildrenLeft],
	    del_edge(Children, K1, W1, ChildrenLeft)
	).

wdgraph_del_edges(G0, Edges, GF) :-
	sort(Edges,SortedEdges),
	continue_del_edges(SortedEdges, G0, GF).

continue_del_edges([]) --> [].
continue_del_edges([V-V1|Es]) --> !,
	{ get_extra_children(Es,V,Children,REs) },
	contract_vertex(V,[V1|Children]),
	continue_del_edges(REs).

contract_vertex(V,Children, Vs0, Vs) :-
	rb_update(Vs0, V, Children0, NewChildren, Vs),
	del_vertices(Children, Children0, NewChildren).

% I assume first argument is subset of second.
del_vertices(Children, [], Children).
del_vertices([K1-W1|Children0], [K-W|ToDel], NewChildren) :-
	( K == K1 ->
	    W = W1,
	    del_vertices(Children0, ToDel, NewChildren)
	;
	    % K1 @< K
	    NewChildren = [K1-W1|ChildrenLeft],
	    del_vertices(Children0, [K-W|ToDel], ChildrenLeft)
	).

wdgraph_del_vertex(Vs0, V, Vsf) :-
	rb_delete(Vs0, V, Vs1),
	rb_map(Vs1, delete_wedge(V), Vsf).

delete_wedge(_, [], []).
delete_wedge(V, [K-W|Children], NewChildren) :-
	( K == V ->
	    NewChildren = Children
	;
	    K @< V ->
	    NewChildren = [K-W|Children2],
	    delete_wedge(V, Children, Children2)
	;
	   Children = NewChildren
	).

wdgraph_del_vertices(G0, Vs, GF) :-
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
	rb_map(Vs0, del_possible_edges(SortedVs), Vsf).

del_possible_edges([], [], []).
del_possible_edges([], [C|Children], [C|Children]).
del_possible_edges([_|_], [], []).
del_possible_edges([K|ToDel], [K1-W1|Children0], NewChildren) :-
	( K == K1 ->
	    del_possible_edges(ToDel, Children0, NewChildren)
	;
	    K1 @< K ->
	    NewChildren = [K1-W1|ChildrenLeft],
	    del_possible_edges([K|ToDel], Children0, ChildrenLeft)
	;
	    del_possible_edges(ToDel, [K1-W1|Children0], NewChildren)
	).

wdgraph_to_dgraph(WG, DG) :-
	rb_clone(WG, EdgesList0, DG, EdgeList),
	cvt_wedges(EdgesList0, EdgeList).

cvt_wedges([], []).
cvt_wedges([V-WEs|EdgesList0], [V-Es|EdgesList]) :-
	cvt_wneighbs(WEs, Es),
	cvt_wedges(EdgesList0, EdgesList).

cvt_wneighbs([], []).
cvt_wneighbs([V-_|WEs], [V|Es]) :-
	cvt_wneighbs(WEs, Es).

dgraph_to_wdgraph(DG, WG) :-
	rb_clone(DG, EdgesList0, WG, EdgesList),
	cvt_edges(EdgesList0, EdgesList).

cvt_edges([], []).
cvt_edges([V-Es|EdgesList0], [V-WEs|WEdgeList]) :-
	cvt_neighbs(Es, WEs),
	cvt_edges(EdgesList0, WEdgeList).

cvt_neighbs([], []).
cvt_neighbs([V|WEs], [V-1|Es]) :-
	cvt_neighbs(WEs, Es).

wdgraph_neighbors(V, WG, Neighbors) :-
	rb_lookup(V, EdgesList0, WG),
	cvt_wneighbs(EdgesList0, Neighbors).

wdgraph_neighbours(V, WG, Neighbors) :-
	rb_lookup(V, EdgesList0, WG),
	cvt_wneighbs(EdgesList0, Neighbors).

wdgraph_wneighbors(V, WG, Neighbors) :-
	rb_lookup(V, Neighbors, WG).

wdgraph_wneighbours(V, WG, Neighbors) :-
	rb_lookup(V, Neighbors, WG).

wdgraph_transpose(Graph, TGraph) :-
	rb_visit(Graph, Edges),
	rb_clone(Graph, TGraph, NewNodes),
	wtedges(Edges,UnsortedTEdges),
	sort(UnsortedTEdges,TEdges),
	fill_nodes(NewNodes,TEdges).

wtedges([],[]).
wtedges([V-Vs|Edges],TEdges) :-
	fill_wtedges(Vs, V, TEdges, TEdges0),
	wtedges(Edges,TEdges0).

fill_wtedges([], _, TEdges, TEdges).
fill_wtedges([V1-W|Vs], V, [V1-(V-W)|TEdges], TEdges0) :-
	fill_wtedges(Vs, V, TEdges, TEdges0).


fill_nodes([],[]).
fill_nodes([V-[Child|MoreChildren]|Nodes],[V-Child|Edges]) :- !,
	get_extra_children(Edges,V,MoreChildren,REdges),
	fill_nodes(Nodes,REdges).
fill_nodes([_-[]|Edges],TEdges) :-
	fill_nodes(Edges,TEdges).

wdgraph_transitive_closure(G,Closure) :-
	dgraph_edges(G,Edges),
	continue_closure(Edges,G,Closure).

continue_closure([], Closure, Closure) :- !.
continue_closure(Edges, G, Closure) :-
	transit_wgraph(Edges,G,NewEdges),
	wdgraph_add_edges(G, NewEdges, GN),
	continue_closure(NewEdges, GN, Closure).

transit_wgraph([],_,[]).
transit_wgraph([V-(V1-W)|Edges],G,NewEdges) :-
	rb_lookup(V1, GrandChildren, G),
	transit_wgraph2(GrandChildren, V, W, G, NewEdges, MoreEdges),
	transit_wgraph(Edges, G, MoreEdges).

transit_wgraph2([], _, _, _, NewEdges, NewEdges).
transit_wgraph2([GC|GrandChildren], V, W, G, NewEdges, MoreEdges) :-
	is_edge(V,GC,G), !,
	transit_wgraph2(GrandChildren, V, W, G, NewEdges, MoreEdges).
transit_wgraph2([GC-W1|GrandChildren], V, W2, G, [V-(GC-W)|NewEdges], MoreEdges) :-
	W is W1+W2,
	transit_wgraph2(GrandChildren, V, W2, G, NewEdges, MoreEdges).

is_edge(V1,V2,G) :-
	rb_lookup(V1,Children,G),
	find_edge(V2-_, Children).

wdgraph_symmetric_closure(G,S) :-
	dgraph_edges(G, WEdges),
	invert_wedges(WEdges, InvertedWEdges),
	wdgraph_add_edges(G, InvertedWEdges, S).

invert_wedges([], []).
invert_wedges([V1-(V2-W)|WEdges], [V2-(V1-W)|InvertedWEdges]) :-
	invert_wedges(WEdges, InvertedWEdges).

wdgraph_min_path(V1, V2, WGraph, Path, Cost) :-
	rb_new(Status0),
	rb_lookup(V1, Edges, WGraph),
	rb_insert(Status0, V1, V2, Status),
	empty_heap(H0),
	queue_edges(Edges, V1, 0, H0, H1),
	dijkstra(H1, V2, WGraph, Status, [], EPath),
	backtrace(EPath, V2, [V2], Path, 0, Cost).

wdgraph_max_path(V1, V2, WGraph0, Path, Cost) :-
	rb_clone(WGraph0, Edges0, WGraph, Edges),
	inv_costs(Edges0, Edges),
	wdgraph_min_path(V1, V2, WGraph, Path, NCost),
	Cost is -NCost.

inv_costs([], []).
inv_costs([V-Es|Edges0], [V-NEs|Edges]) :-
	inv_costs2(Es,NEs),
	inv_costs(Edges0, Edges).

inv_costs2([],[]).
inv_costs2([V-E|Es],[V-NE|NEs]) :-
	NE is -E,
	inv_costs2(Es,NEs).

queue_edges([], _, _, H, H).
queue_edges([V-W|Edges], V0, D0, H, NH) :-
	D is W+D0,
	add_to_heap(H, D, e(V0,V,W), HI),
	queue_edges(Edges, V0, D0, HI, NH).

dijkstra(H0, V2, WGraph, Status, Path0, PathF) :-
	get_from_heap(H0, D, e(V0, V, W), H1),
	continue_dijkstra(H1, V2, WGraph, Status, Path0, PathF, D, V0, V, W).

continue_dijkstra(_, V2, _, _, Path0, [e(V0,V2,W)|Path0], _, V0, V, W) :- V == V2, !.
continue_dijkstra(H1, V2, WGraph, Status, Path0, PathF, _, _, V, _) :-
	rb_lookup(V, _, Status), !,
	% pick some other node.
	dijkstra(H1, V2, WGraph, Status, Path0, PathF).
continue_dijkstra(H1, V2, WGraph, Status0, Path0, PathF, D, V0, V, W) :-
	rb_insert(Status0, V, V0, Status),
	rb_lookup(V, Edges, WGraph),
	queue_edges(Edges, V, D, H1, H2),
	dijkstra(H2, V2, WGraph, Status, [e(V0,V,W)|Path0], PathF).


backtrace([], _, Path, Path, Cost, Cost).
backtrace([e(V0,V,C)|EPath], V1, Path0, Path, Cost0, Cost) :-
	V == V1, !,
	CostI is C+Cost0,
	backtrace(EPath, V0, [V0|Path0], Path, CostI, Cost).
backtrace([_|EPath], V1, Path0, Path, Cost0, Cost) :-
	backtrace(EPath, V1, Path0, Path, Cost0, Cost).


wdgraph_min_paths(V1, WGraph, T) :-
	rb_new(Status0),
	rb_lookup(V1, Edges, WGraph),
	rb_insert(Status0, V1, V1, Status),
	empty_heap(H0),
	queue_edges(Edges, V1, 0, H0, H1),
	dijkstra(H1, WGraph, Status, [], EPath),
	rb_empty(T0),
	wdgraph_add_edges(T0, EPath, T).


dijkstra(H0, WGraph, Status, Path0, PathF) :-
	get_from_heap(H0, D, e(V0, V, W), H1), !,
	continue_dijkstra(H1, WGraph, Status, Path0, PathF, D, V0, V, W).
dijkstra(_, _, _, Path, Path).

continue_dijkstra(H1, WGraph, Status, Path0, PathF, _, _, V, _) :-
	rb_lookup(V, _, Status), !,
	% pick some other node.
	dijkstra(H1, WGraph, Status, Path0, PathF).
continue_dijkstra(H1, WGraph, Status0, Path0, PathF, D, V0, V, W) :-
	rb_insert(Status0, V, V0, Status),
	rb_lookup(V, Edges, WGraph),
	queue_edges(Edges, V, D, H1, H2),
	dijkstra(H2, WGraph, Status, [V0-(V-W)|Path0], PathF).

wdgraph_path(V, WG, P) :-
	wdgraph_to_dgraph(WG, G),
	dgraph_path(V, G, P).

wdgraph_reachable(V, G, Edges) :-
	rb_lookup(V, Children, G),
	ord_list_to_rbtree([V-[]],Done0),
	reachable(Children, Done0, _, G, Edges, []).

reachable([], Done, Done, _, Edges, Edges).
reachable([V-_|Vertices], Done0, DoneF, G, EdgesF, Edges0) :-
	rb_lookup(V,_, Done0), !,
	reachable(Vertices, Done0, DoneF, G, EdgesF, Edges0).
reachable([V-_|Vertices], Done0, DoneF, G, [V|EdgesF], Edges0) :-
	rb_lookup(V, Kids, G),
	rb_insert(Done0, V, [], Done1),
	reachable(Kids, Done1, DoneI, G, EdgesF, EdgesI),
	reachable(Vertices, DoneI, DoneF, G, EdgesI, Edges0).

%% @}
