/**
 * @file   wundgraphs.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   2006
 *
 *
*/


:- module( wundgraphs,
	   [
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

/**
* @defgroup wundgraphs Weighted Undirected Graphs
* @ingroup library
*
 * @brief   Weighted Undirected Graph Processing Utilities.
*/


:- reexport(wdgraphs,
	   [
	    wdgraph_new/1 as wundgraph_new,
	    wdgraph_add_vertex/3 as wundgraph_add_vertex,
	    wdgraph_add_vertices/3 as wundgraph_add_vertices,
	    wdgraph_vertices/2 as wundgraph_vertices,
	    wdgraph_del_vertices/3 as wundgraph_del_vertices,
	    wdgraph_edge/4 as wundgraph_edge,
	    wdgraph_symmetric_closure/2 as wdgraph_to_wundgraph,
	    wdgraph_to_dgraph/2 as wundgraph_to_undgraph,
	    dgraph_to_wdgraph/2 as undgraph_to_wundgraph,
	    wdgraph_min_path/5 as wundgraph_min_path,
	    wdgraph_min_paths/3 as wundgraph_min_paths,
	    wdgraph_max_path/5 as wundgraph_max_path,
	    wdgraph_path/3 as wundgraph_path,
	    wdgraph_reachable/3 as wundgraph_reachable
	   ]).

:- use_module(wdgraphs,
	   [
	    wdgraph_add_edge/5,
	    wdgraph_add_edges/3,
	    wdgraph_del_edge/5,
	    wdgraph_del_edges/3,
	    wdgraph_del_vertex/3,
	    wdgraph_edges/2,
	    wdgraph_neighbours/3,
	    wdgraph_wneighbours/3
	   ]).

:- use_module(library(rbtrees),
	[
         rb_new/1,
         rb_delete/4,
	 rb_partial_map/4,
	 rb_visit/2,
	 rb_insert/4,
	 rb_lookup/3
	]).

:- use_module(library(lists),
	[
         reverse/2
	]).

wundgraph_add_edge(Vs0, V1, V2, K, Vs2) :-
	wdgraphs:wdgraph_new_edge(V1,V2,K,Vs0,Vs1),
	wdgraphs:wdgraph_new_edge(V2,V1,K,Vs1,Vs2).

wundgraph_add_edges(G0, Edges, GF) :-
	dup_edges(Edges, DupEdges),
	wdgraph_add_edges(G0, DupEdges, GF).

dup_edges([],[]).
dup_edges([E1-(E2-K)|Edges], [E1-(E2-K),E2-(E1-K)|DupEdges]) :-
	dup_edges(Edges, DupEdges).

wundgraph_edges(Vs, Edges) :-
	wdgraph_edges(Vs, DupEdges),
	remove_dups(DupEdges,Edges).

remove_dups([],[]).
remove_dups([V1-(V2-K)|DupEdges],NEdges) :- V1 @< V2, !,
	NEdges = [V1-(V2-K)|Edges],
	remove_dups(DupEdges,Edges).
remove_dups([_|DupEdges],Edges) :-
	remove_dups(DupEdges,Edges).

wundgraph_neighbours(V,Vertices,Children) :-
	wdgraph_neighbours(V,Vertices,Children0),
	(
	    del_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).
wundgraph_neighbors(V,Vertices,Children) :-
	wdgraph_neighbours(V,Vertices,Children0),
	(
	    del_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).

wundgraph_wneighbours(V,Vertices,Children) :-
	wdgraph_wneighbours(V,Vertices,Children0),
	(
	    wdel_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).
wundgraph_wneighbors(V,Vertices,Children) :-
	wdgraph_wneighbours(V,Vertices,Children0),
	(
	    wdel_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).

del_me([], _, []).
del_me([K|Children], K1, NewChildren) :-
	( K == K1 ->
	    Children = NewChildren
	;
	    K @< K1 ->
	    NewChildren = [K|ChildrenLeft],
	    del_me(Children, K1, ChildrenLeft)
	;
	    NewChildren = [K|MoreChildren],
	    compact(Children, MoreChildren)
	).

wdel_me([], _, []).
wdel_me([K-A|Children], K1, NewChildren) :-
	( K == K1 ->
	    Children = NewChildren
	;
	    K @< K1 ->
	    NewChildren = [K-A|ChildrenLeft],
	    wdel_me(Children, K1, ChildrenLeft)
	;
	    NewChildren = [K-A|MoreChildren],
	    compact(Children, MoreChildren)
	).

wundgraph_del_edge(Vs0,V1,V2,K,VsF) :-
	wdgraph_del_edge(Vs0,V1,V2,K,Vs1),
	wdgraph_del_edge(Vs1,V2,V1,K,VsF).

wundgraph_del_edges(G0, Edges, GF) :-
	dup_edges(Edges,DupEdges),
	wdgraph_del_edges(G0, DupEdges, GF).

wundgraph_del_vertex(Vs0, V, Vsf) :-
	rb_delete(Vs0, V, BackEdges, Vsi),
	del_and_compact(BackEdges,V,BackVertices),
	rb_partial_map(Vsi, BackVertices, del_edge(V), Vsf).

del_and_compact([], _, []).
del_and_compact([K-_|Children], K1, NewChildren) :-
	( K == K1 ->
	    compact(Children, NewChildren)
	;
	    K @< K1 ->
	    NewChildren = [K|ChildrenLeft],
	    del_and_compact(Children, K1, ChildrenLeft)
	;
	    NewChildren = [K|CompactChildren],
	    compact(Children, CompactChildren)
	).

compact([], []).
compact([K-_|Children], [K|CompactChildren]) :-
	compact(Children, CompactChildren).


del_edge(_, [], []).
del_edge(K1, [K-W|Children], NewChildren) :-
	( K == K1 ->
	    Children = NewChildren
	;
	    K @< K1 ->
	    NewChildren = [K-W|ChildrenLeft],
	    del_edge(K1, Children, ChildrenLeft)
	;
	    NewChildren = [K-W|Children]
	).

wundgraph_to_wdgraph(G, G).


% simplistic algorithm to build a minimal spanning tree.
% Just sort edges and then walk over each one.

wundgraph_min_tree(G, T, C) :-
	rb_visit(G, Els0),
	generate_min_tree(Els0, T, C).

generate_min_tree([], T, 0) :- !,
	wundgraph_new(T).
generate_min_tree([El-_], T, 0) :- !,
	wundgraph_new(T0),
	wundgraph_add_vertex(T0, El, T).
generate_min_tree(Els0, T, C) :-
	mk_list_of_edges(Els0, Edges),
	keysort(Edges, SortedEdges),
	rb_new(V0),
	rb_new(T0),
	add_sorted_edges(SortedEdges, V0, TreeEdges, 0, C),
	wundgraph_add_edges(T0, TreeEdges, T).

wundgraph_max_tree(G, T, C) :-
	rb_visit(G, Els0),
	generate_max_tree(Els0, T, C).

generate_max_tree([], T, 0) :- !,
	wundgraph_new(T).
generate_max_tree([El-_], T, 0) :- !,
	wundgraph_new(T0),
	wundgraph_add_vertex(T0, El, T).
generate_max_tree(Els0, T, C) :-
	mk_list_of_edges(Els0, Edges),
	keysort(Edges, SortedEdges),
	reverse(SortedEdges, ReversedEdges),
	rb_new(V0),
	rb_new(T0),
	add_sorted_edges(ReversedEdges, V0, TreeEdges, 0, C),
	wundgraph_add_edges(T0, TreeEdges, T).

mk_list_of_edges([], []).
mk_list_of_edges([V-Els|Els0], Edges) :-
	add_neighbs(Els, V, Edges, Edges0),
	mk_list_of_edges(Els0, Edges0).

add_neighbs([], _, Edges, Edges).
add_neighbs([V-W|Els], V0, [W-(V0-V)|Edges], Edges0) :-
	V0 @< V, !,
	add_neighbs(Els, V0, Edges, Edges0).
add_neighbs([_|Els], V0, Edges, Edges0) :-
	add_neighbs(Els, V0, Edges, Edges0).


add_sorted_edges([], _, [], C, C).
add_sorted_edges([W-(V0-V)|SortedEdges], T0, NewTreeEdges, C0, C) :-
	( rb_lookup(V0, Component, T0) ->
	    ( rb_lookup(V, Component1, T0) ->
		( Component \== Component1 ->
		    /* edge that links two separate sub-trees (components) */
	          Component = Component1,
		  Ti = T0
	        ;
	        /* same component, can't add edge */
	          fail
	        )
	      ;
              /* V is new */
	      rb_insert(T0, V, Component, Ti)
	    )
	;
	    ( rb_lookup(V, Component1, T0) ->
              /* V0 is new */
	      rb_insert(T0, V0, Component1, Ti)
	    ;
              /* new edges, new tree */
	      rb_insert(T0, V0, NewComponent, T1),
	      rb_insert(T1, V, NewComponent, Ti)
	    )
        ),
	!,
	NewTreeEdges = [(V0-(V-W)),(V-(V0-W))|TreeEdges],
	Ci is C0+W,
	add_sorted_edges(SortedEdges, Ti, TreeEdges, Ci, C).
add_sorted_edges([_|SortedEdges],  T0, NewTreeEdges, C0, C) :-
	add_sorted_edges(SortedEdges, T0, NewTreeEdges, C0, C).

