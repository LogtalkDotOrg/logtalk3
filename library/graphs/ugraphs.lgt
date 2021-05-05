/**
 * @file   ugraphs.yap
 * @author R.A.O'Keefe
 * @author adapted to support some of the functionality of the SICStus ugraphs library
by Vitor Santos Costa.
 * @date  20 March 1984
 *
 * @brief
 *
 *
*/
%   File   : GRAPHS.PL
%   Author : R.A.O'Keefe
%   Updated:
%   Purpose: .

%
%
%

:- module(ugraphs,
	  [
	   add_vertices/3,
	   add_edges/3,
	   complement/2,
	   compose/3,
	   del_edges/3,
	   del_vertices/3,
	   edges/2,
	   neighbours/3,
	   neighbors/3,
	   reachable/3,
	   top_sort/2,
	   top_sort/3,
	   transitive_closure/2,
	   transpose/2,
	   vertices/2,
	   vertices_edges_to_ugraph/3,
	   ugraph_union/3
	]).


/** @defgroup ugraphs Unweighted Graphs
@ingroup library
@{

The following graph manipulation routines are based in code originally
written by Richard O'Keefe. The code was then extended to be compatible
with the SICStus Prolog ugraphs library. The routines assume directed
graphs, undirected graphs may be implemented by using two edges. Graphs
are represented in one of two ways:

+ The P-representation of a graph is a list of (from-to) vertex
pairs, where the pairs can be in any old order.  This form is
convenient for input/output.

The S-representation of a graph is a list of (vertex-neighbors)
pairs, where the pairs are in standard order (as produced by keysort)
and the neighbors of each vertex are also in standard order (as
produced by sort).  This form is convenient for many calculations.


These built-ins are available once included with the
`use_module(library(ugraphs))` command.

*/

/*  The P-representation of a graph is a list of (from-to) vertex
    pairs, where the pairs can be in any old order.  This form is
    convenient for input/output.

    The S-representation of a graph is a list of (vertex-neighbours)
    pairs, where the pairs are in standard order (as produced by
    keysort) and the neighbours of each vertex are also in standard
    order (as produced by sort).  This form is convenient for many
    calculations.

    p_to_s_graph(Pform, Sform) converts a P- to an S- representation.
    s_to_p_graph(Sform, Pform) converts an S- to a P- representation.

    warshall(Graph, Closure) takes the transitive closure of a graph
    in S-form.  (NB: this is not the reflexive transitive closure).

    s_to_p_trans(Sform, Pform) converts Sform to Pform, transposed.

    p_transpose transposes a graph in P-form, cost O(|E|).
    s_transpose transposes a graph in S-form, cost O(|V|^2).
*/

/** @pred  vertices_edges_to_ugraph(+ _Vertices_, + _Edges_, - _Graph_)


Given a graph with a set of vertices  _Vertices_ and a set of edges
 _Edges_,  _Graph_ must unify with the corresponding
s-representation. Note that the vertices without edges will appear in
 _Vertices_ but not in  _Edges_. Moreover, it is sufficient for a
vertex to appear in  _Edges_.

~~~~~{.prolog}
?- vertices_edges_to_ugraph([],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[]] ?

~~~~~
In this case all edges are defined implicitly. The next example shows
three unconnected edges:

~~~~~{.prolog}
?- vertices_edges_to_ugraph([6,7,8],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[],6-[],7-[],8-[]] ?

~~~~~


*/
/** @pred add_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_. In the next example:

~~~~~{.prolog}
?- add_edges([1-[3,5],2-[4],3-[],4-[5],5-[],6-[],
              7-[],8-[]],[1-6,2-3,3-2,5-7,3-2,4-5],NL).

NL = [1-[3,5,6],2-[3,4],3-[2],4-[5],5-[7],6-[],7-[],8-[]]
~~~~~


*/
/** @pred add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~{.prolog}
?- add_vertices([1-[3,5],2-[4],3-[],4-[5],
                 5-[],6-[],7-[],8-[]],
                [0,2,9,10,11],
                   NG).

NG = [0-[],1-[3,5],2-[4],3-[],4-[5],5-[],
      6-[],7-[],8-[],9-[],10-[],11-[]]
~~~~~


*/
/** @pred complement(+ _Graph_, - _NewGraph_)


Unify  _NewGraph_ with the graph complementary to  _Graph_.
In the next example:

~~~~~{.prolog}
?- complement([1-[3,5],2-[4],3-[],
               4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[2,4,6,7,8],2-[1,3,5,6,7,8],3-[1,2,4,5,6,7,8],
      4-[3,5,6,8],5-[1,2,3,4,6,7,8],6-[1,2,3,4,5,7,8],
      7-[1,2,3,4,5,6,8],8-[1,2,3,4,5,6,7]]
~~~~~


*/
/** @pred compose(+ _LeftGraph_, + _RightGraph_, - _NewGraph_)


Compose the graphs  _LeftGraph_ and  _RightGraph_ to form  _NewGraph_.
In the next example:

~~~~~{.prolog}
?- compose([1-[2],2-[3]],[2-[4],3-[1,2,4]],L).

L = [1-[4],2-[1,2,4],3-[]]
~~~~~


*/
/** @pred del_edges(+ _Graph_, + _Edges_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted. In the next example:

~~~~~{.prolog}
?- del_edges([1-[3,5],2-[4],3-[],4-[5],5-[],
              6-[],7-[],8-[]],
             [1-6,2-3,3-2,5-7,3-2,4-5,1-3],NL).

NL = [1-[5],2-[4],3-[],4-[],5-[],6-[],7-[],8-[]]
~~~~~


*/
/** @pred del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~{.prolog}
?- del_vertices([2,1],[1-[3,5],2-[4],3-[],
                 4-[5],5-[],6-[],7-[2,6],8-[]],NL).

NL = [3-[],4-[5],5-[],6-[],7-[6],8-[]]
~~~~~


*/
/** @pred edges(+ _Graph_, - _Edges_)


Unify  _Edges_ with all edges appearing in graph
 _Graph_. In the next example:

~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~


*/
/** @pred neighbors(+ _Vertex_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail. In the next
example:

~~~~~{.prolog}
?- neighbors(4,[1-[3,5],2-[4],3-[],
                4-[1,2,7,5],5-[],6-[],7-[],8-[]],
             NL).

NL = [1,2,7,5]
~~~~~


*/
/** @pred neighbours(+ _Vertex_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_. In the next example:

~~~~~{.prolog}
?- neighbours(4,[1-[3,5],2-[4],3-[],
                 4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1,2,7,5]
~~~~~


*/
/** @pred reachable(+ _Node_, + _Graph_, - _Vertices_)


Unify  _Vertices_ with the set of all vertices in graph
 _Graph_ that are reachable from  _Node_. In the next example:

~~~~~{.prolog}
?- reachable(1,[1-[3,5],2-[4],3-[],4-[5],5-[]],V).

V = [1,3,5]
~~~~~




 */
/** @pred top_sort(+ _Graph_, - _Sort0_, - _Sort_)

Generate the difference list  _Sort_- _Sort0_ as a topological
sorting of graph  _Graph_, if one is possible.


*/
/** @pred top_sort(+ _Graph_, - _Sort_)


Generate the set of nodes  _Sort_ as a topological sorting of graph
 _Graph_, if one is possible.
In the next example we show how topological sorting works for a linear graph:

~~~~~{.prolog}
?- top_sort([_138-[_219],_219-[_139], _139-[]],L).

L = [_138,_219,_139]
~~~~~


*/
/** @pred transitive_closure(+ _Graph_, + _Closure_)


Generate the graph  _Closure_ as the transitive closure of graph
 _Graph_.
In the next example:

~~~~~{.prolog}
?- transitive_closure([1-[2,3],2-[4,5],4-[6]],L).

L = [1-[2,3,4,5,6],2-[4,5,6],4-[6]]
~~~~~


*/
/** @pred vertices(+ _Graph_, - _Vertices_)


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_. In the next example:

~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~


*/

:- use_module(lists, [
	append/3,
	member/2,
	memberchk/2
   ]).

:- use_module(ordsets, [
	ord_add_element/3,
	ord_subtract/3,
	ord_union/3,
	ord_union/4
   ]).


/*

:- public
	p_to_s_graph/2,
	s_to_p_graph/2, % edges
	s_to_p_trans/2,
	p_member/3,
	s_member/3,
	p_transpose/2,
	s_transpose/2,
	compose/3,
	top_sort/2,
	vertices/2,
	warshall/2.

:- mode
	vertices(+, -),
	p_to_s_graph(+, -),
	    p_to_s_vertices(+, -),
	    p_to_s_group(+, +, -),
		p_to_s_group(+, +, -, -),
	s_to_p_graph(+, -),
	    s_to_p_graph(+, +, -, -),
	s_to_p_trans(+, -),
	    s_to_p_trans(+, +, -, -),
	p_member(?, ?, +),
	s_member(?, ?, +),
	p_transpose(+, -),
	s_transpose(+, -),
	    s_transpose(+, -, ?, -),
		transpose_s(+, +, +, -),
	compose(+, +, -),
	    compose(+, +, +, -),
		compose1(+, +, +, -),
		    compose1(+, +, +, +, +, +, +, -),
	top_sort(+, -),
	    vertices_and_zeros(+, -, ?),
	    count_edges(+, +, +, -),
		incr_list(+, +, +, -),
	    select_zeros(+, +, -),
	    top_sort(+, -, +, +, +),
		decr_list(+, +, +, -, +, -),
	warshall(+, -),
	    warshall(+, +, -),
		warshall(+, +, +, -).

*/


%   vertices(S_Graph,  Vertices)
%   strips off the neighbours lists of an S-representation to produce
%   a list of the vertices of the graph.  (It is a characteristic of
%   S-representations that *every* vertex appears, even if it has no
%   neighbours.)

vertices([], []) :- !.
vertices([Vertex-_|Graph], [Vertex|Vertices]) :-
	vertices(Graph, Vertices).

vertices_edges_to_ugraph(Vertices, Edges, Graph) :-
	sort(Edges, EdgeSet),
	p_to_s_vertices(EdgeSet, IVertexBag),
	append(Vertices, IVertexBag, VertexBag),
	sort(VertexBag, VertexSet),
	p_to_s_group(VertexSet, EdgeSet, Graph).


add_vertices(Graph, Vertices, NewGraph) :-
	msort(Vertices, V1),
	add_vertices_to_s_graph(V1, Graph, NewGraph).

add_vertices_to_s_graph(L, [], NL) :- !, add_empty_vertices(L, NL).
add_vertices_to_s_graph([], L, L) :- !.
add_vertices_to_s_graph([V1|VL], [V-Edges|G], NGL) :-
	compare(Res, V1, V),
	add_vertices_to_s_graph(Res, V1, VL, V, Edges, G, NGL).

add_vertices_to_s_graph(=, _, VL, V, Edges, G, [V-Edges|NGL]) :-
	add_vertices_to_s_graph(VL, G, NGL).
add_vertices_to_s_graph(<, V1, VL, V, Edges, G, [V1-[]|NGL]) :-
	add_vertices_to_s_graph(VL, [V-Edges|G], NGL).
add_vertices_to_s_graph(>, V1, VL, V, Edges, G, [V-Edges|NGL]) :-
	add_vertices_to_s_graph([V1|VL], G, NGL).

add_empty_vertices([], []).
add_empty_vertices([V|G], [V-[]|NG]) :-
	add_empty_vertices(G, NG).

%
% unmark a set of vertices plus all edges leading to them.
%
del_vertices(Graph, Vertices, NewGraph) :-
	msort(Vertices, V1),
	(V1 = [] -> Graph = NewGraph ;
	 del_vertices(Graph, V1, V1, NewGraph) ).

del_vertices(G, [], V1, NG) :- !,
	del_remaining_edges_for_vertices(G, V1, NG).
del_vertices([], _, _, []).
del_vertices([V-Edges|G], [V0|Vs], V1, NG) :-
	compare(Res, V, V0),
	split_on_del_vertices(Res, V,Edges, [V0|Vs], NVs, V1, NG, NGr),
	del_vertices(G, NVs, V1, NGr).

del_remaining_edges_for_vertices([], _, []).
del_remaining_edges_for_vertices([V0-Edges|G], V1, [V0-NEdges|NG]) :-
	ord_subtract(Edges, V1, NEdges),
	del_remaining_edges_for_vertices(G, V1, NG).

split_on_del_vertices(<, V, Edges, Vs, Vs, V1, [V-NEdges|NG], NG) :-
	ord_subtract(Edges, V1, NEdges).
split_on_del_vertices(>, V, Edges, [_|Vs], Vs, V1, [V-NEdges|NG], NG) :-
	ord_subtract(Edges, V1, NEdges).
split_on_del_vertices(=, _, _, [_|Vs], Vs, _, NG, NG).

add_edges(Graph, Edges, NewGraph) :-
	p_to_s_graph(Edges, G1),
	graph_union(Graph, G1, NewGraph).

%   graph_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  This code is a copy
%   of set union

graph_union(Set1, [], Set1) :- !.
graph_union([], Set2, Set2) :- !.
graph_union([Head1-E1|Tail1], [Head2-E2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	graph_union(Order, Head1-E1, Tail1, Head2-E2, Tail2, Union).

graph_union(=, Head-E1,  Tail1, _-E2,     Tail2, [Head-Es|Union]) :-
	ord_union(E1, E2, Es),
	graph_union(Tail1, Tail2, Union).
graph_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	graph_union(Tail1, [Head2|Tail2], Union).
graph_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	graph_union([Head1|Tail1], Tail2, Union).

del_edges(Graph, Edges, NewGraph) :-
	p_to_s_graph(Edges, G1),
	graph_subtract(Graph, G1, NewGraph).

%   graph_subtract(+Set1, +Set2, ?Difference)
%   is based on ord_subtract
%

graph_subtract(Set1, [], Set1) :- !.
graph_subtract([], _, []).
graph_subtract([Head1-E1|Tail1], [Head2-E2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	graph_subtract(Order, Head1-E1, Tail1, Head2-E2, Tail2, Difference).

graph_subtract(=, H-E1,     Tail1, _-E2,     Tail2, [H-E|Difference]) :-
	ord_subtract(E1,E2,E),
	graph_subtract(Tail1, Tail2, Difference).
graph_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	graph_subtract(Tail1, [Head2|Tail2], Difference).
graph_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
	graph_subtract([Head1|Tail1], Tail2, Difference).



edges(Graph, Edges) :-
	s_to_p_graph(Graph, Edges).

p_to_s_graph(P_Graph, S_Graph) :-
	sort(P_Graph, EdgeSet),
	p_to_s_vertices(EdgeSet, VertexBag),
	sort(VertexBag, VertexSet),
	p_to_s_group(VertexSet, EdgeSet, S_Graph).


p_to_s_vertices([], []).
p_to_s_vertices([A-Z|Edges], [A,Z|Vertices]) :-
	p_to_s_vertices(Edges, Vertices).


p_to_s_group([], _, []).
p_to_s_group([Vertex|Vertices], EdgeSet, [Vertex-Neibs|G]) :-
	p_to_s_group(EdgeSet, Vertex, Neibs, RestEdges),
	p_to_s_group(Vertices, RestEdges, G).


p_to_s_group([V1-X|Edges], V2, [X|Neibs], RestEdges) :- V1 == V2, !,
	p_to_s_group(Edges, V2, Neibs, RestEdges).
p_to_s_group(Edges, _, [], Edges).



s_to_p_graph([], []) :- !.
s_to_p_graph([Vertex-Neibs|G], P_Graph) :-
	s_to_p_graph(Neibs, Vertex, P_Graph, Rest_P_Graph),
	s_to_p_graph(G, Rest_P_Graph).


s_to_p_graph([], _, P_Graph, P_Graph) :- !.
s_to_p_graph([Neib|Neibs], Vertex, [Vertex-Neib|P], Rest_P) :-
	s_to_p_graph(Neibs, Vertex, P, Rest_P).



s_to_p_trans([], []) :- !.
s_to_p_trans([Vertex-Neibs|G], P_Graph) :-
	s_to_p_trans(Neibs, Vertex, P_Graph, Rest_P_Graph),
	s_to_p_trans(G, Rest_P_Graph).


s_to_p_trans([], _, P_Graph, P_Graph) :- !.
s_to_p_trans([Neib|Neibs], Vertex, [Neib-Vertex|P], Rest_P) :-
	s_to_p_trans(Neibs, Vertex, P, Rest_P).



transitive_closure(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall([], Closure, Closure) :- !.
warshall([V-_|G], E, Closure) :-
	memberchk(V-Y, E),	%  Y := E(v)
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).


warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	memberchk(V, Neibs),
	!,
	ord_union(Neibs, Y, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :- !,
	warshall(G, V, Y, NewG).
warshall([], _, _, []).



p_transpose([], []) :- !.
p_transpose([From-To|Edges], [To-From|Transpose]) :-
	p_transpose(Edges, Transpose).


/** @pred transpose(+ _Graph_, - _NewGraph_)


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_. The cost is `O(|V|^2)`. In the next example:

~~~~~{.prolog}
?- transpose([1-[3,5],2-[4],3-[],
              4-[5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[],2-[],3-[1],4-[2],5-[1,4],6-[],7-[],8-[]]
~~~~~
Notice that an undirected graph is its own transpose.


*/
transpose(S_Graph, Transpose) :-
	s_transpose(S_Graph, Base, Base, Transpose).

s_transpose(S_Graph, Transpose) :-
	s_transpose(S_Graph, Base, Base, Transpose).

s_transpose([], [], Base, Base) :- !.
s_transpose([Vertex-Neibs|Graph], [Vertex-[]|RestBase], Base, Transpose) :-
	s_transpose(Graph, RestBase, Base, SoFar),
	transpose_s(SoFar, Neibs, Vertex, Transpose).

transpose_s([Neib-Trans|SoFar], [Neib|Neibs], Vertex,
		[Neib-[Vertex|Trans]|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([Head|SoFar], Neibs, Vertex, [Head|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([], [], _, []).



%   p_member(X, Y, P_Graph)
%   tests whether the edge (X,Y) occurs in the graph.  This always
%   costs O(|E|) time.  Here, as in all the operations in this file,
%   vertex labels are assumed to be ground terms, or at least to be
%   sufficiently instantiated that no two of them have a common instance.

p_member(X, Y, P_Graph) :-
	nonvar(X), nonvar(Y), !,
	memberchk(X-Y, P_Graph).
p_member(X, Y, P_Graph) :-
	member(X-Y, P_Graph).

%   s_member(X, Y, S_Graph)
%   tests whether the edge (X,Y) occurs in the graph.  If either
%   X or Y is instantiated, the check is order |V| rather than
%   order |E|.

s_member(X, Y, S_Graph) :-
	var(X), var(Y), !,
	member(X-Neibs, S_Graph),
	member(Y, Neibs).
s_member(X, Y, S_Graph) :-
	var(X), !,
	member(X-Neibs, S_Graph),
	memberchk(Y, Neibs).
s_member(X, Y, S_Graph) :-
	var(Y), !,
	memberchk(X-Neibs, S_Graph),
	member(Y, Neibs).
s_member(X, Y, S_Graph) :-
	memberchk(X-Neibs, S_Graph),
	memberchk(Y, Neibs).


%   compose(G1, G2, Composition)
%   calculates the composition of two S-form graphs, which need not
%   have the same set of vertices.

compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).


compose([], _, _, []) :- !.
compose([Vertex|Vertices], [Vertex-Neibs|G1], G2, [Vertex-Comp|Composition]) :- !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([Vertex|Vertices], G1, G2, [Vertex-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).


compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :-
	compare(Rel, V1, V2), !,
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).


compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :- !,
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :- !,
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).


/*  NOT USED AFTER ALL
%   raakau(Vertices, InitialValue, Tree)
%   takes an *ordered* list of verticies and an initial value, and
%   makes a very special sort of tree out of them, which represents
%   a function sending each vertex to the initial value.  Note that
%   in the third clause for raakau/6 Z can never be 0, this means
%   that it doesn't matter *what* "greatest member" is reported for
%   empty trees.

raakau(Vertices, InitialValue, Tree) :-
	length(Vertices, N),
	raakau(N, Vertices, _, _, InitialValue, Tree).


raakau(0, Vs, Vs, 0, I, t) :- !.
raakau(1, [V|Vs], Vs, V, I, t(V,I)) :- !.
raakau(N, Vi, Vo, W, I, t(V,W,I,L,R)) :-
	A is (N-1)/2,
	Z is (N-1)-A,	%  Z >= 1
	raakau(A, Vi, [V|Vm], _, I, L),
	raakau(Z, Vm, Vo, W, I, R).


%   incdec(OldTree, Labels, Incr, NewTree)
%   adds Incr to the value associated with each element of Labels
%   in OldTree, producing a new tree.  OldTree must have been produced
%   either by raakau or by incdec, Labels must be in ascedning order,
%   and must be a subset of the labels of the tree.

incdec(OldTree, Labels, Incr, NewTree) :-
	incdec(OldTree, NewTree, Labels, _, Incr).


incdec(t(V,M), t(V,N), [V|L], L, I) :- !,
	N is M+I.
incdec(t(V,W,M,L1,R1), t(V,W,N,L2,R2), Li, Lo, I) :-
	(   Li = [Hi|_], Hi @< V, !,
		incdec(L1, L2, Li, Lm, I)
	;   L2 = L1, Lm = Li
	),
	(   Lm = [V|Lr], !,
		N is M+I
	;   Lr = Lm, N = M
	),
	(   Lr = [Hr|_], Hr @=< W, !,
		incdec(R1, R2, Lr, Lo, I)
	;   R2 = R1, Lo = Lr
	).
  END UNUSED CODE */



top_sort(Graph, Sorted) :-
	vertices_and_zeros(Graph, Vertices, Counts0),
	count_edges(Graph, Vertices, Counts0, Counts1),
	select_zeros(Counts1, Vertices, Zeros),
	top_sort(Zeros, Sorted, Graph, Vertices, Counts1).

top_sort(Graph, Sorted0, Sorted) :-
	vertices_and_zeros(Graph, Vertices, Counts0),
	count_edges(Graph, Vertices, Counts0, Counts1),
	select_zeros(Counts1, Vertices, Zeros),
	top_sort(Zeros, Sorted, Sorted0, Graph, Vertices, Counts1).


vertices_and_zeros([], [], []) :- !.
vertices_and_zeros([Vertex-_|Graph], [Vertex|Vertices], [0|Zeros]) :-
	vertices_and_zeros(Graph, Vertices, Zeros).


count_edges([], _, Counts, Counts) :- !.
count_edges([_-Neibs|Graph], Vertices, Counts0, Counts2) :-
	incr_list(Neibs, Vertices, Counts0, Counts1),
	count_edges(Graph, Vertices, Counts1, Counts2).


incr_list([], _, Counts, Counts) :- !.
incr_list([V1|Neibs], [V2|Vertices], [M|Counts0], [N|Counts1]) :- V1 == V2, !,
	N is M+1,
	incr_list(Neibs, Vertices, Counts0, Counts1).
incr_list(Neibs, [_|Vertices], [N|Counts0], [N|Counts1]) :-
	incr_list(Neibs, Vertices, Counts0, Counts1).


select_zeros([], [], []) :- !.
select_zeros([0|Counts], [Vertex|Vertices], [Vertex|Zeros]) :- !,
	select_zeros(Counts, Vertices, Zeros).
select_zeros([_|Counts], [_|Vertices], Zeros) :-
	select_zeros(Counts, Vertices, Zeros).



top_sort([], [], Graph, _, Counts) :- !,
	vertices_and_zeros(Graph, _, Counts).
top_sort([Zero|Zeros], [Zero|Sorted], Graph, Vertices, Counts1) :-
	graph_memberchk(Zero-Neibs, Graph),
	decr_list(Neibs, Vertices, Counts1, Counts2, Zeros, NewZeros),
	top_sort(NewZeros, Sorted, Graph, Vertices, Counts2).

top_sort([], Sorted0, Sorted0, Graph, _, Counts) :- !,
	vertices_and_zeros(Graph, _, Counts).
top_sort([Zero|Zeros], [Zero|Sorted], Sorted0, Graph, Vertices, Counts1) :-
	graph_memberchk(Zero-Neibs, Graph),
	decr_list(Neibs, Vertices, Counts1, Counts2, Zeros, NewZeros),
	top_sort(NewZeros, Sorted, Sorted0, Graph, Vertices, Counts2).

graph_memberchk(Element1-Edges, [Element2-Edges2|_]) :- Element1 == Element2, !,
	Edges = Edges2.
graph_memberchk(Element, [_|Rest]) :-
        graph_memberchk(Element, Rest).


decr_list([], _, Counts, Counts, Zeros, Zeros) :- !.
decr_list([V1|Neibs], [V2|Vertices], [1|Counts1], [0|Counts2], Zi, Zo) :- V1 == V2, !,
	decr_list(Neibs, Vertices, Counts1, Counts2, [V2|Zi], Zo).
decr_list([V1|Neibs], [V2|Vertices], [N|Counts1], [M|Counts2], Zi, Zo) :- V1 == V2, !,
	M is N-1,
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).
decr_list(Neibs, [_|Vertices], [N|Counts1], [N|Counts2], Zi, Zo) :-
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).



neighbors(V,[V0-Neig|_],Neig) :- V == V0, !.
neighbors(V,[_|G],Neig) :-
	neighbors(V,G,Neig).

neighbours(V,[V0-Neig|_],Neig) :- V == V0, !.
neighbours(V,[_|G],Neig) :-
	neighbours(V,G,Neig).


%
% Simple two-step algorithm. You could be smarter, I suppose.
%
complement(G, NG) :-
	vertices(G,Vs),
	complement(G,Vs,NG).

complement([], _, []).
complement([V-Ns|G], Vs, [V-INs|NG]) :-
	ord_add_element(Ns,V,Ns1),
	ord_subtract(Vs,Ns1,INs),
	complement(G, Vs, NG).



reachable(N, G, Rs) :-
	reachable([N], G, [N], Rs).

reachable([], _, Rs, Rs).
reachable([N|Ns], G, Rs0, RsF) :-
	neighbours(N, G, Nei),
	ord_union(Rs0, Nei, Rs1, D),
	append(Ns, D, Nsi),
	reachable(Nsi, G, Rs1, RsF).

%%	ugraph_union(+Set1, +Set2, ?Union)
%
%	Is true when Union is the union of Set1 and Set2. This code is a
%	copy of set union

ugraph_union(Set1, [], Set1) :- !.
ugraph_union([], Set2, Set2) :- !.
ugraph_union([Head1-E1|Tail1], [Head2-E2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ugraph_union(Order, Head1-E1, Tail1, Head2-E2, Tail2, Union).

ugraph_union(=, Head-E1, Tail1, _-E2, Tail2, [Head-Es|Union]) :-
	ord_union(E1, E2, Es),
	ugraph_union(Tail1, Tail2, Union).
ugraph_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ugraph_union(Tail1, [Head2|Tail2], Union).
ugraph_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ugraph_union([Head1|Tail1], Tail2, Union).
