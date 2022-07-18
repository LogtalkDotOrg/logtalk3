.. _library_union_find:

``union_find``
==============

This library implements a union-find data structure. This structure
tracks a set of elements partitioned into a number of disjoint
(non-overlapping) subsets. It provides fast operations to add new sets,
to merge existing sets, and to determine whether elements are in the
same set. This implementation of the union-find algorithm provides the
following features:

-  Path compression: Path compression flattens the structure of the tree
   by making every node point to the root whenever a find predicate is
   used on it.

-  Union by rank: Union predicates always attach the shorter tree to the
   root of the taller tree. Thus, the resulting tree is no taller than
   the originals unless they were of equal height, in which case the
   resulting tree is taller by one node.

For a general and extended discussion on this data structure, see e.g.

https://en.wikipedia.org/wiki/Disjoint-set_data_structure

API documentation
-----------------

Open the
`../../docs/library_index.html#union-find <../../docs/library_index.html#union-find>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(union_find(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(union_find(tester)).

Usage
-----

An usage example is Kruskal's algorithm, a minimum-spanning-tree
algorithm which finds an edge of the least possible weight that connects
any two trees in the forest. It is a greedy algorithm in graph theory as
it finds a minimum spanning tree for a connected weighted graph adding
increasing cost arcs at each step.

::

   :- object(kruskal).

       :- public(kruskal/2).

       :- uses(union_find, [
           new/2, find/4, union/4
       ]).

       kruskal(g(Vertices-Edges), g(Vertices-Tree)) :-
           new(Vertices, UnionFind),
           keysort(Edges, Sorted),
           kruskal(UnionFind, Sorted, Tree).

       kruskal(_, [], []).
       kruskal(UnionFind0, [Edge| Edges], [Edge| Tree]) :-
           Edge = _-(Vertex1, Vertex2),
           find(UnionFind0, Vertex1, Root1, UnionFind1),
           find(UnionFind1, Vertex2, Root2, UnionFind2),
           Root1 \== Root2,
           !,
           union(UnionFind2, Vertex1, Vertex2, UnionFind3),
           kruskal(UnionFind3, Edges, Tree).
       kruskal(UnionFind, [_| Edges], Tree) :-
           kruskal(UnionFind, Edges, Tree).

   :- end_object.

Sample query:

::

   | ?- kruskal::kruskal(g([a,b,c,d,e,f,g]-[7-(a,b), 5-(a,d), 8-(b,c), 7-(b,e), 9-(b,d), 5-(c,e), 15-(d,e), 6-(d,f), 8-(e,f), 9-(e,g), 11-(f,g)]), Tree).

   Tree = g([a,b,c,d,e,f,g]-[5-(a,d),5-(c,e),6-(d,f),7-(a,b),7-(b,e),9-(e,g)])
   yes
