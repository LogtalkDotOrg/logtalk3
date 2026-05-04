.. _library_agglomerative:

``agglomerative``
=================

Agglomerative clusterer.

The library implements the ``clusterer_protocol`` defined in the
``clustering_protocols`` library. It provides predicates for learning a
clusterer from a dataset, assigning new instances to clusters, and
exporting the learned clusterer as a list of predicate clauses or to a
file.

Datasets are represented as objects implementing the
``clustering_dataset_protocol`` protocol from the
``clustering_protocols`` library.

API documentation
-----------------

Open the
`../../apis/library_index.html#agglomerative <../../apis/library_index.html#agglomerative>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(agglomerative(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(agglomerative(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(agglomerative(tester_performance)).

Features
--------

- **Bottom-Up Clustering**: Uses deterministic bottom-up agglomerative
  clustering and stops when the requested number of clusters is reached.
- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Linkage Strategies**: Supports ``single``, ``complete``, and
  ``average`` linkage.
- **Distance Metrics**: Supports ``euclidean`` and ``manhattan``
  distances.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Linkage-Aware Prediction**: New instances are assigned to the
  nearest learned cluster using the selected linkage strategy and
  distance metric applied to the learned cluster members.
- **Deterministic Ordering**: Equal-distance merges are broken using
  node-id order and final clusters are ordered by minimum training
  example id so equivalent dataset permutations keep the same cluster
  ids.
- **Cached Distances**: Inter-cluster distances are cached and
  incrementally updated after each merge instead of being fully
  recomputed from cluster members at every iteration.
- **Priority-Queue Merge Selection**: Candidate merges are tracked in a
  min-heap keyed by distance and node-id order, allowing stale entries
  to be discarded lazily while keeping merge selection deterministic.
- **Rich Diagnostics**: Diagnostics report the training example count,
  performed merge count, initial pair count, maximum heap size,
  stale-pair discard count, deterministic pair-selection strategy, and
  linkage-aware prediction strategy.
- **Fail-Fast Consistency Checks**: Internal heap, active-node, and
  cached-distance inconsistencies raise explicit
  ``agglomerative_error/2`` exceptions instead of failing silently.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to retain after merging. Default is
  ``2``.
- ``linkage(Linkage)``: Linkage strategy to use. Options: ``single``,
  ``complete``, or ``average`` (default).
- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default) or ``manhattan``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 4. For
example:

::

   agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Clusters``: List of ``cluster(Id, Points)`` terms in cluster-id
  order.
- ``Prototypes``: List of average vectors used for display, diagnostics,
  and export metadata.
- ``Options``: Effective training options used to learn the clusterer.
- ``Diagnostics``: Training metadata including heap and prediction
  details.

Diagnostics
-----------

The ``diagnostics/2`` predicate returns metadata including:

- ``training_example_count/1``
- ``merge_count/1``
- ``initial_pair_count/1``
- ``maximum_heap_size/1``
- ``stale_pair_discard_count/1``
- ``pair_selection(priority_queue)``
- ``prediction_strategy(cluster_member_linkage_distance)``
- ``tie_breaking(node_id_order)``
- ``options/1``
