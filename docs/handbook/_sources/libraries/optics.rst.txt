.. _library_optics:

``optics``
==========

OPTICS clusterer.

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
`../../apis/library_index.html#optics <../../apis/library_index.html#optics>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(optics(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(optics(tester)).

Features
--------

- **OPTICS Ordering**: Learns a deterministic ordering using
  density-based reachability over continuous datasets.
- **Adaptive Neighborhood Indexing**: Uses a low-dimensional
  epsilon-grid index when it is likely to be cheaper and otherwise falls
  back to a deterministic metric tree for neighborhood search during
  ordering construction. The search backend can also be selected
  explicitly.
- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Distance Metrics**: Supports ``euclidean`` and ``manhattan``
  distances.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Epsilon-Based Extraction**: Extracts clusters from the ordering
  using a configurable extraction epsilon threshold.
- **Noise Detection**: New instances not reachable from an extracted
  core cluster within the extraction threshold are assigned to
  ``noise``.
- **Prediction Pruning**: Classification reuses per-cluster core-point
  bounds to prune clusters that cannot beat the current best reachable
  match.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``ordering_and_extraction_epsilons(MaximumOrderingEpsilon, ExtractionEpsilon)``:
  Pair of epsilon thresholds where ``MaximumOrderingEpsilon`` is the
  neighborhood radius used while constructing the OPTICS ordering and
  ``ExtractionEpsilon`` is the threshold used when extracting clusters
  from the learned ordering and when classifying new instances. Default
  is ``ordering_and_extraction_epsilons(1.0, 1.0)``.
  ``ExtractionEpsilon`` must not be greater than
  ``MaximumOrderingEpsilon``.
- ``search_index(SearchIndex)``: Search backend selection used while
  constructing the OPTICS ordering. Options are ``auto`` (default),
  ``grid``, and ``metric_tree``.
- ``minimum_points(MinimumPoints)``: Minimum neighborhood size required
  for a point to be considered a core point. Default is ``2``.
- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default) or ``manhattan``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 5. For
example:

::

   optics_clusterer(Encoders, Ordering, Clusters, Noise, Options)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Ordering``: List of ordered points annotated with reachability and
  core-distance information.
- ``Clusters``: List of extracted clusters in cluster-id order.
- ``Noise``: List of extracted noise points.
- ``Options``: Effective training options used to learn the clusterer.
