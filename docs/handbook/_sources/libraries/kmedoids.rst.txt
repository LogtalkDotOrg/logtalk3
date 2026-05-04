.. _library_kmedoids:

``kmedoids``
============

k-Medoids clusterer.

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
`../../apis/library_index.html#kmedoids <../../apis/library_index.html#kmedoids>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(kmedoids(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(kmedoids(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(kmedoids(tester_performance)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Distance Metrics**: Supports ``euclidean`` and ``manhattan``
  distances.
- **Deterministic Initialization**: Supports ``first_k`` and
  deterministic ``spread`` initialization.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Rich Training Diagnostics**: Learned clusterers report training
  example count, convergence status, iteration count, and final medoid
  shift.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous
  medoids instead of failing.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to learn. Default is ``2``.
- ``maximum_iterations(Iterations)``: Maximum number of medoid-update
  iterations. Default is ``100``.
- ``tolerance(Tolerance)``: Maximum medoid shift threshold for
  convergence. Default is ``1.0e-6``.
- ``initialization(Initialization)``: Medoid initialization strategy.
  Options: ``spread`` (default) or ``first_k``.
- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default) or ``manhattan``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Diagnostics
-----------

The ``diagnostics/2`` predicate returns a list containing:

- ``model(kmedoids)``
- ``medoid_count(Count)``
- ``training_example_count(Count)``
- ``convergence(Reason)``
- ``iterations(Count)``
- ``final_shift(Shift)``
- ``options(Options)``

Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 4. For
example:

::

   kmedoids_clusterer(Encoders, Medoids, Options, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Medoids``: List of medoid vectors in cluster-id order.
- ``Options``: Effective training options used to learn the clusterer.
- ``Diagnostics``: Training diagnostics metadata returned by the
  ``diagnostics/2`` predicate.

References
----------

1. Kaufman & Rousseeuw (1987) - "Clustering by Means of Medoids". In
   Statistical Data Analysis Based on the L1-Norm and Related Methods.
2. Kaufman & Rousseeuw (1990) - "Finding Groups in Data: An Introduction
   to Cluster Analysis". Wiley.
