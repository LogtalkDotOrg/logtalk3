.. _library_kmedians:

``kmedians``
============

k-Medians clusterer.

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
`../../apis/library_index.html#kmedians <../../apis/library_index.html#kmedians>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(kmedians(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(kmedians(tester)).

To run the performance and reference-fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(kmedians(tester_performance)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Deterministic Initialization**: Supports ``first_k`` and
  deterministic ``spread`` initialization.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Manhattan Distance**: Uses Manhattan distance for cluster assignment
  and convergence checks.
- **Rich Training Diagnostics**: Learned clusterers report training
  example count, convergence status, iteration count, and final median
  shift.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous
  medians instead of failing.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to learn. Default is ``2``.
- ``maximum_iterations(Iterations)``: Maximum number of median-update
  iterations. Default is ``100``.
- ``tolerance(Tolerance)``: Maximum median shift threshold for
  convergence. Default is ``1.0e-6``.
- ``initialization(Initialization)``: Median initialization strategy.
  Options: ``spread`` (default) or ``first_k``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Diagnostics
-----------

The ``diagnostics/2`` predicate returns a list containing:

- ``model(kmedians)``
- ``median_count(Count)``
- ``training_example_count(Count)``
- ``convergence(Reason)``
- ``iterations(Count)``
- ``final_shift(Shift)``
- ``options(Options)``

Clusterer Representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 4. For
example:

::

   kmedians_clusterer(Encoders, Medians, Options, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Medians``: List of median vectors in cluster-id order.
- ``Options``: Effective training options used to learn the clusterer.
- ``Diagnostics``: Training diagnostics metadata returned by the
  ``diagnostics/2`` predicate.

References
----------

1. MacQueen (1967) - "Some Methods for Classification and Analysis of
   Multivariate Observations". Proceedings of the Fifth Berkeley
   Symposium on Mathematical Statistics and Probability.
2. Kaufman & Rousseeuw (1990) - "Finding Groups in Data: An Introduction
   to Cluster Analysis". Wiley.
