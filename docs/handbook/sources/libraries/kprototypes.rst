.. _library_kprototypes:

``kprototypes``
===============

k-Prototypes clusterer.

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
`../../apis/library_index.html#kprototypes <../../apis/library_index.html#kprototypes>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(kprototypes(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(kprototypes(tester)).

To run the performance and reference-fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(kprototypes(tester_performance)).

Features
--------

- **Mixed Datasets**: Accepts datasets with continuous, discrete, or
  mixed attributes.
- **Deterministic Initialization**: Supports ``first_k`` and
  deterministic ``spread`` initialization.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Categorical Weighting**: Uses a ``gamma`` mismatch penalty for
  discrete attributes.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous
  prototypes instead of failing.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to learn. Default is ``2``.
- ``maximum_iterations(Iterations)``: Maximum number of prototype-update
  iterations. Default is ``100``.
- ``tolerance(Tolerance)``: Maximum prototype shift threshold for
  convergence. Default is ``1.0e-6``.
- ``initialization(Initialization)``: Prototype initialization strategy.
  Options: ``spread`` (default) or ``first_k``.
- ``gamma(Gamma)``: Penalty added for each discrete-feature mismatch.
  Default is ``1.0``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Clusterer Representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 3. For
example:

::

   kprototypes_clusterer(Encoders, Prototypes, Options)

Where:

- ``Encoders``: List of continuous and discrete attribute encoders.
- ``Prototypes``: List of learned mixed prototypes in cluster-id order.
- ``Options``: Effective training options used to learn the clusterer.

References
----------

1. Huang (1997) - "Clustering large data sets with mixed numeric and
   categorical values". Proceedings of the First Pacific-Asia Conference
   on Knowledge Discovery and Data Mining.
2. Huang (1998) - "Extensions to the k-means algorithm for clustering
   large data sets with categorical values". Data Mining and Knowledge
   Discovery, 2, 283-304.
