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

Features
--------

- **Bottom-Up Clustering**: Merges singleton clusters until the
  requested number of clusters is reached.
- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Linkage Strategies**: Supports ``single``, ``complete``, and
  ``average`` linkage.
- **Distance Metrics**: Supports ``euclidean`` and ``manhattan``
  distances.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
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

Clusterer Representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 4. For
example:

::

   agglomerative_clusterer(Encoders, Clusters, Prototypes, Options)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Clusters``: List of ``cluster(Id, Points)`` terms in cluster-id
  order.
- ``Prototypes``: List of average vectors used for assigning new
  instances.
- ``Options``: Effective training options used to learn the clusterer.
