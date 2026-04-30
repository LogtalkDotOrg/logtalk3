.. _library_isolation_forest:

``isolation_forest``
====================

This library implements the Extended Isolation Forest (EIF) algorithm
for anomaly detection as described by Hariri et al. (2019). The Extended
Isolation Forest improves upon the original Isolation Forest algorithm
(Liu et al., 2008) by using random hyperplane cuts instead of
axis-aligned cuts, eliminating bias artifacts in anomaly scores along
coordinate axes.

The algorithm builds an ensemble of isolation trees (iTrees) by
recursively partitioning data using random hyperplanes. Anomalous
points, being few and different from normal points, require fewer
partitions (shorter path lengths) to be isolated. The anomaly score for
an instance is computed based on the average path length across all
trees in the forest.

Datasets are represented as objects implementing the
``anomaly_dataset_protocol`` protocol from the
``anomaly_detection_protocols`` library. See the
``anomaly_detection_protocols/test_datasets`` directory for examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#isolation-forest <../../apis/library_index.html#isolation-forest>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(isolation_forest(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(isolation_forest(tester)).

Implemented features
--------------------

- Extended Isolation Forest with random hyperplane cuts: splits are
  defined by random normal vectors and intercept points drawn from the
  data range, producing ``(x - p) * n =< 0`` partitions that generalize
  to arbitrary orientations
- Configurable extension level: level 0 corresponds to the original
  axis-aligned Isolation Forest; levels up to ``d - 1`` (the default)
  use fully extended random hyperplanes where ``d`` is the number of
  dimensions
- Anomaly score computation following Liu et al. (2008):
  ``s(x) = 2^(-E(h(x)) / c(psi))`` where ``E(h(x))`` is the average path
  length across all trees, ``c(psi)`` is the average path length of
  unsuccessful searches in a BST, and ``psi`` is the subsample size
- Handling of both continuous (numeric) and discrete (categorical)
  attributes: discrete attributes are mapped to numeric indices based on
  their position in the attribute value list declared by the dataset
- Handling of missing attribute values (represented using anonymous
  variables): during tree construction, missing values are replaced with
  random values drawn from the observed range of the corresponding
  attribute; during scoring, each internal tree node stores its own
  feasible per-dimension bounds so that missing dimensions can be routed
  using subtree-local support instead of only the global training ranges
- Scoring all dataset instances with results sorted by descending
  anomaly score for easy identification of top anomalies
- Pretty-printing of learned models with tree depth and node count
  summaries
- Learning rejects empty datasets with a
  ``domain_error(non_empty_dataset, Dataset)`` exception

Options
-------

The following options can be passed to the ``learn/3`` and ``predict/4``
predicates:

- ``number_of_trees(N)``: number of isolation trees to build (default:
  ``100``)
- ``subsample_size(N)``: subsample size used to build each isolation
  tree. When omitted, the implementation uses ``256`` or the number of
  training instances if smaller
- ``extension_level(N)``: controls the dimensionality of the random
  hyperplane cuts. ``0`` reproduces the original axis-aligned Isolation
  Forest; when omitted, the implementation uses ``d - 1``, where ``d``
  is the number of dimensions
- ``anomaly_threshold(T)``: threshold used by ``predict/3-4`` (default:
  ``0.5``)

Detector Representation
-----------------------

The learned detector is represented by default as:

::

   if_model(Trees, Psi, AttributeNames, Attributes, Ranges, Options)

Where:

- ``Trees``: List of learned isolation trees
- ``Psi``: Effective subsample size used to build each tree
- ``AttributeNames``: List of attribute names in order
- ``Attributes``: List of ``Attribute-Values`` declarations from the
  training dataset
- ``Ranges``: Observed numeric ranges used for imputing missing values
  during training
- ``Options``: Learned options

Each internal tree node additionally stores node-local dimension bounds
used when resolving missing-value routing during scoring.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this detector term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

Limitations
-----------

- No incremental learning (the forest must be rebuilt from scratch when
  new examples are added)
- No streaming or online variant

References
----------

- Liu, F.T., Ting, K.M. and Zhou, Z.-H. (2008). Isolation Forest.
  *Proceedings of the 2008 Eighth IEEE International Conference on Data
  Mining*, 413-422. https://doi.org/10.1109/ICDM.2008.17

- Hariri, S., Kind, M.C. and Brunner, R.J. (2019). Extended Isolation
  Forest. *IEEE Transactions on Knowledge and Data Engineering*, 33(4),
  1479-1489. https://doi.org/10.1109/TKDE.2019.2947676

Usage
-----

To learn an isolation forest model from a dataset with default options:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model).

To learn with custom options:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model, [
            number_of_trees(200),
            subsample_size(128),
            extension_level(1),
            anomaly_threshold(0.6)
        ]).

To compute the anomaly score for a new instance:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model),
        isolation_forest::score(Model, [x-0.12, y-0.34], Score).

To predict whether an instance is an anomaly or normal:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model),
        isolation_forest::predict(Model, [x-4.50, y-4.20], Prediction).

To compute and rank anomaly scores for all instances in a dataset:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model),
        isolation_forest::score_all(gaussian_anomalies, Model, Scores).

The ``Scores`` list contains ``Id-Class-Score`` triples sorted by
descending anomaly score. This makes it easy to inspect top anomalies:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model),
        isolation_forest::score_all(gaussian_anomalies, Model, [Top1, Top2, Top3| _]).

To print a summary of the learned model:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model),
        isolation_forest::print_anomaly_detector(Model).

To use the original (non-extended) Isolation Forest, set the extension
level to 0:

::

   | ?- isolation_forest::learn(gaussian_anomalies, Model, [extension_level(0)]).
