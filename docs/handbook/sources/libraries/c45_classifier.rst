.. _library_c45_classifier:

``c45_classifier``
==================

This library implements the C4.5 decision tree learning algorithm. C4.5
is an extension of the ID3 algorithm that uses information gain ratio
instead of information gain for attribute selection, which avoids bias
towards attributes with many values (see below for implementation
details).

The library implements the ``classifier_protocol`` defined in the
``classification_protocols`` library. It provides predicates for
learning a decision tree from a dataset, optionally prune it, using it
to make predictions, and exporting it as a list of predicate clauses or
to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classification_protocols``
library. See its ``test_datasets`` directory for examples. Attribute
declarations must be unique, datasets must be non-empty, and every
example must contain all declared attribute bindings. Use an unbound
variable for a missing attribute value.

API documentation
-----------------

Open the
`../../apis/library_index.html#c45-classifier <../../apis/library_index.html#c45-classifier>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(c45_classifier(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(c45_classifier(tester)).

Implemented features
--------------------

- Information gain ratio for attribute selection: only usable candidates
  with information gain at least the average usable gain are eligible.
  Usable zero-gain splits remain eligible when all usable gains are
  zero, allowing interactions such as XOR to be learned. Constant and
  all-missing attributes are excluded.
- Handling of discrete (categorical) attributes with multi-way splits
- Handling of continuous (numeric) attributes with binary threshold
  splits: each attribute's threshold maximizes information gain. Values
  are sorted using ``sort/2``. Each continuous attribute's known
  training values should use a consistent representation: all integers
  or all floats. Threshold comparisons use the built-in arithmetic
  operators. Midpoints avoid addition overflow; boundaries without a
  separating float according to those operators are skipped.
- Handling of missing attribute values (represented using anonymous
  variables): examples are allocated fractionally in proportion to known
  branch populations, preserving their total weight. Information gain is
  discounted by the known population fraction, and split information
  includes the missing population.
- Tree pruning using pessimistic error pruning (PEP): estimates error
  rates using the upper confidence bound of the binomial distribution
  (Wilson score interval) with a configurable confidence factor (default
  ``0.25``) and minimum weighted node population (default ``2``). A
  non-empty internal node below the minimum becomes a majority leaf;
  otherwise a subtree is replaced when the leaf estimate is no worse.
  Existing leaves and empty branches are preserved. The confidence
  factor must be a float in ``(0.0, 0.5]``, and the minimum must be a
  positive integer.
- Export of learned decision trees as predicate clauses
- Pretty-printing of learned decision trees

Learned model representation
----------------------------

The ``learn/2-3`` predicates return ``c45_model(Tree, Statistics)``. The
``Tree`` argument contains the decision tree, and ``Statistics``
contains weighted class counts and branch populations used for
missing-value prediction.

For discrete attributes, the learned decision tree is represented as
``leaf(Class)`` for leaf nodes and ``tree(Attribute, Subtrees)`` for
internal nodes with discrete attributes, where ``Subtrees`` is a list of
``Value-Subtree`` pairs.

For continuous (numeric) attributes, the tree uses binary threshold
splits represented as
``tree(Attribute, threshold(Threshold), LeftSubtree, RightSubtree)``
where ``LeftSubtree`` corresponds to values ``=< Threshold`` and
``RightSubtree`` to values ``> Threshold``.

The statistics returned by the ``learn/2-3`` and ``prune/3-5``
predicates follow the tree structure:

leaf_stats(Counts) threshold_stats(Counts, LeftMass, LeftStatistics,
RightMass, RightStatistics) discrete_stats(Counts, Branches)

The ``Counts`` argument is a list of distinct ``Class-PositiveFloat``
pairs in standard class term order. Each discrete branch is represented
by ``branch(Value, Mass, Statistics)``. An empty branch has zero mass
and inherits its parent's counts for its fallback leaf. Learned class
ties use standard term order; equally scored splits retain attribute
declaration order, and equally scored thresholds retain numeric boundary
order.

The ``predict/3`` predicate follows known values directly. An absent or
unbound attribute aggregates class distributions using branch population
weights; an unseen categorical value uses the current node's class
distribution. Predictions do not instantiate missing values. The
``diagnostics/2`` predicate reports the model identifier and sorted root
classes for annotated models.

Raw ``leaf/1`` and ``tree/2-4`` terms remain accepted for prediction,
validation, pruning, printing, and export. They do not carry population
statistics, so missing-value prediction retains legacy branch-majority
voting and requires the attribute binding to be present. Raw-tree
pruning returns a raw tree; annotated-model pruning returns an annotated
model.

Export
------

The ``export_to_clauses/4`` and ``export_to_file/4`` predicates export
one positional argument per declared attribute, followed by the
predicted class. Annotated models export a clause delegating to
``c45_classifier::predict/3``, preserving weighted missing-value
prediction, unseen-category fallback, and variable sharing. The
``c45_classifier`` object must therefore be loaded when calling the
exported predicate, and the clause must be compiled as Logtalk source.

Raw-tree exports contain standalone tree-path clauses and do not require
the classifier object. Numeric guards use the built-in arithmetic
comparison operators; categorical values such as ``=<(2)`` and ``>(2)``
remain categorical values. Raw path exports are intended for known
attribute values, not legacy missing-value voting.

Implementation
--------------

Continuous split scoring sorts once per attribute and sweeps grouped
values with incremental class histograms. Discrete scoring shares
grouped entropy and population statistics. Node entropy and selected
partitions are reused, and model statistics are built alongside the
tree. Pruning returns error estimates and statistics bottom-up instead
of rescanning descendants. Raw export builds clause lists using
difference lists.

Instance lookup retains the list representation. A per-instance balanced
index was measured but not adopted: its construction overhead outweighs
lookup savings for small instances and early attribute bindings.

Limitations
-----------

- No incremental learning (the tree must be rebuilt from scratch when
  new examples are added)

References
----------

- Quinlan, J.R. (1986). Induction of Decision Trees. *Machine Learning*,
  1(1), 81-106. https://doi.org/10.1007/BF00116251

- Quinlan, J.R. (1993). *C4.5: Programs for Machine Learning*. Morgan
  Kaufmann Publishers. https://doi.org/10.1016/C2009-0-27846-9

- Mitchell, T.M. (1997). *Machine Learning*. McGraw-Hill. Chapter 3:
  Decision Tree Learning.

Usage
-----

To learn a decision tree from a dataset:

::

   | ?- c45_classifier::learn(play_tennis, Tree).

To prune a learned tree with the default parameters (confidence factor
0.25, minimum weighted node population 2):

::

   | ?- c45_classifier::learn(breast_cancer, Tree),
        c45_classifier::prune(breast_cancer, Tree, PrunedTree).

To prune a learned tree with both custom confidence factor and minimum
weighted node population:

::

   | ?- c45_classifier::learn(breast_cancer, Tree),
        c45_classifier::prune(breast_cancer, Tree, 0.1, 3, PrunedTree).

To export the tree as a list of predicate clauses:

::

   | ?- c45_classifier::learn(play_tennis, Tree),
      c45_classifier::export_to_clauses(play_tennis, Tree, classify, Clauses).

To export the tree to a file:

::

   | ?- c45_classifier::learn(play_tennis, Tree),
      c45_classifier::export_to_file(play_tennis, Tree, classify, 'tree.lgt').

To print the tree to the current output:

::

   | ?- c45_classifier::learn(play_tennis, Tree),
      c45_classifier::print_classifier(Tree).

To predict the class for a new instance (as a list of attribute-value
pairs):

::

   | ?- c45_classifier::learn(play_tennis, Tree),
        c45_classifier::predict(Tree, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).
