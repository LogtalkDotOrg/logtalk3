.. _library_hodge_rank:

``hodge_rank``
==============

HodgeRank pairwise measurement ranker. It builds a weighted graph
Laplacian from the pairwise measurement support graph, solves the
anchored normal equations using deterministic Gaussian elimination with
partial pivoting and residual validation, and computes edge residuals
against the fitted score differences.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from weighted signed pairwise measurements, using it to order
candidate items, inspecting the resulting edge residuals, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_measurement_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. The current implementation requires a well-formed connected
pairwise measurement dataset so that learned scores remain globally
comparable across all ranked items.

API documentation
-----------------

Open the
`../../apis/library_index.html#hodge_rank <../../apis/library_index.html#hodge_rank>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hodge_rank(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hodge_rank(tester)).

Features
--------

- **Weighted Measurement Learning**: Learns one deterministic score per
  item from weighted signed pairwise measurements instead of
  winner/loser counts.
- **HodgeRank Global Component**: Solves the weighted graph-Laplacian
  least- squares system with a zero-sum anchoring convention.
- **Residual Inspection**: Exposes edge residuals through the
  ``residuals/2`` predicate so the non-global residual edge flow can be
  inspected explicitly.
- **Numerically Hardened Solver**: Uses Gaussian elimination with
  partial pivoting plus residual checks before accepting the learned
  scores.
- **Deterministic Ranking**: Orders candidate items by learned score
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-measurements, non-numeric values, non-positive weights,
  and disconnected support graphs.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.

Scoring semantics
-----------------

This implementation interprets each measurement fact

::

   measurement(Item1, Item2, Value, Weight)

as a weighted signed observation for the oriented edge
``Item1 -> Item2``. The learner fits a global zero-sum score vector
``s`` by minimizing the weighted least-squares objective

::

   sum_ij Weight_ij * (Value_ij - (s_i - s_j))^2

subject to ``sum(scores) = 0``. The resulting normal equations are a
weighted graph-Laplacian system with one anchoring row enforcing the
zero-sum gauge.

The learned scores are relative rather than probabilistic: only their
differences and ordering matter. Larger positive values indicate items
whose fitted global potential is higher than average. The
``residuals/2`` predicate returns the unexplained part of each observed
edge measurement after removing the fitted score difference, i.e. the
non-global residual edge flow left after fitting the global component.

Residual semantics
------------------

Residuals capture the part of each observed edge measurement that is not
explained by the fitted global score difference, exposing the non-global
residual edge flow left after fitting the global component.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise measurement dataset object
   | ?- hodge_rank::learn(my_dataset, Ranker).
   ...

   % Learn with an explicit empty options list
   | ?- hodge_rank::learn(my_dataset, Ranker, []).
   ...

The current implementation accepts only the empty options list ``[]``.
Any non-empty options list is rejected.

Inspecting residuals
~~~~~~~~~~~~~~~~~~~~

::

   % Inspect edge residuals from the fitted global scores
   | ?- hodge_rank::learn(my_dataset, Ranker),
        hodge_rank::residuals(Ranker, Residuals).
   Residuals = [...]
   ...

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(hodge_rank),
       options(Options),
       residuals(Residuals),
       residual_norm(ResidualNorm),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   hodge_rank_ranker(Items, Scores, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Score`` pairs.
- ``Diagnostics``: List of metadata terms, including the residuals,
  residual norm, effective options, and dataset summary.

References
----------

1. Jiang, X., Lim, L.-H., Yao, Y., & Ye, Y. (2011). *Statistical ranking
   and combinatorial Hodge theory*.
