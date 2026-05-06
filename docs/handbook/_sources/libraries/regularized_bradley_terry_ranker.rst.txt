.. _library_regularized_bradley_terry_ranker:

``regularized_bradley_terry_ranker``
====================================

Regularized Bradley-Terry MAP pairwise preference ranker. It uses a
deterministic MM-style posterior-mode update for a Bradley-Terry
likelihood regularized by an explicit independent Gamma prior over item
strengths.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from pairwise preferences, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. The training dataset must declare each ranked item once,
enumerate positive-weight pairwise preferences between distinct declared
items, and induce a connected undirected comparison graph. Unlike the
unregularized Bradley-Terry model, the directed win graph is not
required to be strongly connected.

API documentation
-----------------

Open the
`../../apis/library_index.html#regularized_bradley_terry_ranker <../../apis/library_index.html#regularized_bradley_terry_ranker>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(regularized_bradley_terry_ranker(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(regularized_bradley_terry_ranker(tester)).

Features
--------

- **Pairwise Preference Learning**: Learns positive item strengths from
  weighted head-to-head outcomes.

- **Explicit Gamma Prior**: Uses a separate MAP model with an explicit
  independent Gamma prior over item strengths.

- **Finite Regularized Fit**: Admits connected pairwise datasets whose
  directed win graph is not strongly connected, instead of rejecting
  them as non-identifiable maximum-likelihood problems.

- **Deterministic Ranking**: Orders candidate items by learned strength
  with deterministic tie-breaking.

- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  undirected comparison graphs.

- **Training Diagnostics**: Learned rankers include the effective Gamma
  prior, convergence, iteration, and dataset summary metadata that can
  be accessed using the ``diagnostics/2`` predicate.

- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.

- **Shared Pairwise Infrastructure**: Reuses the shared pairwise
  preprocessing and MM-iteration scaffolding from the
  ``ranking_protocols`` library.

Dataset requirements
--------------------

This implementation still requires the undirected comparison graph
induced by the preferences to be connected, but unlike the unregularized
Bradley-Terry model it does not require the directed win graph to be
strongly connected. The explicit Gamma prior regularizes dominance
partitions and one-way chains, yielding a finite posterior mode for
connected datasets that would otherwise fail the maximum-likelihood
existence check.

For the original unregularized maximum-likelihood Bradley-Terry model,
which keeps the same pairwise-preference interface but requires the
directed win graph to be strongly connected, see the
``bradley_terry_ranker`` library.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise ranking dataset object
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker).
   ...

   % Learn with custom iteration and Gamma-prior options
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker, [maximum_iterations(500), tolerance(1.0e-7), gamma_prior(gamma(3.0, 4.0))]).
   ...

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect convergence, prior, and dataset summary metadata
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker),
        regularized_bradley_terry_ranker::diagnostics(Ranker, Diagnostics).
   Diagnostics = [...]
   ...

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(regularized_bradley_terry_ranker),
       options(Options),
       prior(gamma(Shape, Rate)),
       convergence(Status),
       iterations(Iterations),
       final_delta(FinalDelta),
       dataset_summary(DatasetSummary)
   ]

Where:

- ``model(regularized_bradley_terry_ranker)`` identifies the learning
  algorithm that produced the ranker.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
- ``prior(gamma(Shape, Rate))`` stores the effective Gamma prior
  hyperparameters.
- ``convergence(Status)`` records the training stop condition. The
  current values are ``converged`` and ``maximum_iterations_exhausted``.
- ``iterations(Iterations)`` stores the number of update iterations that
  were executed.
- ``final_delta(FinalDelta)`` stores the maximum absolute strength
  update in the last iteration.
- ``dataset_summary(DatasetSummary)`` stores a summary list describing
  the validated training dataset.

Use the ``ranking_protocols`` ``diagnostic/2`` and ``ranker_options/2``
helper predicates when you only need a single metadata term or the
effective options.

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

   % Rank a candidate set from most preferred to least preferred
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker),
        regularized_bradley_terry_ranker::rank(Ranker, [item_a, item_b, item_c], Ranking).
   Ranking = [...]
   ...

Candidate lists must be proper lists of unique, ground items declared by
the training dataset. Invalid ranker terms, duplicate candidates, and
candidates containing variables are rejected with errors instead of
being silently accepted.

Exporting the ranker
~~~~~~~~~~~~~~~~~~~~

Learned rankers can be exported as a list of clauses or to a file for
later use.

::

   % Export as predicate clauses
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker),
        regularized_bradley_terry_ranker::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
   Clauses = [my_ranker(regularized_bt_ranker(...))]
   ...

   % Export to a file
   | ?- regularized_bradley_terry_ranker::learn(my_dataset, Ranker),
        regularized_bradley_terry_ranker::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
   ...

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``maximum_iterations(MaximumIterations)``: Positive integer iteration
  bound.
- ``tolerance(Tolerance)``: Positive convergence tolerance.
- ``gamma_prior(gamma(Shape, Rate))``: Gamma-prior hyperparameters. The
  current implementation requires ``Shape > 1`` and ``Rate > 0``.

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   regularized_bt_ranker(Items, Strengths, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Strength`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options, Gamma prior, convergence status, iteration count, final
  update delta, and dataset summary.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this ranker term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete
   block designs: I. The method of paired comparisons. *Biometrika*,
   39(3/4), 324-345.
2. Hunter, D. R. (2004). MM algorithms for generalized Bradley-Terry
   models. *The Annals of Statistics*, 32(1), 384-406.
3. Caron, F. and Doucet, A. (2012). Efficient Bayesian inference for
   generalized Bradley-Terry models. *Journal of Computational and
   Graphical Statistics*, 21(1), 174-196.
