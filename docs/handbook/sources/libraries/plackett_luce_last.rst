.. _library_plackett_luce_last:

``plackett_luce_last``
======================

Tie-aware Plackett-Luce-last grouped-ranking ranker.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from grouped rankings, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``ranking_dataset_protocol`` protocol from the ``ranking_protocols``
library. See the ``test_datasets`` directory for examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#plackett_luce_last <../../apis/library_index.html#plackett_luce_last>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(plackett_luce_last(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(plackett_luce_last(tester)).

Features
--------

- **Grouped Last-Choice Learning**: Learns positive item strengths from
  grouped rankings by processing each group from lowest relevance to
  highest relevance.
- **Tie-Aware Likelihood**: Uses grouped tie blocks so equal relevance
  judgments are handled as unordered last-choice blocks instead of being
  broken arbitrarily.
- **Deterministic Ranking**: Orders candidate items by learned strength
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects malformed grouped datasets,
  unsupported options, duplicate candidates, and invalid ranker terms.
- **Regular MLE Fidelity**: Rejects grouped datasets whose strict-order
  graph does not admit a finite Plackett-Luce-last maximum-likelihood
  estimate instead of masking non-identifiability with implicit
  regularization.
- **Training Diagnostics**: Learned rankers include convergence,
  iteration, final update delta, and dataset summary metadata accessible
  using the ``diagnostics/2`` predicate.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.
- **Shared Grouped Infrastructure**: Reuses the shared grouped tie-block
  representation and iterative positive-strength scaffolding from the
  ``ranking_protocols`` library.

Dataset requirements
--------------------

This implementation requires more than grouped-dataset well-formedness.
In order to admit a finite Plackett-Luce-last maximum-likelihood
estimate, the directed strict-order graph induced by the grouped
rankings must be strongly connected. Intuitively, no partition of the
items may dominate all others in only one direction across the observed
groups.

Unlike ``borda``, this model therefore rejects grouped datasets that
consist of disconnected query universes or one-way dominance chains,
because those data do not identify a finite global strength scale.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a grouped ranking dataset object
   | ?- plackett_luce_last::learn(my_dataset, Ranker).
   ...

   % Learn with custom iteration and missing-relevance options
   | ?- plackett_luce_last::learn(my_dataset, Ranker, [maximum_iterations(500), tolerance(1.0e-7), missing_relevance(error)]).
   ...

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect convergence and dataset summary metadata
   | ?- plackett_luce_last::learn(my_dataset, Ranker),
        plackett_luce_last::diagnostics(Ranker, Diagnostics).
   Diagnostics = [...]
   ...

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(plackett_luce_last),
       options(Options),
       convergence(Status),
       iterations(Iterations),
       final_delta(FinalDelta),
       dataset_summary(DatasetSummary)
   ]

Where:

- ``model(plackett_luce_last)`` identifies the learning algorithm that
  produced the ranker.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
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
   | ?- plackett_luce_last::learn(my_dataset, Ranker),
        plackett_luce_last::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
   | ?- plackett_luce_last::learn(my_dataset, Ranker),
        plackett_luce_last::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
   Clauses = [my_ranker(plackett_luce_last_ranker(...))]
   ...

   % Export to a file
   | ?- plackett_luce_last::learn(my_dataset, Ranker),
        plackett_luce_last::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
   ...

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``maximum_iterations(MaximumIterations)``: Positive integer iteration
  bound.
- ``tolerance(Tolerance)``: Positive convergence tolerance.
- ``missing_relevance(zero|error)``: Policy used when a declared item in
  a group has no explicit relevance judgment.

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   plackett_luce_last_ranker(Items, Strengths, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of normalized ``Item-Strength`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options, convergence status, iteration count, final update delta, and
  dataset summary.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this ranker term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

See also
--------

For the complementary grouped top-choice variant over the same dataset
protocol, see the ``plackett_luce`` library. For a deterministic
non-probabilistic grouped-ranking baseline over the same dataset
protocol, see the ``borda`` library.

References
----------

1. Plackett, R. L. (1975). The analysis of permutations. *Applied
   Statistics*, 24(2), 193-202.
2. Luce, R. D. (1959). *Individual Choice Behavior: A Theoretical
   Analysis*. Wiley.
3. Hunter, D. R. (2004). MM algorithms for generalized Bradley-Terry
   models. *The Annals of Statistics*, 32(1), 384-406.
