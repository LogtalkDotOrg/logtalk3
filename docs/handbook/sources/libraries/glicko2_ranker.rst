.. _library_glicko2_ranker:

``glicko2_ranker``
==================

Glicko-2 pairwise preference ranker. It applies the standard Glicko-2
rating, rating-deviation, and volatility update equations over a single
synthetic rating period built from the aggregated pairwise outcomes of
the dataset.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from pairwise preferences, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. The current implementation requires a well-formed connected
pairwise dataset so that learned ratings remain globally comparable
across all ranked items.

API documentation
-----------------

Open the
`../../apis/library_index.html#glicko2_ranker <../../apis/library_index.html#glicko2_ranker>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(glicko2_ranker(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(glicko2_ranker(tester)).

Features
--------

- **Pairwise Preference Learning**: Learns one deterministic rating per
  item from pairwise outcomes.
- **Deterministic Single-Period Glicko-2 Semantics**: Applies the
  standard Glicko-2 rating-period update equations over one synthetic
  rating period built from the aggregated pairwise results.
- **Configurable Rating Parameters**: Exposes the initial rating,
  initial rating deviation, initial volatility, volatility constraint
  parameter, and volatility-solver tolerance as user options.
- **Integer Weight Fidelity**: Preference weights must be positive
  integers and are interpreted as repeated unit outcomes inside the same
  rating period.
- **Deterministic Ranking**: Orders candidate items by learned rating
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  comparison graphs.
- **Extended Diagnostics**: Preserves per-item rating deviations and
  volatilities in the learned ranker diagnostics.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.
- **Shared Ranking Infrastructure**: Reuses the ``ranking_protocols``
  helpers for option processing, dataset validation, diagnostics access,
  export, and candidate ranking.

Rating semantics
----------------

This implementation uses a deterministic batch interpretation of
Glicko-2. The whole dataset is treated as a single synthetic rating
period because the current pairwise dataset protocol does not encode
timestamps or rating-period boundaries.

The implementation converts ratings and rating deviations to the
internal Glicko-2 scale, applies the standard opponent-scaling factor
``g(phi)``, expected-score function, variance term ``v``, improvement
estimate ``delta``, and volatility update iteration, and then converts
the updated ratings and deviations back to the conventional Glicko
scale.

Positive integer preference weights are replayed as repeated unit
outcomes. Datasets using non-integer preference weights are rejected
because they do not map cleanly to standard Glicko-2 update semantics.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise ranking dataset object
   | ?- glicko2_ranker::learn(my_dataset, Ranker).
   ...

   % Learn with custom Glicko-2 parameters
   | ?- glicko2_ranker::learn(my_dataset, Ranker, [initial_rating(1400.0), initial_deviation(300.0), initial_volatility(0.07), tau(0.4)]).
   ...

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect model, options, deviations, volatilities, and dataset metadata
   | ?- glicko2_ranker::learn(my_dataset, Ranker),
        glicko2_ranker::diagnostics(Ranker, Diagnostics).
   Diagnostics = [...]
   ...

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

   % Rank a candidate set from most preferred to least preferred
   | ?- glicko2_ranker::learn(my_dataset, Ranker),
        glicko2_ranker::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
   | ?- glicko2_ranker::learn(my_dataset, Ranker),
        glicko2_ranker::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
   Clauses = [my_ranker(glicko2_ranker(...))]
   ...

   % Export to a file
   | ?- glicko2_ranker::learn(my_dataset, Ranker),
        glicko2_ranker::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
   ...

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``initial_rating(Rating)``: Initial rating assigned to every item.
- ``initial_deviation(Deviation)``: Initial rating deviation assigned to
  every item.
- ``initial_volatility(Volatility)``: Initial volatility assigned to
  every item.
- ``tau(Tau)``: Positive volatility-constraint parameter used by the
  Glicko-2 update.
- ``volatility_tolerance(Tolerance)``: Positive stopping tolerance used
  by the volatility root-finding iteration.

Datasets supplied to the ranker must use positive integer preference
weights. Non-integer weights are rejected.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(glicko2_ranker),
       options(Options),
       rating_deviations(Deviations),
       volatilities(Volatilities),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   glicko2_ranker(Items, Ratings, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Ratings``: List of ``Item-Rating`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options, per-item rating deviations, per-item volatilities, and
  dataset summary.

References
----------

1. Glickman, M. E. (2012). *Example of the Glicko-2 system*.
