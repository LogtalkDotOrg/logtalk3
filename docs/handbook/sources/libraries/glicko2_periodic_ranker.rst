.. _library_glicko2_periodic_ranker:

``glicko2_periodic_ranker``
===========================

Multi-period Glicko-2 ranker over temporal pairwise game datasets.
Applies the standard Glicko-2 rating, rating-deviation, and volatility
update equations period by period using simultaneous player updates
inside each declared rating period.

This library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It learns one rating per item from
datasets implementing the ``temporal_pairwise_ranking_dataset_protocol``
protocol, processing declared rating periods in order and applying
simultaneous Glicko-2 player updates inside each period using the
standard Glicko-2 rating, rating-deviation, and volatility update
equations.

Draws are represented directly using game scores on the set
``{0.0, 0.5, 1.0}``. Players who are inactive in a declared period keep
their rating and volatility while their rating deviation is inflated for
that period.

Players are initialized when they first play instead of being forced to
appear in the first declared period.

The library provides predicates for learning a ranker from temporal
pairwise games, using it to order candidate items, and exporting it as a
list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``temporal_pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#glicko2_periodic_ranker <../../apis/library_index.html#glicko2_periodic_ranker>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

       | ?- logtalk_load(glicko2_periodic_ranker(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

       | ?- logtalk_load(glicko2_periodic_ranker(tester)).

Features
--------

- **Temporal Pairwise Game Learning**: Learns one deterministic rating
  per item from temporal pairwise game results.
- **Deterministic Multi-Period Glicko-2 Semantics**: Processes the
  declared rating periods in order and applies simultaneous Glicko-2
  player updates within each period.
- **Direct Draw Support**: Uses explicit game scores on the set
  ``{0.0, 0.5, 1.0}``, allowing wins, draws, and losses to be
  represented directly.
- **Inactive-Period Deviation Inflation**: Players who do not play in a
  declared period keep their rating and volatility while their rating
  deviation is inflated for that period.
- **Late Player Initialization**: Players are initialized when they
  first appear in the dataset instead of being forced to play in the
  first declared period.
- **Configurable Rating Parameters**: Exposes the initial rating,
  initial rating deviation, initial volatility, volatility constraint
  parameter, and volatility-solver tolerance as user options.
- **Deterministic Ranking**: Orders candidate items by learned rating
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate periods, unknown
  periods, undeclared items, self-games, illegal scores, and
  disconnected comparison graphs.
- **Extended Diagnostics**: Preserves per-item rating deviations,
  volatilities, processed period count, and final period metadata in the
  learned ranker diagnostics.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.
- **Shared Ranking Infrastructure**: Reuses the ``ranking_protocols``
  helpers for option processing, dataset validation, diagnostics access,
  export, and candidate ranking.

Rating semantics
----------------

This implementation uses the standard Glicko-2 update equations over
explicit rating periods supplied by the dataset. The declared periods
are processed in order, and all active players in a period are updated
simultaneously from the pre-period ratings, deviations, and
volatilities.

Game results are represented directly using scores in the set
``{0.0, 0.5, 1.0}``. A score of ``0.5`` denotes a draw. Players who are
inactive in a period keep their rating and volatility, but their rating
deviation is inflated for that period according to the Glicko-2
inactivity rule.

Players are initialized when they first appear in a game rather than
being required to occur in the first declared period. Items declared by
the dataset but never seen in a game are initialized after training so
they remain present in the learned ranker with the configured initial
parameters.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

       % Learn from a temporal pairwise ranking dataset object
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker).
       ...

       % Learn with custom Glicko-2 parameters
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker, [initial_rating(1400.0), initial_deviation(300.0), initial_volatility(0.07), tau(0.4)]).
       ...

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

       % Inspect model, options, deviations, volatilities, and period metadata
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker),
            glicko2_periodic_ranker::diagnostics(Ranker, Diagnostics).
       Diagnostics = [...]
       ...

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

       % Rank a candidate set from most preferred to least preferred
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker),
            glicko2_periodic_ranker::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker),
            glicko2_periodic_ranker::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
       Clauses = [my_ranker(glicko2_periodic_ranker(...))]
       ...

       % Export to a file
       | ?- glicko2_periodic_ranker::learn(my_dataset, Ranker),
            glicko2_periodic_ranker::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
       ...

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``initial_rating(Rating)``: Initial rating assigned to a player when
  it is first initialized.
- ``initial_deviation(Deviation)``: Initial rating deviation assigned to
  a player when it is first initialized.
- ``initial_volatility(Volatility)``: Initial volatility assigned to a
  player when it is first initialized.
- ``tau(Tau)``: Positive volatility-constraint parameter used by the
  Glicko-2 update.
- ``volatility_tolerance(Tolerance)``: Positive stopping tolerance used
  by the volatility root-finding iteration.

Datasets supplied to the ranker must use legal game scores from the set
``{0.0, 0.5, 1.0}``.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(glicko2_periodic_ranker),
       options(Options),
       rating_deviations(Deviations),
       volatilities(Volatilities),
       periods_processed(PeriodsProcessed),
       final_period(FinalPeriod),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   glicko2_periodic_ranker(Items, Ratings, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Ratings``: List of ``Item-Rating`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options, per-item rating deviations, per-item volatilities, processed
  period count, final period, and dataset summary.

References
----------

1. Glickman, M. E. (2012). *Example of the Glicko-2 system*.
