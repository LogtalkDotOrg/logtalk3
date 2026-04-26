.. _library_elo:

``elo``
=======

Elo pairwise preference ranker.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from pairwise preferences, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#elo <../../apis/library_index.html#elo>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(elo(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(elo(tester)).

Features
--------

- **Pairwise Preference Learning**: Learns one deterministic rating per
  item from pairwise outcomes.
- **Deterministic Batch Elo Semantics**: Replays the dataset preference
  stream in enumeration order using the standard Elo expected-score
  formula.
- **Configurable Rating Parameters**: Exposes the initial rating,
  K-factor, and rating scale as user options.
- **Integer Weight Fidelity**: Preference weights must be positive
  integers and are interpreted as repeated unit outcomes.
- **Deterministic Ranking**: Orders candidate items by learned rating
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  comparison graphs.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.
- **Shared Ranking Infrastructure**: Reuses the ``ranking_protocols``
  helpers for option processing, dataset validation, diagnostics,
  export, and candidate ranking.

Rating semantics
----------------

This implementation uses a deterministic batch Elo interpretation over
the pairwise preference stream. Each preference is processed in dataset
enumeration order. For a winner with rating ``R_w`` and a loser with
rating ``R_l``, the expected winner score is computed as

::

   1 / (1 + 10^((R_l - R_w)/Scale))

and the winner receives a rating update of

::

   K * (1 - ExpectedWinnerScore)

with the loser receiving the symmetric negative update.

Because the current pairwise dataset protocol does not encode
timestamps, this is a deterministic batch Elo variant rather than a
literal historical competition-log replay.

Positive integer preference weights are replayed as repeated unit
outcomes. Datasets using non-integer preference weights are rejected
because they do not map cleanly to standard Elo update semantics.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise ranking dataset object
   | ?- elo::learn(my_dataset, Ranker).
   ...

   % Learn with custom Elo parameters
   | ?- elo::learn(my_dataset, Ranker, [initial_rating(1400.0), k_factor(24.0), rating_scale(200.0)]).
   ...

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect model, options, and dataset summary metadata
   | ?- elo::learn(my_dataset, Ranker),
        elo::diagnostics(Ranker, Diagnostics).
   Diagnostics = [...]
   ...

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

   % Rank a candidate set from most preferred to least preferred
   | ?- elo::learn(my_dataset, Ranker),
        elo::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
   | ?- elo::learn(my_dataset, Ranker),
        elo::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
   Clauses = [my_ranker(elo_ranker(...))]
   ...

   % Export to a file
   | ?- elo::learn(my_dataset, Ranker),
        elo::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
   ...

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``initial_rating(Rating)``: Initial rating assigned to every item.
- ``k_factor(KFactor)``: Positive Elo K-factor.
- ``rating_scale(Scale)``: Positive rating-scale denominator used in the
  expected-score formula.

Datasets supplied to the ranker must use positive integer preference
weights. Non-integer weights are rejected.

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   elo_ranker(Items, Ratings, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Rating`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options and dataset summary.

References
----------

1. Elo, A. E. (1978). *The Rating of Chessplayers, Past and Present*.
   Arco.
