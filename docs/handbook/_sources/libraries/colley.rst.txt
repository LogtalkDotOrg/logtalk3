.. _library_colley:

``colley``
==========

Colley pairwise preference ranker.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from pairwise preferences, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. Requires well-formed connected pairwise datasets so that the
learned rankings remain globally comparable across all ranked items.

API documentation
-----------------

Open the
`../../apis/library_index.html#colley <../../apis/library_index.html#colley>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(colley(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(colley(tester)).

Features
--------

- **Pairwise Preference Learning**: Learns one deterministic rating per
  item from aggregated pairwise outcomes.
- **Colley Matrix Fidelity**: Solves the standard Colley linear system
  with diagonal regularization ``2 + games_i`` and off-diagonal entries
  ``-games_ij``.
- **Numerically Hardened Solver**: Uses Gaussian elimination with
  partial pivoting plus residual checks before accepting the learned
  ratings.
- **Conservative Ratings**: Produces ratings in the interval ``[0,1]``
  with the neutral prior centered at ``0.5``.
- **Deterministic Ranking**: Orders candidate items by learned rating
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  comparison graphs.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.
- **Shared Ranking Infrastructure**: Reuses the ``ranking_protocols``
  helpers for dataset validation, diagnostics, export, and candidate
  ranking.

Scoring semantics
-----------------

This implementation aggregates pairwise preferences into matchup totals
and then solves the Colley system

::

   C r = b

where ``C_ii = 2 + games_i``, ``C_ij = -games_ij`` for ``i \= j``, and
``b_i = 1 + (wins_i - losses_i) / 2``.

The linear system is solved using deterministic Gaussian elimination
with partial pivoting. The implementation also verifies that the
recovered solution has a small residual and only clamps negligible
floating-point boundary noise at ``0.0`` or ``1.0``; materially invalid
out-of-range values are rejected instead of being silently accepted.

The resulting ratings remain close to ``0.5`` when evidence is weak and
move toward ``1.0`` or ``0.0`` only when repeated pairwise results
justify doing so.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise ranking dataset object
   | ?- colley::learn(my_dataset, Ranker).
   ...

   % Learn with an explicit empty options list
   | ?- colley::learn(my_dataset, Ranker, []).
   ...

The current implementation accepts only the empty options list ``[]``.
Any non-empty options list is rejected.

Inspecting diagnostics
~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect model and dataset summary metadata
   | ?- colley::learn(my_dataset, Ranker),
        colley::diagnostics(Ranker, Diagnostics).
   Diagnostics = [...]
   ...

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

   % Rank a candidate set from most preferred to least preferred
   | ?- colley::learn(my_dataset, Ranker),
        colley::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
   | ?- colley::learn(my_dataset, Ranker),
        colley::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
   Clauses = [my_ranker(colley_ranker(...))]
   ...

   % Export to a file
   | ?- colley::learn(my_dataset, Ranker),
        colley::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
   ...

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(colley),
       options(Options),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   colley_ranker(Items, Ratings, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Rating`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options and dataset summary.

References
----------

1. Colley, W. N. (2002). *Colley's bias free college football ranking
   method: The Colley Matrix explained*.
