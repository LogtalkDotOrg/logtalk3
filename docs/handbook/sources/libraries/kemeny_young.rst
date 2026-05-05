.. _library_kemeny_young:

``kemeny_young``
================

Kemeny-Young pairwise preference ranker. It aggregates head-to-head
preference weights, then performs an exact branch-and-bound search over
linear orders to maximize the total pairwise agreement score under the
Kemeny-Young criterion.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning an
exact consensus ranker from pairwise preferences, using it to order
candidate items, inspecting the learned consensus ranking and consensus
score, and exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. The current implementation requires a well-formed connected
pairwise dataset so that all ranked items remain part of a single
comparison graph.

API documentation
-----------------

Open the
`../../apis/library_index.html#kemeny_young <../../apis/library_index.html#kemeny_young>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(kemeny_young(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(kemeny_young(tester)).

Features
--------

- **Exact Consensus Ranking**: Learns one deterministic Kemeny-Young
  consensus order from aggregated pairwise outcomes.
- **Branch-and-Bound Search**: Uses exact search with pruning over
  linear orders to maximize total pairwise agreement.
- **Deterministic Optimal Tie Breaking**: Supports both standard
  term-order and dataset declaration-order selection when multiple
  optimal consensus rankings exist.
- **Consensus Access**: Exposes the learned full-item consensus ranking
  and its maximum agreement score using the ``consensus_ranking/2`` and
  ``consensus_score/2`` predicates.
- **Deterministic Ranking**: Orders candidate items by the selected
  consensus order with deterministic tie-breaking. When multiple optimal
  Kemeny orders exist, the ``tie_breaking/1`` option selects the
  deterministic search order used to choose one representative consensus
  ranking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  comparison graphs.
- **Training Diagnostics**: Learned rankers include dataset summary
  metadata, the effective tie-breaking mode, the selected consensus
  ranking, and the maximum agreement score.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.

Usage
-----

Learning a ranker
~~~~~~~~~~~~~~~~~

::

   % Learn from a pairwise ranking dataset object
   | ?- kemeny_young::learn(my_dataset, Ranker).
   ...

   % Learn with custom optimal-order tie breaking
   | ?- kemeny_young::learn(my_dataset, Ranker, [tie_breaking(declaration_order)]).
   ...

Inspecting the learned consensus
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   % Inspect the learned full-item consensus ranking
   | ?- kemeny_young::learn(my_dataset, Ranker),
        kemeny_young::consensus_ranking(Ranker, ConsensusRanking).
   ConsensusRanking = [...]
   ...

   % Inspect the maximum pairwise agreement score
   | ?- kemeny_young::learn(my_dataset, Ranker),
        kemeny_young::consensus_score(Ranker, ConsensusScore).
   ConsensusScore = ...
   ...

Ranking candidate items
~~~~~~~~~~~~~~~~~~~~~~~

::

   % Rank a candidate set from most preferred to least preferred
   | ?- kemeny_young::learn(my_dataset, Ranker),
        kemeny_young::rank(Ranker, [item_a, item_b, item_c], Ranking).
   Ranking = [...]
   ...

Candidate lists must be proper lists of unique, ground items declared by
the training dataset. Invalid ranker terms, duplicate candidates, and
candidates containing variables are rejected with errors instead of
being silently accepted.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``tie_breaking(term_order)``: Select the lexicographically earliest
  optimal consensus ranking using the standard term order of the item
  identifiers.
- ``tie_breaking(declaration_order)``: Select the earliest optimal
  consensus ranking using the dataset declaration order preserved by the
  pairwise dataset helpers.

The default is ``tie_breaking(term_order)``.

Scoring semantics
-----------------

The learned ``scores/2`` values are reverse consensus positions. For a
learned consensus order with ``N`` items, the first item receives score
``N-1`` and the last item receives score ``0``.

This positional encoding is a deterministic surrogate used to satisfy
the shared ``ranker_protocol`` scoring interface. The authoritative
Kemeny-Young output is the full ``consensus_ranking/2`` order together
with its ``consensus_score/2`` agreement value.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(kemeny_young),
       options(Options),
       consensus_ranking(ConsensusRanking),
       consensus_score(ConsensusScore),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   kemeny_young_ranker(Items, Scores, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Score`` pairs.
- ``Diagnostics``: List of metadata terms, including the effective
  options, selected consensus ranking, maximum agreement score, and
  dataset summary.

Notes
-----

Kemeny-Young optimization is NP-hard in general. This implementation
uses an exact branch-and-bound search and is intended for small to
moderate item sets.

References
----------

1. Kemeny, J. G. (1959). *Mathematics without numbers*.
2. Young, H. P. (1988). *Condorcet's theory of voting*.
