.. _library_thurstone_mosteller_ranker:

``thurstone_mosteller_ranker``
==============================

Thurstone-Mosteller Case V pairwise preference ranker. It aggregates
pairwise outcomes into matchup probabilities, applies a fixed continuity
correction to avoid infinite probit values, transforms them with the
inverse standard normal CDF, and fits Case V latent utilities using a
deterministic weighted least-squares linear solve.

The library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It provides predicates for learning a
ranker from pairwise preferences, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``pairwise_ranking_dataset_protocol`` protocol from the
``ranking_protocols`` library. See the ``test_datasets`` directory for
examples. The current implementation requires a well-formed connected
pairwise dataset so that all learned utilities remain comparable across
the ranked items.

API documentation
-----------------

Open the
`../../apis/library_index.html#thurstone_mosteller_ranker <../../apis/library_index.html#thurstone_mosteller_ranker>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(thurstone_mosteller_ranker(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(thurstone_mosteller_ranker(tester)).

Features
--------

- **Pairwise Preference Learning**: Learns one deterministic latent
  utility per item from aggregated pairwise outcomes.
- **Case V Thurstone-Mosteller Fit**: Uses continuity-corrected matchup
  win probabilities transformed by the inverse standard normal CDF and
  fitted with a deterministic weighted least-squares solve.
- **Centered Utility Scale**: Learned scores are centered so that only
  utility differences encode preference strength while the absolute
  location remains arbitrary.
- **Deterministic Ranking**: Orders candidate items by learned utility
  with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared
  items, self-preferences, non-positive weights, and disconnected
  comparison graphs.
- **Training Diagnostics**: Learned rankers include metadata describing
  the fitting method, continuity correction, and validated dataset
  summary.
- **Ranker Export**: Learned rankers can be exported as self-contained
  terms.

Scoring semantics
-----------------

This implementation aggregates pairwise preferences into matchup totals
and then estimates latent utilities under the Thurstone-Mosteller Case V
model. For each observed unordered pair, it computes a
continuity-corrected empirical win probability

::

   (wins + 0.5) / (total + 1.0)

maps that probability through the inverse standard normal CDF, and fits
utility differences using weighted least squares with matchup totals as
the weights.

The resulting utilities are centered, real-valued scores on an arbitrary
additive scale. Only differences between utilities are meaningful when
interpreting implied paired-comparison probabilities and ranking order.

Options
-------

The current ``learn/3`` implementation does not define any user options
beyond the default empty list. Non-empty options lists are rejected.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(thurstone_mosteller_ranker),
       options(Options),
       fitting(weighted_least_squares_case_v),
       continuity_correction(0.5),
       dataset_summary(DatasetSummary)
   ]

Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

::

   thurstone_mosteller_ranker(Items, Scores, Diagnostics)

Where:

- ``Items``: List of ranked items.
- ``Scores``: List of ``Item-Score`` pairs.
- ``Diagnostics``: List of metadata terms, including the fitting method,
  continuity correction, and dataset summary.
