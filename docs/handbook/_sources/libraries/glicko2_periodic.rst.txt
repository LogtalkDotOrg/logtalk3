.. _library_glicko2_periodic:

``glicko2_periodic``
====================

Multi-period Glicko-2 ranker over temporal pairwise game datasets.

This library implements the ``ranker_protocol`` defined in the
``ranking_protocols`` library. It learns one rating per item from
datasets implementing the ``temporal_pairwise_ranking_dataset_protocol``
protocol, processing declared rating periods in order and applying
simultaneous Glicko-2 player updates inside each period.

Draws are represented directly using game scores on the set
``{0.0, 0.5, 1.0}``. Players who are inactive in a declared period keep
their rating and volatility while their rating deviation is inflated for
that period.

Load with:

::

   | ?- logtalk_load(glicko2_periodic(loader)).

Test with:

::

   | ?- logtalk_load(glicko2_periodic(tester)).

The supported options are ``initial_rating/1``, ``initial_deviation/1``,
``initial_volatility/1``, ``tau/1``, and ``volatility_tolerance/1``.
