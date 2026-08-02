.. _library_trueskill_ranker:

order is preserved within equal ranks.

API documentation
-----------------

Open the
`../../apis/library_index.html#trueskill_ranker <../../apis/library_index.html#trueskill_ranker>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(trueskill_ranker(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(trueskill_ranker(tester)).

Dataset representation
----------------------

Datasets define:

::

   item(Player).
   match(Match).
   team(Match, Team, Rank).
   team_member(Match, Team, Player, Weight).

Lower non-negative integer ranks are better. Equal ranks denote a draw
and ranks need not be contiguous. Participation weights must be greater
than zero and no greater than one. Team identifiers are local to a
match. Every match must contain at least two non-empty teams and a
player may occur only once in each match.

Options
-------

The values shown below are the defaults. The effective options,
including defaults not explicitly supplied to ``learn/3``, are recorded
in the learned ranker diagnostics.

- ``initial_mean(25.0)``: Mean of the Gaussian prior assigned to every
  declared player. Any number is accepted.
- ``initial_deviation(8.333333333333334)``: Standard deviation of the
  Gaussian skill prior. The value must be a number greater than zero.
  Larger values express greater initial uncertainty and allow early
  results to move ratings further.
- ``performance_deviation(4.166666666666667)``: Standard deviation of
  player performance around latent skill, conventionally called beta in
  TrueSkill. The value must be a number greater than zero. Larger values
  treat match outcomes as noisier evidence of skill.
- ``dynamics_factor(0.08333333333333333)``: Standard deviation added as
  variance before each match for participating players, conventionally
  called tau. The value must be a non-negative number. Zero models
  static skills; larger values allow skills to change more rapidly
  between matches.
- ``draw_probability(0.10)``: Prior probability used to derive draw
  margins. The value must be a number in the half-open interval
  ``[0.0, 1.0)``. Zero gives a zero draw margin; larger values make
  observed draws less surprising.
- ``conservative_multiplier(3.0)``: Number of posterior standard
  deviations subtracted from a posterior mean to compute the native
  score returned by ``scores/2``. The value must be a non-negative
  number. Zero ranks by posterior mean; larger values penalize uncertain
  ratings more strongly.
- ``maximum_iterations(100)``: Maximum expectation-propagation sweeps
  performed for each match. The value must be a positive integer.
  Reaching the limit produces a ranker with
  ``convergence(not_converged)`` rather than an error.
- ``tolerance(1.0e-6)``: Convergence threshold for the largest mean or
  deviation update in a match sweep. The value must be a number greater
  than zero.

Scores and diagnostics
----------------------

The learned ranker is represented as:

::

   trueskill_ranker(Items, Exposures, Diagnostics)

The native score returned by ``scores/2`` is the conservative exposure
``Mean - ConservativeMultiplier * Deviation``. Posterior means and
deviations are available as ``skill_means/1`` and ``skill_deviations/1``
diagnostics. Additional diagnostics report options, matches processed,
convergence, iteration counts, the maximum final update delta, and the
dataset summary.

Declared players that never participate retain the configured prior.
Disconnected participation components are valid and are learned
independently.

Limitations
-----------

- This library implements the original TrueSkill model, not TrueSkill 2.
- Match declaration order is semantically significant. Training is an
  ordered replay of all matches rather than an order-independent batch
  fit.
- Prior and model parameters are global. Per-player priors, per-match
  draw probabilities, handicaps, and other player- or event-specific
  parameters are not supported.
- Matches have no timestamps or rating periods. The dynamics factor is
  applied once to participating players before each match and does not
  account for elapsed time or inactive periods.
- Learning always constructs a new ranker from a dataset. There is no
  mutable or incremental API for updating an existing ranker with one
  additional match.
- The library ranks players and exposes posterior skill parameters, but
  does not currently provide match-quality or win-probability prediction
  APIs.

References
----------

1. Herbrich, R., Minka, T., and Graepel, T. (2007). *TrueSkill: A
   Bayesian Skill Rating System*.
