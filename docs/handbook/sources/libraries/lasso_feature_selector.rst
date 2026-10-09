.. _library_lasso_feature_selector:

``lasso_feature_selector``
==========================

This library selects original features using the coefficients learned by
``lasso_regression``. It implements ``feature_selector_protocol``
through ``feature_selector_common``. Training delegates to the existing
regression learner without changing its optimizer, objective, encoding,
or convergence controls. Classification targets are not accepted.

API documentation
-----------------

Open the
`../../apis/library_index.html#lasso-feature-selector <../../apis/library_index.html#lasso-feature-selector>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(lasso_feature_selector(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(lasso_feature_selector(tester)).

Dataset and adapter
-------------------

The ``learn/2`` and ``learn/3`` predicates accept a dataset implementing
``feature_dataset_protocol``. Shared validation checks all original
examples, including rows with unbound targets, for duplicate
declarations, undeclared or repeated feature bindings, proper feature
lists, and consistent example counts. The selector checks that every
remaining target is numeric and that at least one usable example
remains.

The parametric ``regression_dataset_adapter(Dataset)`` object implements
``regression_dataset_protocol``. The ``attribute_values/2`` predicate
forwards the declarations after checking that names are distinct atoms
and domains are ``continuous`` or non-empty proper lists of distinct
atomic categorical values. The atom-name restriction follows the
regression learner contract. The ``example/3`` predicate translates
``example(Id, Pairs, Target)`` into ``example(Id, Target, Pairs)`` and
excludes only unbound targets. It preserves omitted features and unbound
feature values. The ``target/1`` predicate returns the fixed atom
``target`` as named metadata, not as a declaration of a candidate
feature or of an objective. The adapter uses object parameters directly
and does not create dynamic objects.

Rows with an unbound target are excluded before fitting encoders or
computing scaling statistics. Known non-numeric targets raise
``type_error(number, Target)``. An empty dataset or one with no known
targets raises ``domain_error(non_empty_examples, Dataset)``.
Unsupported declarations raise
``domain_error(feature_type, Feature-Domain)``. Feature values are
checked by the regression learner; missing values remain unbound.

Scoring and options
-------------------

Each original feature receives the maximum absolute coefficient in its
encoder block. Continuous encoders contribute two columns: the scaled
value and the missing-value indicator. Categorical encoders contribute
exactly ``length(Values)`` columns: all categories after the first
declared baseline, followed by the missing-value indicator. All columns
in each block contribute to its score. The intercept does not
contribute. These scores describe fitted encoded coefficients, not
raw-unit effects or causal importance; continuous scaling and
categorical reference levels affect their interpretation.

The ``learn/3`` predicate accepts these wrapper options:

- ``regressor_options(Options)`` delegates solver options to
  ``lasso_regression``. Its default is ``[]``; solver defaults and
  validation belong to the learner.
- ``coefficient_threshold(Cutoff)`` accepts a non-negative number and
  defaults to ``0.0``. A group is active only when its score is strictly
  greater than this cutoff.
- ``selection_strategy(Strategy)`` selects ``all`` (default),
  ``top_k(K)``, or ``threshold(T)``. The ``all`` strategy selects all
  active groups; ``top_k(K)`` requires a positive integer and selects at
  most ``K`` active groups. The ``threshold(T)`` strategy accepts a
  number and selects active groups whose scores are at least ``T``.
- ``regularization_search(Search)`` selects ``none`` (default) or a
  holdout search. The ``holdout(Fraction, Values)`` form searches a
  nonempty proper list of non-negative numeric penalties using
  validation MSE. The numeric fraction must be strictly between zero and
  one. Alternatively,
  ``holdout(Fraction, linear(Minimum, Maximum, Count))`` generates an
  ascending linear grid. Bounds must be numeric, with a non-negative
  minimum and a greater maximum; ``Count`` must be an integer of at
  least two.

Selection strategies operate only on active groups. Neither a large
``K`` nor a zero or negative strategy threshold reintroduces
zero-coefficient groups. All candidate scores are sorted numerically in
decreasing order using the shared sorting predicate, preserving encoder
declaration order for ties. Selected names follow score order, never
encoded-column indexes.

Repeated wrapper and solver options are accepted. Lookup uses the first
occurrence. The first stored ``regressor_options/1`` term contains the
complete effective options reported by the learner; later occurrences
are preserved. No solver defaults are copied into this implementation.

The first stored ``regularization_search/1`` option retains the
requested linear specification or explicit list; later occurrences are
preserved.

Regularization search
---------------------

The linear form generates ``Count`` floating-point penalties, including
both float-converted endpoints. Interior candidates use
``Minimum + (Maximum-Minimum)*Index/(Count-1)``. For example,
``linear(0, 1, 3)`` produces ``[0.0, 0.5, 1.0]``. Floating-point
rounding can produce repeated values, which are accepted with the
existing tie policy. Grid generation uses no data or randomness and
obeys backend arithmetic limits. Users still choose the bounds and
candidate count.

Search requires at least two usable numeric-target rows; otherwise it
raises ``domain_error(lasso_search_examples, Count)``. Rows with an
unbound target are removed before splitting. For N usable rows, the last
``min(N-1, max(1, ceiling(Fraction*N)))`` rows form the validation
suffix; the preceding rows form the training prefix. Row order is
preserved, and example identifiers are not used as split keys. There is
no randomization.

Each candidate is fitted using only the training prefix. Its encoders
and continuous scaling therefore exclude validation rows. Predictions on
the validation suffix determine mean squared error; an intercept-only
candidate uses its fitted bias. Exact MSE ties prefer stronger
regularization, and numerically equal penalties retain the first grid
occurrence. Integer penalties are converted to floats for the solver.

The candidate penalty replaces the first nested ``regularization/1``
option, or is inserted when absent. Other solver settings and later
repeated options are preserved and validated. Trial fitting and
prediction errors propagate; no candidates are silently discarded. The
winning penalty is refitted from scratch on all usable rows, so the
retained encoders, coefficients, and scores describe the full-data fit,
not a trial model.

The transient ``regression_examples_adapter(Declarations, Examples)``
object provides materialized subsets in the existing adapter file. It
receives the validated declarations and rows and creates no dynamic
objects. Trial data, trial models, and subset handles are not retained
in the selector.

Model and diagnostics
---------------------

The ``learn/2`` and ``learn/3`` predicates return models using the
following term representation:

::

   lasso_feature_selector(Regressor, AllSortedFeatureScores, SelectedOriginalFeatures, Diagnostics)

The retained ``Regressor`` is the complete
``lasso_regressor(Encoders, Bias, Weights, RegressorDiagnostics)`` term.
The ``feature_scores/2`` predicate returns all original
``Feature-Score`` pairs, and the ``selected_features/2`` predicate
returns only selected original names.

The ``Diagnostics`` argument is a list of diagnostic terms. The
``diagnostics/2`` predicate returns this list; the ``diagnostic/2``
predicate enumerates its terms:

::

   [
       model(lasso_feature_selector),
       example_count(OriginalCount),
       options(EffectiveOptions),
       usable_example_count(UsableCount),
       excluded_example_count(ExcludedCount),
       candidate_count(CandidateCount),
       selected_count(SelectedCount),
       encoded_feature_count(EncodedCount),
       aggregation(max_abs),
       maximum_absolute_coefficient(Maximum),
       regressor_diagnostics(RegressorDiagnostics),
       regularization_search_result(SearchResult)
   ]

The original count includes rows with unbound targets. The usable and
excluded counts partition that count. The nested diagnostics are
retained unchanged, including convergence status, completed iterations,
final delta, encoded count, and effective solver options. The
``selector_options/2`` predicate returns the effective wrapper options.
Exhausting the solver iteration limit does not discard the fitted
regressor or conceal its stop status.

The ``regularization_search_result/1`` diagnostic value is ``none`` when
search is disabled. When enabled, it is the following term, where counts
refer to the usable-row split:

::

   holdout(TrainingCount, ValidationCount, Trials, SelectedPenalty)

The trials follow grid order and contain these summaries:

::

   trial(Penalty, ValidationMSE, Convergence, Iterations, FinalDelta)

Convergence and iteration exhaustion are reported for every trial as
well as the final fit. Search does not imply that these fits converged
or that the supplied finite grid contains the optimal continuous
penalty.

The ``check_selector/1`` predicate requires a ground model with the
correct name, validates the nested regressor through its existing API,
and checks encoder names, coefficient lengths, effective options, and
metadata counts. It recomputes every group score and the selected set
from the coefficients. It also checks split counts, grid/trial alignment
(reconstructing generated values), non-negative MSE values, convergence
summaries, the winning penalty and tie policy, and agreement with the
final solver options. Without training data it cannot recompute the
recorded validation errors. A nonground selector raises
``instantiation_error``; a malformed ground selector raises
``domain_error(selector, Selector)`` without modifying it. The
``valid_selector/1`` predicate fails for invalid models without
throwing.

The ``export_to_clauses/4`` predicate exports the complete selector as
``Functor(Selector)``. The inherited ``export_to_file/4`` predicate
writes these clauses with dataset and diagnostics metadata. The
``print_selector/1`` predicate prints the scores, selection,
diagnostics, and retained regressor.

Example
-------

For a numeric-target feature dataset object named ``dataset``, the
following goals learn and inspect a selector:

::

   lasso_feature_selector::learn(dataset, Selector, [
       regressor_options([regularization(0.05), feature_scaling(false)]),
       coefficient_threshold(0.0),
       selection_strategy(top_k(5))
   ]),
   lasso_feature_selector::feature_scores(Selector, Scores),
   lasso_feature_selector::selected_features(Selector, Features),
   lasso_feature_selector::diagnostics(Selector, Diagnostics).

Limitations
-----------

Selection is limited to numeric-target linear regression.
Classification, automatic data-dependent penalty bounds,
cross-validation, and group-Lasso penalties are not provided. Holdout
search depends on row order, fraction, and the supplied finite grid; a
single validation suffix can give a noisy estimate. Users must choose an
ordering suitable for their task, especially for time-dependent data.

Maximum absolute coefficient aggregation is a reporting heuristic, not a
group penalty or causal importance measure. Scores depend on feature
scaling and categorical reference levels. Correlated features can yield
unstable selections; redundant features are not guaranteed to be
removed. A selected feature may owe its importance to its missing-value
indicator rather than its observed values.

Coefficient cutoffs still require task-specific validation. Models
returned after the iteration limit may not have converged; inspect the
retained convergence diagnostics before interpreting their selections.

References
----------

- Tibshirani, R. (1996). Regression Shrinkage and Selection via the
  Lasso. *Journal of the Royal Statistical Society: Series B*, 58(1),
  267-288. https://doi.org/10.1111/j.2517-6161.1996.tb02080.x
