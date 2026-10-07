________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`lasso_feature_selector`
========================

This library selects original features using the coefficients learned by
`lasso_regression`. It implements `feature_selector_protocol` through
`feature_selector_common`. Training delegates to the existing regression
learner without changing its optimizer, objective, encoding, or convergence
controls. Classification targets are not accepted.


API documentation
-----------------

Open the [../../apis/library_index.html#lasso-feature-selector](../../apis/library_index.html#lasso-feature-selector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(lasso_feature_selector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(lasso_feature_selector(tester)).


Dataset and adapter
-------------------

The `learn/2` and `learn/3` predicates accept a dataset implementing
`feature_dataset_protocol`. Shared validation checks all original examples,
including rows with unknown targets, for duplicate declarations, undeclared
or repeated feature bindings, proper feature lists, and consistent example
counts. The selector checks that every remaining target is numeric and that
at least one usable example remains.

The parametric `regression_dataset_adapter(Dataset)` object implements
`regression_dataset_protocol`. The `attribute_values/2` predicate forwards
the declarations after checking that names are distinct atoms and domains
are `continuous` or non-empty proper lists of distinct atomic categorical
values. The atom-name restriction follows the regression learner contract.
The `example/3` predicate translates `example(Id, Pairs, Target)` into
`example(Id, Target, Pairs)` and excludes only unbound targets. It preserves
omitted features and unbound feature values. The `target/1` predicate returns
the fixed atom `target` as named metadata, not as a declaration of a candidate
feature or of an objective. The adapter uses object parameters directly and
does not create dynamic objects.

Unknown targets are excluded before fitting encoders or computing scaling
statistics. Known non-numeric targets raise `type_error(number, Target)`.
An empty dataset or one with no known targets raises
`domain_error(non_empty_examples, Dataset)`. Unsupported declarations raise
`domain_error(feature_type, Feature-Domain)`. Feature values are checked by
the regression learner; missing values remain unbound.


Scoring and options
-------------------

Each original feature receives the maximum absolute coefficient in its
encoder block. Continuous encoders contribute two columns: the scaled value
and the missing-value indicator. Categorical encoders contribute exactly
`length(Values)` columns: all categories after the first declared baseline,
followed by the missing-value indicator. All columns in each block contribute
to its score. The intercept does not contribute. These scores describe fitted
encoded coefficients, not raw-unit effects or causal importance; continuous
scaling and categorical reference levels affect their interpretation.

The `learn/3` predicate accepts these wrapper options:

- `regressor_options(Options)` delegates solver options to `lasso_regression`.
  Its default is `[]`; solver defaults and validation belong to the learner.
- `coefficient_threshold(Cutoff)` accepts a non-negative number and defaults
  to `0.0`. A group is active only when its score is strictly greater than
  this cutoff.
- `selection_strategy(all)` selects all active groups and is the default.
  `selection_strategy(top_k(K))` accepts a positive integer and selects at most
  `K` active groups. `selection_strategy(threshold(T))` accepts a number and
  selects active groups whose scores are at least `T`.

Selection strategies operate only on active groups. Neither a large `K` nor
a zero or negative strategy threshold reintroduces zero-coefficient groups.
All candidate scores are sorted numerically in decreasing order using the
shared sorting predicate, preserving encoder declaration order for ties.
Selected names follow score order, never encoded-column indexes.

Repeated wrapper and solver options are accepted. Lookup uses the first
occurrence. The first stored `regressor_options/1` term contains the complete
effective options reported by the learner; later occurrences are preserved.
No solver defaults are copied into this implementation.


Model and diagnostics
---------------------

The `learn/2` and `learn/3` predicates return the following selector term:

	lasso_feature_selector(Regressor, AllSortedFeatureScores, SelectedOriginalFeatures, Diagnostics)

The retained `Regressor` is the complete
`lasso_regressor(Encoders, Bias, Weights, RegressorDiagnostics)` term. The
`feature_scores/2` predicate returns all original `Feature-Score` pairs, and
the `selected_features/2` predicate returns only selected original names.

The `diagnostics/2` predicate returns the following metadata list; the
`diagnostic/2` predicate enumerates its terms:

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
	    regressor_diagnostics(RegressorDiagnostics)
	]

The original count includes unknown-target rows. The usable and excluded
counts partition that count. The nested diagnostics are retained unchanged,
including convergence status, completed iterations, final delta, encoded
count, and effective solver options. The `selector_options/2` predicate
returns the effective wrapper options. Exhausting the solver iteration limit
does not discard the fitted regressor or conceal its stop status.

The `check_selector/1` predicate requires a ground model with the correct
name, validates the nested regressor through its existing API, and checks
encoder names, coefficient lengths, effective options, and metadata counts.
It recomputes every group score and the selected set from the coefficients.
A variable selector raises `instantiation_error`; a partial or malformed
selector raises `domain_error(selector, Selector)` without modifying it.
The `valid_selector/1` predicate fails for invalid models without throwing.

The `export_to_clauses/4` predicate exports the complete selector as
`Functor(Selector)`. The inherited `export_to_file/4` predicate writes these
clauses with dataset and diagnostics metadata. The `print_selector/1`
predicate prints the scores, selection, diagnostics, and retained regressor.


Example
-------

For a numeric-target feature dataset object named `dataset`, the following
goals learn and inspect a selector:

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

Selection is limited to numeric-target linear regression. Classification,
automatic regularization search, and group-Lasso penalties are not provided.

Maximum absolute coefficient aggregation is a reporting heuristic, not a
group penalty or causal importance measure. Scores depend on feature scaling
and categorical reference levels. Correlated features can yield unstable
selections; redundant features are not guaranteed to be removed. A selected
feature may owe its importance to its missing-value indicator rather than
its observed values.

Regularization and coefficient cutoffs require task-specific validation.
Models returned after the iteration limit may not have converged; inspect
the retained convergence diagnostics before interpreting their selections.
