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


`ripple_down_rules`
===================

This library provides persistent implementations of Ripple-Down Rules
(RDRs). A correction returns a new model and leaves the original model
unchanged. Conditions and conclusions are qualified callable terms,
allowing applications to represent cases and conclusions using arbitrary
terms.

The library distinguishes three RDR semantics instead of providing nominal
"optimized" duplicates:

- Single Classification Ripple-Down Rules (SCRDR) follows refinement and
  alternative links and returns one conclusion.
- Multi Classification Ripple-Down Rules (MCRDR) accumulates conclusions and
  supports selective stop and filter refinements.
- Generalized Ripple-Down Rules (GRDR) composes keyed SCRDR or MCRDR models
  and repeats inference until the keyed conclusions reach a fixed point.

Models are opaque ground terms. Always use the protocol predicates to
construct, revise, classify, validate, and inspect them.


API documentation
-----------------

Open the [../../apis/library_index.html#ripple_down_rules](../../apis/library_index.html#ripple_down_rules)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ripple_down_rules(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file for the `types`
library:

	| ?- logtalk_load(ripple_down_rules(tester)).


Protocol implementations
------------------------

The `ripple_down_rules_common_protocol` and `ripple_down_rules_protocol`
protocols are implemented by three objects:

- `single_classification_ripple_down_rules` implements Single Classification
    Ripple-Down Rules. Classification follows EXCEPT and ELSE links and returns
    the conclusion of the deepest applicable rule, or the configured default
    conclusion. Corrections use the `replace` operation.
- `multi_classification_ripple_down_rules` implements Multi Classification
    Ripple-Down Rules. Classification evaluates an ordered forest of rules and
    returns an ordered list of unique conclusions. Corrections use the `add`,
    `remove`, and `filter` operations.
- `generalized_ripple_down_rules` implements Generalized Ripple-Down Rules.
    It stores keyed `single_classification_ripple_down_rules` or
    `multi_classification_ripple_down_rules` submodels and evaluates them
    in standard key order until their keyed conclusions reach a fixed point.
    Its `revise/6` predicate takes `Key-Case` as the case argument and routes
    the correction atom to the keyed submodel.

All three implementations use persistent opaque models: revising or updating
a model returns a new model without changing the original one.

When using explicit message-sending to the implementations, you may want to
shorten the object names. For example (in yourclient object or category):

	:- uses([
		single_classification_ripple_down_rules as scrdr,
		multi_classification_ripple_down_rules as mcrdr,
		generalized_ripple_down_rules as grdr
	]).


Options
-------

The `new/1` predicate creates an empty model using the implementation default
options. The `new/2` predicate takes the model as its first argument and an
options list as its second argument:

- `single_classification_ripple_down_rules` accepts `default(Conclusion)`,
    which specifies the conclusion returned when no rule applies. The default
    value is `none`.
- `multi_classification_ripple_down_rules` accepts `default(Conclusions)`,
    where `Conclusions` is a proper list of initial conclusions. The default
    value is `[]`.
- `generalized_ripple_down_rules` accepts `maximum_cycles(Count)`, where
    `Count` is a positive integer limiting fixed-point inference. The default
    value is `16`.

For example:

	| ?- single_classification_ripple_down_rules::new(Model, [default(unknown)]).

	| ?- multi_classification_ripple_down_rules::new(Model, [default([unclassified])]).

	| ?- generalized_ripple_down_rules::new(Model, [maximum_cycles(32)]).

Unknown, malformed, or repeated options are handled by the standard `options`
library predicates.


Walkthroughs
------------

The walkthroughs below use the topic-named objects defined in `examples.lgt`:

	| ?- logtalk_load(ripple_down_rules(examples)).


### Classic SCRDR correction

The first walkthrough follows the classic tennis/squash worked example. A
root rule classifies sunny, cool weather as suitable for tennis. A storm case
also has windy wind and high humidity and is initially misclassified. The
correction adds a differentiating EXCEPT rule that concludes squash.

	| ?- tennis::tennis_models(Before, After),
	     tennis::squash_storm_case(Case),
	     single_classification_ripple_down_rules::classify(Before, Case, BeforeConclusion),
	     single_classification_ripple_down_rules::classify(After, Case, AfterConclusion).
	BeforeConclusion = tennis,
	AfterConclusion = squash.

The old model remains unchanged:

	| ?- tennis::tennis_models(Before, _),
	     tennis::squash_storm_case(Case),
	     single_classification_ripple_down_rules::classify(Before, Case, Conclusion).
	Conclusion = tennis.

Classification can include an ordered trace. The root fires but does not
contribute because its EXCEPT child supplies the final conclusion:

	| ?- tennis::tennis_models(_, After),
	     tennis::squash_storm_case(Case),
	     single_classification_ripple_down_rules::classify(After, Case, Conclusion, Trace).
	Conclusion = squash,
	Trace = [evaluated(1, true, false), evaluated(2, true, true)].

The local companion cornerstone used by the executable example is not part
of the published worked example. It exists to demonstrate the RDR exception
invariant: a correction condition must match the correction case but must not
also match the cornerstone of the rule being refined.


### Tennis MCRDR extension

The `tennis` object also provides a local MCRDR teaching extension. Independent
rules initially conclude both `tennis` and `kite_flying`; a stop refinement
selectively removes `tennis`, and a new top-level rule adds `squash`:

	| ?- tennis::mcrdr_models(Before, Stopped, After),
	     tennis::squash_storm_case(Case),
	     multi_classification_ripple_down_rules::classify(Before, Case, BeforeConclusions),
	     multi_classification_ripple_down_rules::classify(Stopped, Case, StoppedConclusions),
	     multi_classification_ripple_down_rules::classify(After, Case, AfterConclusions).
	BeforeConclusions = [tennis, kite_flying],
	StoppedConclusions = [kite_flying],
	AfterConclusions = [kite_flying, squash].


### Tennis GRDR extension

The GRDR teaching extension evaluates `equipment` before `sport`. The first
pass infers `sport-squash`; the second can then infer `equipment-[racket]`;
the third confirms the fixed point:

	| ?- tennis::grdr_model(Model),
	     tennis::squash_storm_case(Case),
	     generalized_ripple_down_rules::classify(Model, Case, Conclusions, Trace).
	Conclusions = [equipment-[racket], sport-squash],
	Trace = [pass(1, _), pass(2, _), pass(3, _)].


### UCI Zoo dataset

The `zoo` object contains one unchanged representative row from each of the
seven classes in Richard Forsyth's Zoo dataset. Every row retains all 16
features and its original numeric class label. An SCRDR model classifies the
seven rows, while a GRDR model composes species and habitat inference:

	| ?- zoo::scrdr_model(Model),
	     zoo::representative(pitviper, Case, Class),
	     single_classification_ripple_down_rules::classify(Model, Case, Species).
	Class = 3,
	Species = reptile.

	| ?- zoo::grdr_model(Model),
	     zoo::representative(bass, Case, 4),
	     generalized_ripple_down_rules::classify(Model, Case, Conclusions).
	Conclusions = [habitat-[aquatic], species-fish].

The rows are from Forsyth, R. (1990), *Zoo*, UCI Machine Learning
Repository, DOI `10.24432/C5R59V`, and are redistributed under CC BY 4.0.


### GRDR cycle detection

The `grdr_cycle` object provides a deliberately non-convergent two-key model.
Its three-cycle limit turns oscillation into a domain-specific error:

	| ?- grdr_cycle::model(Model), generalized_ripple_down_rules::classify(Model, [], _).
	ERROR: domain_error(grdr_non_convergence, maximum_cycles(3))


### Adapted research scenarios

Three compact examples are independently expressed adaptations of domains
exercised by the public Python Ripple-Down Rules project. They demonstrate
relational computed conclusions, simultaneous MCRDR labels, and structural
classification without copying its implementation code or presenting the
fixtures as historical canonical examples.

	| ?- robot_containment::model(Model),
	     robot_containment::case(wheel, Case),
	     single_classification_ripple_down_rules::classify(Model, Case, Conclusion).
	Conclusion = contained(wheel, chassis).

	| ?- furniture_recognition::model(Model),
	     furniture_recognition::scene(Case),
	     multi_classification_ripple_down_rules::classify(Model, Case, Conclusions).
	Conclusions = [drawer, cabinet].

	| ?- mutagenicity::model(Model),
	     mutagenicity::molecule(nitrobenzene, Case),
	     single_classification_ripple_down_rules::classify(Model, Case, Conclusion).
	Conclusion = mutagenic.


### Provenance and credits

The tennis/squash walkthrough is independently expressed from the Ripple
Down Rules worked example described on Wikipedia, licensed under CC BY-SA
4.0. The companion cornerstone and later MCRDR and GRDR teaching extensions
are local examples and are labeled as such.

The Zoo rows are credited and licensed in the Zoo section above. The robot
containment, furniture recognition, and mutagenicity examples are compact local
adaptations of test domains found during research of the Python project.

The design also draws on the RDR literature by Paul Compton and Byeong Ho
Kang and on the public Python Ripple-Down Rules project. PEIRS/GARVAN-ES1 and
RDRPOSTagger are historical real-world applications; they are not treated as
fully specified acquisition fixtures here.
