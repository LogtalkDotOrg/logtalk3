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


`univariate_distributions`
==========================

This library provides sampling, density, log-density, cumulative distribution,
and quantile predicates for normal, Student's t, chi-squared, gamma, beta,
exponential, and Fisher-Snedecor distributions.


API documentation
-----------------

Open the
[../../apis/library_index.html#univariate-distributions](../../apis/library_index.html#univariate-distributions)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(univariate_distributions(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(univariate_distributions(tester)).


Scope
-----

The `univariate_distributions(Random)` parametric object accepts a random
source implementing the `sampling_protocol` protocol. Common choices are
`random(xoshiro128pp)` and `fast_random(xoshiro128pp)`.

Sampling predicates accept a zero standard deviation, producing the mean as a
degenerate sample. Density, log-density, cumulative distribution, and quantile
predicates require a positive standard deviation.


Examples
--------

Generate one normal sample and a batch of five samples:

	| ?- univariate_distributions(fast_random)::normal(0.0, 1.0, Sample).

	| ?- univariate_distributions(fast_random)::normal_samples(5, 0.0, 1.0, Samples).

Compute the standard normal density, cumulative probability, and quantile:

	| ?- univariate_distributions(fast_random)::standard_normal_density(0.0, Density).
	Density = 0.3989422804014327.

	| ?- univariate_distributions(fast_random)::standard_normal_distribution(0.0, Probability).
	Probability = 0.5000000005248086.

	| ?- univariate_distributions(fast_random)::standard_normal_quantile(0.975, Quantile).
	Quantile = 1.959963986120195.
