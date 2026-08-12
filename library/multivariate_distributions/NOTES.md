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


`multivariate_distributions`
============================

This library provides sampling, density, and log-density predicates for
multivariate normal and Student's t distributions, Mahalanobis distance
predicates, additive-log-ratio logistic-normal sampling predicates, Dirichlet
sampling, density, and log-density predicates, and multinomial sampling,
probability mass, and log-probability mass predicates. Batch predicates return
samples as lists of row vectors.


API documentation
-----------------

Open the
[../../apis/library_index.html#multivariate-distributions](../../apis/library_index.html#multivariate-distributions)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(multivariate_distributions(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(multivariate_distributions(tester)).


Scope
-----

The `multivariate_distributions(Random)` parametric object accepts a random
source implementing the `sampling_protocol` protocol. Common choices are
`random(xoshiro128pp)` and `fast_random(xoshiro128pp)`.

Covariance and scale matrices may be singular positive-semidefinite matrices.
The predicates use eigendecomposition to operate on their affine support and
do not add diagonal jitter. Density predicates return `0.0` outside singular
support and log-density predicates return `negative_infinity`. Mahalanobis
distance predicates throw an error for points outside support.

For the multivariate Student's t distribution, the matrix argument is a scale
matrix. When the degrees of freedom are greater than two, the covariance is
the scale matrix multiplied by `DegreesOfFreedom / (DegreesOfFreedom - 2)`.

The logistic-normal predicates use the additive log-ratio representation. A
latent vector of length d maps to a simplex vector of length d+1, with the last
simplex component used as the fixed reference component.

Dirichlet densities support simplex boundary points. Depending on the alpha
parameters, boundary density and log-density values may be represented by the
atoms `positive_infinity` and `negative_infinity`. When zero components have
both alphas smaller than one and alphas greater than one, the boundary limit is
path-dependent and both predicates return `undefined`.

Multinomial probability lists must be nonempty and sum to one within a
tolerance of `1.0e-12`. Multinomial quantiles order count vectors by decreasing
probability mass and use increasing lexicographic order to break log-probability
ties within a relative tolerance of `1.0e-12`.

Multinomial sampling performs one linear category selection and one count-vector
update per trial, requiring time proportional to the product of the trial and
category counts. Multinomial quantiles enumerate and sort the complete
count-vector state space. For `Trials` trials and `Categories` categories, this
space contains `binomial(Trials + Categories - 1, Categories - 1)` vectors; the
implementation therefore requires combinatorial time and memory and rejects
queries exceeding 100000 vectors. Inputs near that limit can still require
substantial runtime and memory.

Normal, Student's t, and logistic-normal sampling requires an eigendecomposition
of the covariance or scale matrix. Batch sampling predicates factorize the
matrix once and should be preferred when generating multiple samples.


Examples
--------

Generate one multivariate normal sample using a portable pseudo-random source:

	| ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal(
	         [0.0, 0.0], [[1.0, 0.5], [0.5, 1.0]], Sample
	     ).

Generate a batch of five samples, factorizing the covariance matrix only once:

	| ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_samples(
	         5, [0.0, 0.0], [[1.0, 0.5], [0.5, 1.0]], Samples
	     ).

Compute a density and a Mahalanobis distance (these predicates do not consume
random values):

	| ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_density(
	         [1.0, 0.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Density
	     ).
	Density = 0.09653235263005393.

	| ?- multivariate_distributions(random(xoshiro128pp))::mahalanobis_distance(
	         [2.0, 0.0], [0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], Distance
	     ).
	Distance = 1.0.

For a singular covariance matrix, density is evaluated on its affine support
and is zero outside it:

	| ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_density(
	         [1.0, 1.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], Density
	     ).
	Density = 0.0.

Generate a multivariate Student's t sample with five degrees of freedom:

	| ?- multivariate_distributions(random(xoshiro128pp))::multivariate_t(
	         5.0, [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Sample
	     ).

Generate a logistic-normal probability vector. A two-dimensional latent
normal distribution produces three simplex components:

	| ?- multivariate_distributions(random(xoshiro128pp))::logistic_normal(
	         [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Probabilities
	     ).
